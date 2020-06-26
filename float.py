from z3 import *
import converter

RoundingMode, (NearestTieToEven, NearestTieAwayFromZero, Up, Down, Truncate) = EnumSort("RoundingMode", [
    "NearestTieToEven", 
    "NearestTieAwayFromZero", 
    "Up", 
    "Down", 
    "Truncate"])

# Creates a floating point sort, the constructor(sign, mantissa, exponent) and accessors for sign, mantissa and exponent.
def FloatSort(mantissa_size : int, exponent_size : int) -> DatatypeSortRef: 
    # we're using Datatypes instead of tuples because Datatypes encapsulate constructors and accessors nicely
    Float = Datatype("Float(%d, %d)" % (mantissa_size, exponent_size))
    Float.declare("mk", ("sign", BitVecSort(1)), 
                 ("mantissa", BitVecSort(mantissa_size)), 
                 ("exponent", BitVecSort(exponent_size)))
    return Float.create()

def sizes(sort : DatatypeSortRef) -> (int, int):
    c = sort.constructor(0)
    return (c.domain(1).size(), c.domain(2).size())

def Float(name : str, mantissa_size : int, exponent_size : int) -> DatatypeRef:
    return Const(name, FloatSort(mantissa_size, exponent_size))

def FloatVal(sign : int, mantissa : int, exponent : int, sort : DatatypeSortRef) -> DatatypeRef:
    m, e = sizes(sort)
    return sort.mk(BitVecVal(sign, 1), BitVecVal(mantissa, m), BitVecVal(exponent, e))

def FloatValDec(dec_val : str, rounding_mode : converter.RoundingMode, sort : DatatypeSortRef) -> DatatypeRef:
    m, e = sizes(sort)
    f = converter.convert(dec_val, rounding_mode, m, e)
    # this is a suboptimal conversion 
    # (converter converts ints to binary strings, here we convert strings back to int)
    # but we can get around to fixing this at some later point in the decoder
    return FloatVal(int(f.s), int(f.m, 2), int(f.e, 2), sort)

def FloatValBV(bv : BitVecNumRef, sort : DatatypeSortRef) -> DatatypeRef:
    m, e = sizes(sort)
    val = bv.as_long()
    mantissa = val & (2**m - 1)
    exponent = val & ((2**e - 1) << m)
    sign = val & (1 << (m + e))
    return FloatVal(sign, mantissa, exponent, sort)

def FloatValPosInf(sort : DatatypeSortRef) -> DatatypeRef:
    m, e = sizes(sort)
    return FloatVal(0, 0, 2**e - 1)

def FloatValNegInf(sort : DatatypeSortRef) -> DatatypeRef:
    m, e = sizes(sort)
    return FloatVal(1, 0, 2**e - 1)

def FloatValNaN(sort : DatatypeSortRef, value = 1) -> DatatypeRef:
    if value == 0:
        raise ValueError("NaN value cannot be zero")
    m, e = sizes(sort)
    sign = int(value >= 0)
    return FloatVal(sign, value, 2**e - 1)

def get_sort(a : Float) -> DatatypeSortRef:
    m, e = sizes(a.sort())
    return FloatSort(m, e)

def to_ieee_bv(a : DatatypeRef) -> BitVecNumRef:
    s = get_sort(a)
    return Concat(s.sign(a), s.exponent(a), s.mantissa(a))

def ensure_eq_sort(a : DatatypeRef, b : DatatypeRef):
    a_m, a_e = sizes(get_sort(a))
    b_m, b_e = sizes(get_sort(b))
    if a_m != b_m or a_e != b_e:
        raise ValueError("cannot compute operation on two floats with different sorts")

def eq_bitwise(a : DatatypeRef, b : DatatypeRef) -> BoolRef:
    ensure_eq_sort(a, b)
    return to_ieee_bv(a) == to_ieee_bv(b)

# Checks whether a is +0
def is_pos_zero(a : DatatypeRef) -> BoolRef:
    return eq_bitwise(a, FloatVal(0, 0, 0, get_sort(a)))

# Checks whether a is -0
def is_neg_zero(a : DatatypeRef) -> BoolRef:
    return eq_bitwise(a, FloatVal(1, 0, 0, get_sort(a)))

# Checks whether a is +0 or -0
def is_zero(val : DatatypeRef) -> BoolRef:
    return Or(is_pos_zero(val), is_neg_zero(val))

# Checks whether a is +inf or -inf
def is_inf(a : DatatypeRef) -> BoolRef:
    s = get_sort(a)
    m, e = sizes(s)
    return And(s.mantissa(a) == BitVecVal(0, m), s.exponent(a) == BitVecVal(2**e - 1, e))

# Checks whether a is +inf
def is_pos_inf(a : DatatypeRef) -> BoolRef:
    s = get_sort(a)
    return And(is_inf(a), s.sign(a) == 0)

# Checks whether a is +inf
def is_neg_inf(a : DatatypeRef) -> BoolRef: 
    s = get_sort(a)
    return And(is_inf(a), s.sign(a) == 1)

# Checks whether a is a NaN value (and optionally whether it is a specific NaN value)
def is_nan(a : DatatypeRef, nan_value : int = 0) -> BoolRef: 
    # nan_value 0 is used to signalize that no specific nan value is required
    s = get_sort(a)
    m, e = sizes(s)
    mantissa = s.mantissa(a)
    exponent = s.exponent(a)
    inf_exp = BitVecVal(2**e-1, e)
    if nan_value == 0:
        return And(Not(mantissa == BitVecVal(0, m)), exponent == inf_exp) # exclude zero mantissa so we don't accidentally declare infs as nans
    else:
        return And(mantissa == BitVecVal(nan_value, m), exponent == inf_exp)

# Checks whether a is a subnormal float
def is_subnormal(a : DatatypeRef) -> BoolRef:
    s = get_sort(a)
    m, e = sizes(s)
    return And(Not(s.mantissa(a) == BitVecVal(0, m)), s.exponent(a) == BitVecVal(0, e))

# Checks whether a is a normal float
def is_normal(a : DatatypeRef) -> BoolRef:
    return And(Not(is_inf(a)), 
               Not(is_zero(a)), 
               Not(is_subnormal(a)), 
               Not(is_nan(a)))

# Checks whether a is equal to b:
# If either is NaN, the two are unequal.
# If both are zero (either pos. or neg. zero), they are equal.
# Otherwise, they are equal iff their bitvector representations are equal.
def eq(a : DatatypeRef, b : DatatypeRef) -> BoolRef:
    ensure_eq_sort(a, b)
    return And(Not(Or(is_nan(a), is_nan(b))), 
               Or(And(is_zero(a), is_zero(a)), 
                  eq_bitwise(a, b)))

# Checks whether a is greater than b.
# If either is NaN or both are zero, a is not greater than b.
def gt(a : DatatypeRef, b : DatatypeRef) -> BoolRef:
    ensure_eq_sort(a, b)
    s = get_sort(a)
    abs_gt = UGT(Concat(s.exponent(a), s.mantissa(a)), Concat(s.exponent(b), s.mantissa(b)))
    abs_lt = ULT(Concat(s.exponent(a), s.mantissa(a)), Concat(s.exponent(b), s.mantissa(b)))
    return And(Not(Or(is_nan(a), is_nan(b))),
               Not(And(is_zero(a), is_zero(b))),
               Or(And(s.sign(a) == 0, s.sign(b) == 1), 
                  And(s.sign(a) == 0, s.sign(b) == 0, abs_gt),
                  And(s.sign(a) == 1, s.sign(b) == 1, abs_lt)))

# Checks whether a is less than b.
# If either is NaN or both are zero, a is not less than b.
def lt(a : DatatypeRef, b : DatatypeRef) -> BoolRef:
    return gt(b, a)

# Checks whether a is greater than or equal to b.
# If either is NaN, a is not greater than or equal to b.
def gte(a : DatatypeRef, b : DatatypeRef) -> BoolRef:
    return Or(gt(a, b), eq(a, b))

# Checks whether a is less than or equal to b.
# If either is NaN, a is not less than or equal to b.
def lte(a : DatatypeRef, b : DatatypeRef) -> BoolRef:
    return gte(b, a)