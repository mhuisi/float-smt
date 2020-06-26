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

def sizes(sort : DatatypeSortRef):
    c = sort.constructor(0)
    return (c.domain(1).size(), c.domain(2).size())

def Float(name : str, mantissa_size : int, exponent_size : int):
    return Const(name, FloatSort(mantissa_size, exponent_size))

def FloatVal(sign : int, mantissa : int, exponent : int, sort : DatatypeSortRef):
    m, e = sizes(sort)
    return sort.mk(BitVecVal(sign, 1), BitVecVal(mantissa, m), BitVecVal(exponent, e))

def FloatValDec(dec_val : str, rounding_mode : converter.RoundingMode, sort : DatatypeSortRef):
    m, e = sizes(sort)
    f = converter.convert(dec_val, rounding_mode, m, e)
    # this is a suboptimal conversion 
    # (converter converts ints to binary strings, here we convert strings back to int)
    # but we can get around to fixing this at some later point in the decoder
    return FloatVal(int(f.s), int(f.m, 2), int(f.e, 2), sort)

def FloatValBV(bv : BitVecNumRef, sort : DatatypeSortRef):
    m, e = sizes(sort)
    val = bv.as_long()
    mantissa = val & (2**m - 1)
    exponent = val & ((2**e - 1) << m)
    sign = val & (1 << (m + e))
    return FloatVal(sign, mantissa, exponent, sort)

def FloatValPosInf(sort : DatatypeSortRef):
    m, e = sizes(sort)
    return FloatVal(0, 0, 2**e - 1)

def FloatValNegInf(sort : DatatypeSortRef):
    m, e = sizes(sort)
    return FloatVal(1, 0, 2**e - 1)

def FloatValNaN(sort : DatatypeSortRef, value=1):
    if value == 0:
        raise ValueError("NaN value cannot be zero")
    m, e = sizes(sort)
    sign = int(value >= 0)
    return FloatVal(sign, value, 2**e - 1)

def get_sort(a : Float) -> DatatypeSortRef:
    m, e = sizes(a.sort())
    return FloatSort(m, e)

def to_ieee_bv(a : DatatypeRef):
    s = get_sort(a)
    return Concat(s.sign(a), s.exponent(a), s.mantissa(a))

def eq_bitwise(a: Float, b: Float) -> BoolRef:
    return to_ieee_bv(a) == to_ieee_bv(b)

# Checks whether a is +0
def is_pos_zero(a : Float) -> BoolRef:
    return eq_bitwise(a, FloatVal(0, 0, 0, get_sort(a)))

# Checks whether a is -0
def is_neg_zero(a : Float) -> BoolRef:
    return eq_bitwise(a, FloatVal(1, 0, 0, get_sort(a)))

# Checks whether a is +0 or -0
def is_zero(val : Float) -> BoolRef:
    return Or(is_pos_zero(val), is_neg_zero(val))

# Checks whether a is +inf or -inf
def is_inf(a : Float) -> BoolRef:
    s = get_sort(a)
    m, e = sizes(s)
    mantissa = s.mantissa(a)
    exponent = s.exponent(a)
    return And(mantissa == BitVecVal(0, m), exponent == BitVecVal(2**e - 1, e))

# Checks whether a is +inf
def is_pos_inf(a : Float) -> BoolRef:
    s = get_sort(a)
    return And(is_inf(a), s.sign(a) == 0)

# Checks whether a is +inf
def is_neg_inf(a : Float) -> BoolRef: 
    s = get_sort(a)
    return And(is_inf(a), s.sign(a) == 1)

# Checks whether a is a NaN value (and optionally whether it is a specific NaN value)
def is_nan(a : Float, nan_value : int = 0) -> BoolRef: 
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
def is_subnormal(a : Float) -> BoolRef:
    s = get_sort(a)
    m, e = sizes(s)
    mantissa = s.mantissa(a)
    exponent = s.exponent(a)
    return And(Not(mantissa == BitVecVal(0, m)), exponent == BitVecVal(0, e))

# Checks whether a is a normal float
def is_normal(a : Float) -> BoolRef:
    return And(Not(is_inf(a)), 
               Not(is_zero(a)), 
               Not(is_subnormal(a)), 
               Not(is_nan(a)))