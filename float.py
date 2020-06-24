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
    return sort.mk(BitVecVal(sign, 1), BitVecVal(mantissa, sizes(sort)[0]), BitVecVal(exponent, sizes(sort)[1]))

def FloatValDec(dec_val : str, rounding_mode : converter.RoundingMode, sort : DatatypeSortRef):
    mantissa_size, exponent_size = sizes(sort)
    f = converter.convert(dec_val, rounding_mode, mantissa_size, exponent_size)
    assert(len(f.s) == 1)
    assert(len(f.m) == mantissa_size)
    assert(len(f.e) == exponent_size)
    # this is a suboptimal conversion 
    # (converter converts ints to binary strings, here we convert strings back to int)
    # but we can get around to fixing this at some later point in the decoder
    return FloatVal(int(f.s), int(f.m, 2), int(f.e, 2), sort)

def FloatValBV(bv : BitVecNumRef, sort : DatatypeSortRef):
    mantissa_size, exponent_size = sizes(sort)
    val = bv.as_long()
    mantissa = val & (2**mantissa_size - 1)
    exponent = val & ((2**exponent_size - 1) << mantissa_size)
    sign = val & (1 << (mantissa_size + exponent_size))
    return FloatVal(sign, mantissa, exponent, sort)

def FloatValPosInf(sort : DatatypeSortRef):
    mantissa_size, exponent_size = sizes(sort)
    return FloatVal(0, 0, 2**exponent_size - 1)

def FloatValNegInf(sort : DatatypeSortRef):
    mantissa_size, exponent_size = sizes(sort)
    return FloatVal(1, 0, 2**exponent_size - 1)

def FloatValNaN(sort : DatatypeSortRef, value=1):
    if value == 0:
        raise ValueError("NaN value cannot be zero")
    sign = int(value >= 0)
    mantissa_size, exponent_size = sizes(sort)
    return FloatVal(sign, value, 2**exponent_size - 1)

def to_ieee_bv(val : DatatypeRef):
    s = get_sort(val)
    return Concat(s.sign(val), s.exponent(val), s.mantissa(val))

def eq_bitwise(val1: Float, val2: Float) -> BoolRef:
    v1, v2 = to_ieee_bv(val1), to_ieee_bv(val2)
    return v1 == v2

def get_sort(val: Float) -> DatatypeSortRef:
    m,e = sizes(val.sort())
    return FloatSort(m,e)

# Checks whether a is +0 or -0
def is_zero(val : Float) -> BoolRef:
    return Or(is_pos_zero(val), is_neg_zero(val))

# Checks whether a is +0
def is_pos_zero(a : Float) -> BoolRef:
    pos_zero = FloatVal(0, 0, 0, get_sort(a))
    return eq_bitwise(a, pos_zero)

# Checks whether a is -0
def is_neg_zero(a : Float) -> BoolRef:
    neg_zero = FloatVal(1, 0, 0, get_sort(a))
    return eq_bitwise(a, neg_zero)



# Checks whether a is +inf or -inf
def is_inf(a : Float) -> BoolRef:
    s = get_sort(a)
    mantissa = s.mantissa(a)
    exponent = s.exponent(a)
    m,e = sizes(s)
    zero_mantissa = BitVecVal(0, m)
    inf_exp = BitVecVal(2**e-1, e)
    return And((mantissa == zero_mantissa),(exponent == inf_exp))


# Checks whether a is +inf
def is_pos_inf(a : Float) -> BoolRef:
    s = get_sort(a)
    sign = s.sign(a)
    return And(is_inf(a), sign == 0)

# Checks whether a is +inf
def is_neg_inf(a : Float) -> BoolRef: 
    s = get_sort(a)
    sign = s.sign(a)
    return And(is_inf(a), sign == 1)


# Checks whether a is a NaN value (and optionally whether it is a specific NaN value)
def is_nan(a : Float, nan_value : int = 0) -> BoolRef: 
    #nan_value 0 is used to signalize that no specific nan value is required
    s = get_sort(a)
    mantissa = s.mantissa(a)
    exponent = s.exponent(a)
    m,e = sizes(s)
    nan_mantissa = BitVecVal(nan_value, m)
    inf_exp = BitVecVal(2**e-1, e)
    if nan_value == 0:
        return And(Not(mantissa == nan_mantissa),(exponent == inf_exp)) #Not because this would be a zero mantissa which would be an inf value instead of a nan value
    else:
        return And((mantissa == nan_mantissa),(exponent == inf_exp))


# Checks whether a is a subnormal float
def is_subnormal(a : Float) -> BoolRef:
    #This implementation treats zeroes as subnormals
    s = get_sort(a)
    exponent = s.exponent(a)
    m,e = sizes(s)
    zero_exp = BitVecVal(0, e)
    return And((exponent == zero_exp))

# Checks whether a is a normal float
def is_normal(a : Float) -> BoolRef:
    return And(Not(is_inf(a)), Not(is_zero(a)), Not(is_subnormal(a)), Not(is_nan(a)))