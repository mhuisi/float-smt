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
    return sort.mk(BitVecVal(sign), BitVecVal(mantissa), BitVecVal(exponent))

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
    s = val.sort()
    return Concat(s.sign(val), s.exponent(val), s.mantissa(val))