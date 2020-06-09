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
    Float.declare("mk", ("sign", BoolSort()), 
                 ("mantissa", BitVecSort(mantissa_size)), 
                 ("exponent", BitVecSort(exponent_size)))
    return Float.create()

def sizes(sort : DatatypeSortRef):
    c = sort.constructor(0)
    return (c.domain(1).size(), c.domain(2).size())

def Float(name : str, mantissa_size : int, exponent_size : int):
    return Const(name, FloatSort(mantissa_size, exponent_size))

def FloatVal(sign : bool, mantissa : int, exponent : int, sort : DatatypeSortRef):
    return sort.mk(BoolVal(sign), BitVecVal(mantissa), BitVecVal(exponent))

def FloatValDec(dec_val : str, rounding_mode : converter.RoundingMode, sort : DatatypeSortRef):
    mantissa_size, exponent_size = sizes(sort)
    f = converter.convert(dec_val, rounding_mode, mantissa_size, exponent_size)
    assert(len(f.s) == 1)
    assert(len(f.m) == mantissa_size)
    assert(len(f.e) == exponent_size)
    # this is a suboptimal conversion 
    # (converter converts ints to binary strings, here we convert strings back to int)
    # but we can get around to fixing this at some later point in the decoder
    return FloatVal(f.s == "1", int(f.m, 2), int(f.e, 2), sort)