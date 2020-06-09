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

def Float(name : str, mantissa_size : int, exponent_size : int):
    return Const(name, FloatSort(mantissa_size, exponent_size))
