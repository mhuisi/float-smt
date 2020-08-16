from z3 import *

RoundingMode, (NearestTiesToEven, NearestTiesAwayFromZero, Up, Down, Truncate) = EnumSort("RoundingMode", [
    "NearestTiesToEven", 
    "NearestTiesAwayFromZero", 
    "Up", 
    "Down", 
    "Truncate"])

def FloatSort(mantissa_size : int, exponent_size : int) -> DatatypeSortRef: 
    '''
    Creates a floating point sort, the constructor(sign, mantissa, exponent) and accessors for sign, mantissa and exponent.
    '''
    # we're using Datatypes instead of tuples because Datatypes encapsulate constructors and accessors nicely
    Float = Datatype("Float(%d, %d)" % (mantissa_size, exponent_size))
    Float.declare("mk", ("sign", BitVecSort(1)), 
                 ("mantissa", BitVecSort(mantissa_size)), 
                 ("exponent", BitVecSort(exponent_size)))
    return Float.create()

def sizes(sort : DatatypeSortRef) -> (int, int):
    c = sort.constructor(0)
    return (c.domain(1).size(), c.domain(2).size())

def get_sort(a : DatatypeRef) -> DatatypeSortRef:
    m, e = sizes(a.sort())
    return FloatSort(m, e)

def ensure_eq_sort(a : DatatypeRef, b : DatatypeRef):
    a_m, a_e = sizes(get_sort(a))
    b_m, b_e = sizes(get_sort(b))
    if a_m != b_m or a_e != b_e:
        raise ValueError("cannot compute operation on two floats with different sorts")