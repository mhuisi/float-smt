from z3 import *
from floatsmt.sorts import *
import floatsmt.converter as converter

def FloatConst(name : str, mantissa_size : int, exponent_size : int) -> DatatypeRef:
    return Const(name, FloatSort(mantissa_size, exponent_size))

def FloatVar(sign : BitVecRef, mantissa : BitVecRef, exponent : BitVecRef, sort : DatatypeSortRef) -> DatatypeRef:
    return sort.mk(sign, mantissa, exponent)

def FloatVarBV(bv : BitVecRef, sort : DatatypeSortRef) -> DatatypeRef:
    m, e = sizes(sort)
    sign = Extract(m+e, m+e, bv)
    mantissa = Extract(m-1, 0, bv)
    exponent = Extract(m+e-1, m, bv)
    return FloatVar(sign, mantissa, exponent, sort)

def FloatVarZero(sort : DatatypeSortRef, sign = BitVecVal(0, 1)) -> DatatypeRef:
    m, e = sizes(sort)
    return FloatVar(sign, BitVecVal(0, m), BitVecVal(0, e), sort)

def FloatVal(sign : int, mantissa : int, exponent : int, sort : DatatypeSortRef) -> DatatypeRef:
    m, e = sizes(sort)
    return sort.mk(BitVecVal(sign, 1), BitVecVal(mantissa, m), BitVecVal(exponent, e))

def FloatValDec(dec_val : str, rounding_mode : converter.RoundingMode, sort : DatatypeSortRef) -> DatatypeRef:
    m, e = sizes(sort)
    f = converter.convert(dec_val, rounding_mode, m, e)
    return FloatVal(int(f.s), int(f.m, 2), int(f.e, 2), sort)

def FloatValPosInf(sort : DatatypeSortRef) -> DatatypeRef:
    _, e = sizes(sort)
    return FloatVal(0, 0, 2**e - 1, sort)

def FloatValNegInf(sort : DatatypeSortRef) -> DatatypeRef:
    _, e = sizes(sort)
    return FloatVal(1, 0, 2**e - 1, sort)

def FloatValNaN(sort : DatatypeSortRef, value = 1) -> DatatypeRef:
    if value == 0:
        raise ValueError("NaN value cannot be zero")
    _, e = sizes(sort)
    sign = int(value >= 0)
    return FloatVal(sign, value, 2**e - 1, sort)

def FloatValZero(sort : DatatypeSortRef, sign = 0) -> DatatypeRef:
    return FloatVal(sign, 0, 0, sort)