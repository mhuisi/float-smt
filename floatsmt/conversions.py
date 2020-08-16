from z3 import *
from floatsmt.constructors import *

def to_ieee_bv(a : DatatypeRef) -> BitVecNumRef:
    s = get_sort(a)
    return Concat(s.sign(a), s.exponent(a), s.mantissa(a))

def Float_to_z3FP(x : DatatypeRef) -> FPRef:
    sort = get_sort(x)
    m,e = sizes(sort)
    x_bv = to_ieee_bv(x)
    # m+1 due to z3 including the sign bit in the mantissa as its a signed bv
    return fpBVToFP(x_bv, FPSort(e, m+1)) 

def z3FP_to_Float(x : FPRef) -> DatatypeRef:
    x_bv = fpToIEEEBV(x)
    # -1 due to z3 including the sign bit in the mantissa as its a signed bv
    return FloatVarBV(x_bv, FloatSort(x.sbits()-1, x.ebits()))

def rm_to_z3rm(rm : RoundingMode) -> FPRMRef:
    switch = {
        NearestTiesToEven : RoundNearestTiesToEven(), 
        NearestTiesAwayFromZero : RoundNearestTiesToAway(), 
        Up : RoundTowardPositive(), 
        Down : RoundTowardNegative(), 
        Truncate: RoundTowardZero(),
    }
    return switch.get(rm, -1)

def z3rm_to_rm(rm : RoundingMode) -> FPRMRef:
    switch = {
        RoundNearestTiesToEven(): NearestTiesToEven,
        RoundNearestTiesToAway(): NearestTiesAwayFromZero,
        RoundTowardPositive(): Up,
        RoundTowardNegative(): Down,
        RoundTowardZero(): Truncate,
    }
    return switch.get(rm, -1)