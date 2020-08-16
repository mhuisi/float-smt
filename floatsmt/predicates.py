from z3 import *
from floatsmt.conversions import *

def eq_bitwise(a : DatatypeRef, b : DatatypeRef) -> BoolRef:
    ensure_eq_sort(a, b)
    return to_ieee_bv(a) == to_ieee_bv(b)

def is_pos_zero(a : DatatypeRef) -> BoolRef:
    return eq_bitwise(a, FloatValZero(get_sort(a), 0))

def is_neg_zero(a : DatatypeRef) -> BoolRef:
    return eq_bitwise(a, FloatValZero(get_sort(a), 1))

def is_zero(a : DatatypeRef) -> BoolRef:
    '''
    Checks whether a is +0 or -0.
    '''
    return Or(is_pos_zero(a), is_neg_zero(a))

def is_inf(a : DatatypeRef) -> BoolRef:
    '''
    Checks whether a is +inf or -inf.
    '''
    s = get_sort(a)
    m, e = sizes(s)
    return And(s.mantissa(a) == BitVecVal(0, m), s.exponent(a) == BitVecVal(2**e - 1, e))

def is_pos_inf(a : DatatypeRef) -> BoolRef:
    s = get_sort(a)
    return And(is_inf(a), s.sign(a) == 0)

def is_neg_inf(a : DatatypeRef) -> BoolRef: 
    s = get_sort(a)
    return And(is_inf(a), s.sign(a) == 1)

def is_nan(a : DatatypeRef, nan_value : int = 0) -> BoolRef: 
    '''
    Checks whether a is a NaN value (and optionally whether it is a specific NaN value).
    Note that arithmetical operations do not preserve the NaN value as of now.
    '''
    s = get_sort(a)
    m, e = sizes(s)
    mantissa = s.mantissa(a)
    exponent = s.exponent(a)
    inf_exp = BitVecVal(2**e-1, e)
    # nan_value = 0 is used to signalize that no specific NaN value is required
    if nan_value == 0:
        # exclude zero mantissa so we do not accidentally declare infs as NaNs
        return And(Not(mantissa == BitVecVal(0, m)), exponent == inf_exp)
    else:
        return And(mantissa == BitVecVal(nan_value, m), exponent == inf_exp)

def is_subnormal(a : DatatypeRef) -> BoolRef:
    '''
    Checks whether a is subnormal. Floats are subnormal if their exponent is 0
    and they not inf.
    '''
    s = get_sort(a)
    m, e = sizes(s)
    return And(Not(s.mantissa(a) == BitVecVal(0, m)), s.exponent(a) == BitVecVal(0, e))

def is_normal(a : DatatypeRef) -> BoolRef:
    '''
    Checks whether a is normal. A float is normal
    if it is not subnormal, not zero and neither NaN nor inf.
    '''
    return And(Not(is_inf(a)), 
               Not(is_zero(a)), 
               Not(is_subnormal(a)), 
               Not(is_nan(a)))

def eq_float(a : DatatypeRef, b : DatatypeRef) -> BoolRef:
    '''
    Checks whether a is equal to b:
    If either is NaN, the two are unequal.
    If both are zero (either pos. or neg. zero), they are equal.
    Otherwise, they are equal iff their bitvector representations are equal.
    '''
    ensure_eq_sort(a, b)
    return And(Not(Or(is_nan(a), is_nan(b))), 
               Or(And(is_zero(a), is_zero(b)), 
                  eq_bitwise(a, b)))

def gt(a : DatatypeRef, b : DatatypeRef) -> BoolRef:
    '''
    Checks whether a is greater than b.
    If either is NaN or both are zero, a is not greater than b.
    '''
    ensure_eq_sort(a, b)
    s = get_sort(a)
    a_bv, b_bv = Concat(s.exponent(a), s.mantissa(a)), Concat(s.exponent(b), s.mantissa(b))
    abs_gt = UGT(a_bv, b_bv)
    abs_lt = ULT(a_bv, b_bv)
    return And(Not(Or(is_nan(a), is_nan(b))),
               Not(And(is_zero(a), is_zero(b))),
               Or(And(s.sign(a) == 0, s.sign(b) == 1), 
                  And(s.sign(a) == 0, s.sign(b) == 0, abs_gt),
                  And(s.sign(a) == 1, s.sign(b) == 1, abs_lt)))

def lt(a : DatatypeRef, b : DatatypeRef) -> BoolRef:
    '''
    Checks whether a is less than b.
    If either is NaN or both are zero, a is not less than b.
    '''
    return gt(b, a)

def gte(a : DatatypeRef, b : DatatypeRef) -> BoolRef:
    '''
    Checks whether a is greater than or equal to b.
    If either is NaN, a is not greater than or equal to b.
    '''
    return Or(gt(a, b), eq_float(a, b))

def lte(a : DatatypeRef, b : DatatypeRef) -> BoolRef:
    '''
    Checks whether a is less than or equal to b.
    If either is NaN, a is not less than or equal to b.
    '''
    return gte(b, a)