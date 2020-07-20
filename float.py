from z3 import *
import converter
import utils.utils as utils

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

def FloatConst(name : str, mantissa_size : int, exponent_size : int) -> DatatypeRef:
    return Const(name, FloatSort(mantissa_size, exponent_size))

def FloatVar(sign : BitVecRef, mantissa : BitVecRef, exponent : BitVecRef, sort : DatatypeSortRef) -> DatatypeRef:
    #f = Const("test", sort)
    return sort.mk(sign, mantissa, exponent)

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

def get_sort(a : DatatypeRef) -> DatatypeSortRef:
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
def eq_float(a : DatatypeRef, b : DatatypeRef) -> BoolRef:
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
    return Or(gt(a, b), eq_float(a, b))

# Checks whether a is less than or equal to b.
# If either is NaN, a is not less than or equal to b.
def lte(a : DatatypeRef, b : DatatypeRef) -> BoolRef:
    return gte(b, a)

# Sets the sign bit to 0
def abs(a : DatatypeRef) -> DatatypeRef:
    s = get_sort(a)
    sign = BitVecVal(0, 1)
    return FloatVar(sign, s.mantissa(a), s.exponent(a), s)

# Inverts the sign bit
def neg(a : DatatypeRef) -> DatatypeRef:
    s = get_sort(a)
    sign = If(s.sign(a) == 1, BitVecVal(0, 1), BitVecVal(1, 1))
    return FloatVar(sign, s.mantissa(a), s.exponent(a), s)

# should be cheaper to keep track of the unpacked state with this flag
FloatCase, (unpacked_normal_case, zero_case, inf_case, nan_case) = EnumSort('FloatCase', ['unpacked_normal', 'zero', 'inf', 'nan'])

# Unpacks a float, identifying the specific case of float (as above),
# prepending the implicit leading 1 to the mantissa and turning the exponent into
# an unbiased signed representation.
# The float returned by this function is not a real float, in the sense
# that if case != unpacked_normal_case, is_* predicates do not work as expected
# anymore, there's no implicit leading 1 for normal floats and the real exponent
# is one larger than the one yielded by the float of this function.
def unpack(f : DatatypeRef) -> (DatatypeRef, DatatypeRef):
    s = get_sort(f)
    m, e = sizes(s)
    new_sort = FloatSort(m+1, e)
    sign = s.sign(f)
    mantissa = s.mantissa(f)
    exponent = s.exponent(f) - BitVecVal(2**(e-1) - 1, e)
    case = If(is_nan(f), nan_case, 
           If(is_inf(f), inf_case, 
           If(is_zero(f), zero_case,
              unpacked_normal_case)))
    extended_subnormal = FloatVar(sign, ZeroExt(1, mantissa), exponent, new_sort)
    extended_normal = FloatVar(sign, Concat(BitVecVal(1, 1), mantissa), exponent, new_sort)
    return case, If(is_subnormal(f), extended_subnormal, extended_normal)

def pack(f : DatatypeRef, sort : DatatypeSortRef, rounding_mode : DatatypeRef) -> DatatypeRef:
    # TODO: turn f into a proper float again, normalize number and round with remainder of mantissa in f.
    # the mantissa of f is of the form 0...01x...xy...y, where 1x...x are the first m bits of the mantissa (m is the mantissa size in sort),
    # and y...y is the remainder.
    # the sign in f should be correct independent of the case.
    # should also take care of edge cases like inf/nan/etc after the operation.
    s = get_sort(f)
    m, e = sizes(sort)
    sign = s.sign(f)
    mantissa = s.mantissa(f)
    leading_zeros = utils.clz(mantissa)
    leading_zeros_padded_e = ZeroExt(e-leading_zeros.size(), leading_zeros)
    leading_zeros_padded_m = ZeroExt(mantissa.size()-leading_zeros.size(), leading_zeros)

    exponent = s.exponent(f) + BitVecVal(2**(e-1) - 1, e)
    exponent_padded_m = ZeroExt(mantissa.size()-exponent.size(), exponent)


    normal = BVSubNoUnderflow(exponent, leading_zeros_padded_e, False)

    exponent = If(normal, exponent - leading_zeros_padded_e, BitVecVal(0, e))


    remainder = LShR(mantissa, -(mantissa.size()-m)) #due to extract not working on symbolic expressions, also no need to shift back again
    

    mantissa = If(normal,
        Extract(mantissa.size()-2, (mantissa.size()-1-m), LShR(mantissa, -(leading_zeros_padded_m))),
        Extract(mantissa.size()-2, (mantissa.size()-1-m), LShR(mantissa, exponent_padded_m))
        )


    # Following, you'll see the biggest If-condition mess known to mankind
    half_of_max_remainder = 2**remainder.size()-1

    round_nearest_tie_even = If(remainder == half_of_max_remainder,
                                    If(URem(mantissa, 2) == 1, BitVecVal(1, m), BitVecVal(0, m)),
                                    If(remainder > half_of_max_remainder,
                                        BitVecVal(1, m),
                                        BitVecVal(0, m)
                                    ))
    round_nearest_tie_zero = If(remainder == half_of_max_remainder,
                                    BitVecVal(0, m),
                                    If(remainder > half_of_max_remainder,
                                        BitVecVal(1, m),
                                        BitVecVal(0, m)
                                    ))
    round_up = If(sign == 1, BitVecVal(0, m), BitVecVal(1, m))
    round_down = If(sign == 1, BitVecVal(1, m), BitVecVal(0, m))
    round_truncate = BitVecVal(0, m)

    round_addition = If(rounding_mode == NearestTieToEven, round_nearest_tie_even,
                     If(rounding_mode == NearestTieAwayFromZero, round_nearest_tie_zero,
                     If(rounding_mode == Up, round_up,
                     If(rounding_mode == Down, round_down,
                     If(rounding_mode == Truncate, round_truncate, 0 #The second case should not be possible
                     )))))

    mantissa = mantissa + round_addition
    
    return FloatVar(sign, mantissa, exponent, sort)

# Adds the floating point values a & b
def add(a : DatatypeRef, b : DatatypeRef) -> DatatypeRef: pass

# Subtracts b from a
def sub(a : DatatypeRef, b : DatatypeRef) -> DatatypeRef:
    return add(a, neg(b))

# Multiplies a with b
def mul(a : DatatypeRef, b : DatatypeRef) -> DatatypeRef:
    ensure_eq_sort(a, b)
    case_a, a = unpack(a)
    case_b, b = unpack(b)
    # handle some special cases preemptively so we don't
    # accidentally lose that information during the operation
    result_case = If(Or(case_a == nan_case, 
                        case_b == nan_case, 
                        And(case_a == inf_case, case_b == zero_case), 
                        And(case_a == zero_case, case_b == inf_case)),
                     nan_case,
                  If(Or(case_a == inf_case, case_b == inf_case), 
                     inf_case,
                  If(Or(case_a == zero_case, case_b == zero_case),
                     zero_case,
                     unpacked_normal_case))) # could still be zero or inf instead after the operation (underflow or overflow)
    s = get_sort(a)
    m, e = sizes(s)
    a_mantissa = ZeroExt(m, s.mantissa(a))
    b_mantissa = ZeroExt(m, s.mantissa(b))
    mantissa_result = a_mantissa * b_mantissa
    underflow = Not(BVAddNoUnderflow(s.exponent(a), s.exponent(b), True))
    overflow = Not(BVAddNoOverflow(s.exponent(a), s.exponent(b), True))
    exponent_result = s.exponent(a) + s.exponent(b)
    result_case = If(result_case != unpacked_normal_case, result_case, 
                  If(underflow, zero_case, 
                  If(overflow, inf_case, unpacked_normal_case)))
    result_sign = If(s.sign(a) == s.sign(b), BitVecVal(0, 1), BitVecVal(1, 1))
    result = pack(FloatVar(result_sign, mantissa_result, exponent_result, s))
    return result

# Divides a by b
def div(a : DatatypeRef, b : DatatypeRef) -> DatatypeRef: pass

# Performs the operation a modulo b
def rem(a : DatatypeRef, b : DatatypeRef) -> DatatypeRef: pass

# Performs the square-root operation on a node a
def sqrt(a : DatatypeRef) -> DatatypeRef: pass

# Performs the operation a + (b * c)
def fma(a : DatatypeRef, b : DatatypeRef, c : DatatypeRef) -> DatatypeRef:
    return add(a, mul(b,c)) # TODO: Fix incorrect impl (fma is add & multiply, but only rounds after both)

# Returns a node containing a if a <= b and b else
def min(a : DatatypeRef, b : DatatypeRef) -> DatatypeRef:
    return If(gte(a, b), b, a)

# Returns a node containing a if a >= b and b else
def max(a : DatatypeRef, b : DatatypeRef) -> DatatypeRef:
    return neg(min(neg(a), neg(b)))
