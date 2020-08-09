from z3 import *
from typing import List
import converter
import utils.utils as utils
from math import log2, floor

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

def FloatValBV(bv : BitVecRef, sort : DatatypeSortRef) -> DatatypeRef:
    m, e = sizes(sort)
    mantissa = Extract(m-1, 0, bv)
    exponent = Extract(m+e-1, m, bv)
    sign = Extract(m+e, m+e, bv)
    return FloatVar(sign, mantissa, exponent, sort)

def FloatValPosInf(sort : DatatypeSortRef) -> DatatypeRef:
    m, e = sizes(sort)
    return FloatVal(0, 0, 2**e - 1, sort)

def FloatValNegInf(sort : DatatypeSortRef) -> DatatypeRef:
    m, e = sizes(sort)
    return FloatVal(1, 0, 2**e - 1, sort)

def FloatValNaN(sort : DatatypeSortRef, value = 1) -> DatatypeRef:
    if value == 0:
        raise ValueError("NaN value cannot be zero")
    m, e = sizes(sort)
    sign = int(value >= 0)
    return FloatVal(sign, value, 2**e - 1, sort)

def FloatValZero(sort : DatatypeSortRef, sign = 0) -> DatatypeRef:
    m, e = sizes(sort)
    return FloatVal(sign, 0, 0, sort)

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
    return eq_bitwise(a, FloatValZero(get_sort(a), 0))

# Checks whether a is -0
def is_neg_zero(a : DatatypeRef) -> BoolRef:
    return eq_bitwise(a, FloatValZero(get_sort(a), 1))

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

def matched_sizes(xs):
    max_size = -1
    for x in xs:
        if x.size() > max_size:
            max_size = x.size()
    return [max_size - x.size() for x in xs]

def match_sizes(xs):
    s = zip([x for x, _ in xs], matched_sizes([x for x, _ in xs]), [signed for _, signed in xs])
    return [SignExt(n, x) if signed else ZeroExt(n, x) for x, n, signed in s]

# function which ensures that value + added cannot overflow
def guarantee_space(value, added, signed, offset=0):
    n = value.size()-1 if signed else value.size()
    # in order for value + added to not overflow, we need
    # 2**(n + k) - 2**n > added <=> 2**n*(2**k - 1) > added <=> k > log2(added/2**n + 1)
    added_bits = floor(log2(added/2**n + 1)) + 1 + offset
    value = SignExt(added_bits, value) if signed else ZeroExt(added_bits, value)
    return value

# debug function to print bit repr of z3 bitvectors
def val(x):
    return ("{:0%db}" % x.size()).format(int(str(simplify(x))))

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
    new_sort = FloatSort(m+1, e+2)
    sign = s.sign(f)
    mantissa = s.mantissa(f)
    # two extra bits should be sufficient to never run into overflows in pack().
    # operations may need to expand the exponent further.
    exponent = ZeroExt(2, s.exponent(f))
    exponent = exponent - BitVecVal(2**(e-1) - 1, exponent.size())
    case = If(is_nan(f), nan_case, 
           If(is_inf(f), inf_case, 
           If(is_zero(f), zero_case,
              unpacked_normal_case)))
    # subnormals use exponent = 0 for signalling but the real exponent is 1. 
    extended_subnormal = FloatVar(sign, ZeroExt(1, mantissa), exponent + 1, new_sort)
    extended_zero = FloatVar(sign, ZeroExt(1, mantissa), exponent, new_sort)
    extended_normal = FloatVar(sign, Concat(BitVecVal(1, 1), mantissa), exponent, new_sort)
    return case, If(is_subnormal(f), extended_subnormal, 
                 If(is_zero(f), extended_zero, extended_normal))

def round(sign : DatatypeRef, val : DatatypeRef, remainder : DatatypeRef, rounding_mode : DatatypeRef = Truncate) -> (DatatypeRef, DatatypeRef):
    m = val.size()
    # Following, you'll see the biggest If-condition mess known to mankind
    half_of_max_remainder = 2**(remainder.size()-1)
    round_nearest_tie_even = If(remainder == half_of_max_remainder,
                                If(URem(val, 2) == 1, BitVecVal(1, m), BitVecVal(0, m)),
                                If(UGT(remainder, half_of_max_remainder),
                                   BitVecVal(1, m),
                                   BitVecVal(0, m)))
    round_nearest_tie_away_zero = If(remainder == half_of_max_remainder,
                                     BitVecVal(1, m),
                                     If(UGT(remainder, half_of_max_remainder),
                                        BitVecVal(1, m),
                                        BitVecVal(0, m)))
    round_up = If(UGT(remainder, 0),
                  If(sign == 1, BitVecVal(0, m), BitVecVal(1, m)),
                  BitVecVal(0, m))
    round_down = If(UGT(remainder, 0),
                    If(sign == 1, BitVecVal(1, m), BitVecVal(0, m)),
                    BitVecVal(0, m))
    round_truncate = BitVecVal(0, m)

    round_addition = If(rounding_mode == NearestTieToEven, round_nearest_tie_even,
                     If(rounding_mode == NearestTieAwayFromZero, round_nearest_tie_away_zero,
                     If(rounding_mode == Up, round_up,
                     If(rounding_mode == Down, round_down,
                     If(rounding_mode == Truncate, round_truncate, 0))))) # the 0 case should not be possible
    overflow = Not(BVAddNoOverflow(val, round_addition, False))
    val = val + round_addition
    return val, overflow

def pack(f : DatatypeRef, sort : DatatypeSortRef, rounding_mode : DatatypeRef = Truncate, case : DatatypeRef = unpacked_normal_case) -> DatatypeRef:
    # the mantissa of f is of the form 0...01x...xy...y, where 1x...x are the first m bits of the mantissa (m is the mantissa size in sort),
    # and y...y is the remainder.
    # the sign in f should be correct independent of the case.
    # should also take care of edge cases like inf/nan/etc after the operation.
    s = get_sort(f)
    m_old, e_old = sizes(s)
    m, e = sizes(sort)
    sign = s.sign(f)
    mantissa = s.mantissa(f)
    # add bias
    exponent = s.exponent(f) + BitVecVal(2**(e-1) - 1, e_old)
    extra_mantissa_bits, _ = matched_sizes([mantissa, exponent])
    mantissa, exponent = match_sizes([(mantissa, False), (exponent, True)])

    added_leading_zeros = -exponent + 1
    added_leading_zeros = If(added_leading_zeros > 0, added_leading_zeros, 0)
    # calculate a sticky bit so that we can retain the remainder bits that we shift out
    sticky_bit = If(mantissa << (mantissa.size() - added_leading_zeros) == 0, 
                    BitVecVal(0, mantissa.size()), 
                    BitVecVal(1, mantissa.size()))
    mantissa = LShR(mantissa, added_leading_zeros)
    mantissa = mantissa | sticky_bit
    exponent = exponent + added_leading_zeros

    leading_zeros = utils.clz(mantissa) - extra_mantissa_bits
    normal_pre_rounding = UGT(exponent, leading_zeros)
    # the 0 exponent is only used for signalling.
    # the real exponent is 1 => we can only remove exponent-1 many zeros
    removed_leading_zeros = If(normal_pre_rounding, leading_zeros, exponent-1)
    remainder_bits = m_old - m - removed_leading_zeros - 1
    # remainder to be interpeted as bv, not a numerical representation: if the cut off bits were 101, the remainder would be 101000...
    remainder = mantissa << (mantissa.size() - remainder_bits)
    mantissa = Extract(m_old-2, m_old-1-m, mantissa << removed_leading_zeros)

    mantissa, round_overflow = round(sign, mantissa, remainder, rounding_mode)
    # since mantissa only contains the significand digits, a round overflow will exactly yield
    # the zero mantissa, hence we need to do no additional work on the mantissa during renormalization.
    exponent = If(round_overflow, exponent+1, exponent)
    normal_post_rounding = UGT(exponent, leading_zeros)
    exponent = If(normal_post_rounding, exponent - leading_zeros, BitVecVal(0, exponent.size()))

    underflow = And(Not(normal_post_rounding), mantissa == BitVecVal(0, m))
    overflow = UGE(exponent, 2**e - 1)

    exponent = Extract(e-1, 0, exponent)
    exponent = If(underflow, BitVecVal(0, e), 
               If(overflow, BitVecVal(2**e-1, e), exponent))
    mantissa = If(Or(underflow, overflow), BitVecVal(0, m), mantissa) 

    exponent = If(case == zero_case, BitVecVal(0, e), 
               If(Or(case == inf_case, case == nan_case), BitVecVal(2**e-1, e), exponent))
    # pack does *not* preserve nan error codes as of now. there's some subtlety to this,
    # and we need to carefully read the standard if we want to get this to work.
    mantissa = If(Or(case == inf_case, case == zero_case), BitVecVal(0, m), 
               If(case == nan_case, BitVecVal(1, m), mantissa))

    return FloatVar(sign, mantissa, exponent, sort)




def __add_core(x: DatatypeRef, y: DatatypeRef):
    old_sort = get_sort(x)
    m,e = sizes(old_sort)

    exponent_diff = old_sort.exponent(x) - old_sort.exponent(y)

    max_shift = 2**e

    #Adding additional bits at the end of the mantissas for rounding:
    mantissa_x, mantissa_y = old_sort.mantissa(x), old_sort.mantissa(y)
    mantissa_x, mantissa_y = ZeroExt(max_shift, mantissa_x), ZeroExt(max_shift, mantissa_y)
    mantissa_x, mantissa_y = mantissa_x << max_shift, mantissa_y << max_shift

    #Shifting the y mantissa to match the exponent of x:
    exponent_diff = ZeroExt(mantissa_y.size() - exponent_diff.size(), exponent_diff)
    mantissa_y_shifted = LShR(mantissa_y, exponent_diff)


    #Overflow stuff:
    overflow = Not(BVAddNoOverflow(mantissa_x, mantissa_y_shifted, False))
    exponent_result = If(overflow, old_sort.exponent(x) + 1, old_sort.exponent(x))

    mantissa_y_shifted = If(overflow, LShR(mantissa_y_shifted, 1), mantissa_y_shifted)
    mantissa_x = If(overflow, LShR(mantissa_x, 1), mantissa_x)

    #Compute mantissa
    mantissa_result = If(old_sort.sign(x) + old_sort.sign(y) == 1, mantissa_x - mantissa_y_shifted, mantissa_x + mantissa_y_shifted)
    
    #should work due to x having the bigger value:
    sign_result = old_sort.sign(x)

    new_sort = FloatSort(mantissa_result.size(), e)

    return sign_result, mantissa_result, exponent_result, new_sort



# Adds the floating point values a & b
def add(a : DatatypeRef, b : DatatypeRef, rounding_mode : DatatypeRef = Truncate) -> DatatypeRef:
    ensure_eq_sort(a, b)
    
    #Unpack the floats and put the (absolute) bigger one into x:
    sort = get_sort(a)
    x = If(gt(abs(a), abs(b)), a, b)
    y = If(gt(abs(a), abs(b)), b, a)
    case_x, x = unpack(x)
    case_y, y = unpack(y)

    result_case = If(
        Or(
            case_x == nan_case, 
            case_y == nan_case,
        ),
        nan_case,
        If(
            And(
                case_x == inf_case, 
                case_y == inf_case
            ),
            If(
                sort.sign(a) != sort.sign(b),
                nan_case, #pos_inf + neg_inf
                inf_case
            ),
            If(
                Or(
                    case_x == inf_case, 
                    case_y == inf_case
                ), 
                inf_case,
                unpacked_normal_case
            )
        )
    )
    
    sign_result, mantissa_result, exponent_result, new_sort = __add_core(x, y)

    #standard-conform zero sign handling:
    sign_result = If(
        neg(a) == b, #check if result is zero
        If(
            And(is_zero(a), a==b),#both are zero and equal
            sort.sign(a),
            If(
                rounding_mode == Down,
                BitVecVal(1, 1),#negative
                BitVecVal(0, 1)#positive
            )
        ),
        sign_result #result is not zero
    )
    

    return pack(FloatVar(sign_result, mantissa_result, exponent_result, new_sort), sort, rounding_mode, result_case)

# Subtracts b from a
def sub(a : DatatypeRef, b : DatatypeRef, rounding_mode : DatatypeRef = Truncate) -> DatatypeRef:
    return add(a, neg(b))


def __mul_core(a: DatatypeRef, b: DatatypeRef):
    s = get_sort(a)
    m, e = sizes(s)

    a_mantissa = ZeroExt(m, s.mantissa(a))
    b_mantissa = ZeroExt(m, s.mantissa(b))
    mantissa_result = a_mantissa * b_mantissa
    exponent_result = s.exponent(a) + s.exponent(b) + 1

    result_sign = s.sign(a) ^ s.sign(b)
    new_sort = FloatSort(mantissa_result.size(), exponent_result.size())
    return result_sign, mantissa_result, exponent_result, new_sort 

# Multiplies a with b
def mul(a : DatatypeRef, b : DatatypeRef, rounding_mode : DatatypeRef = Truncate) -> DatatypeRef:
    ensure_eq_sort(a, b)
    result_sort = get_sort(a)

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

    result_sign, mantissa_result, exponent_result, new_sort = __mul_core(a, b)
    
    result = pack(FloatVar(result_sign, mantissa_result, exponent_result, new_sort), result_sort, rounding_mode, result_case)
    return result

def __div_core(a : DatatypeRef, b : DatatypeRef):
    s = get_sort(a)
    m, e = sizes(s)
    # the division of the mantissas is badly behaved wrt rounding.
    # dividing two large mantissas 1xx...x and 1xx...x may yield a very small
    # result mantissa (e.g. 1 if they are equal). in the worst case, to normalize
    # the number, we need to shift the result mantissa a full 2m bits.
    # hence, when rounding, we need at least 2m additional bits for the normalization
    # alone, and then one additional guard- and sticky bit for rounding.
    # we gain this additional precision by extending the numerator by 2m+1 bits (bits to shift + guard bit)
    # and then calculate the sticky bit using the remainder of the division.
    a_mantissa = Concat(s.mantissa(a), BitVecVal(0, 2*m+1))
    b_mantissa = ZeroExt(2*m+1, s.mantissa(b))

    # yields a number where the most significant m bits are the quotient
    # and the rest are the remainder (bits for normalization & guard bit)
    padded_quotient = UDiv(a_mantissa, b_mantissa)
    # used to calculate the sticky bit
    padded_remainder = URem(a_mantissa, b_mantissa)

    quotient = Extract(padded_quotient.size()-1, 2*m+1, padded_quotient)
    remainder = Extract(2*m, 0, padded_quotient)
    sticky_bit = If(padded_remainder == 0, BitVecVal(0, 1), BitVecVal(1, 1))
    mantissa_result = Concat(quotient, remainder, sticky_bit)
    exponent_result = s.exponent(a) - s.exponent(b)

    result_sign = s.sign(a) ^ s.sign(b)
    return (result_sign, mantissa_result, exponent_result)

# Divides a by b
def div(a : DatatypeRef, b : DatatypeRef, rounding_mode : DatatypeRef = Truncate) -> DatatypeRef:
    ensure_eq_sort(a, b)
    result_sort = get_sort(a)

    case_a, a = unpack(a)
    case_b, b = unpack(b)
    m, e = sizes(get_sort(a))

    result_case = If(Or(case_a == nan_case, 
                        case_b == nan_case, 
                        And(case_a == zero_case, case_b == zero_case), 
                        And(case_a == inf_case, case_b == inf_case)),
                     nan_case,
                  If(Or(case_a == inf_case, case_b == zero_case), 
                     inf_case,
                  If(Or(case_a == zero_case, case_b == inf_case),
                     zero_case,
                     unpacked_normal_case)))

    sign, mantissa, exponent = __div_core(a, b)

    exponent = guarantee_space(exponent, m-1, True)
    exponent = exponent + BitVecVal(m-1, exponent.size())

    new_sort = FloatSort(mantissa.size(), exponent.size())

    result = pack(FloatVar(sign, mantissa, exponent, new_sort), result_sort, rounding_mode, result_case)
    return result

def __int_div(a : DatatypeRef, b : DatatypeRef, result_sort : DatatypeSortRef):
    r_m, r_e = sizes(result_sort)
    
    # first, we calculate a / b with its entire remainder.
    # then, we determine the amount of digits extra_leading_digits that we need to add so that
    # the result is an integer: 
    # min(m - leading_digits, exponent) either turns the entire mantissa into leading_digits,
    # or only uses exponent many digits if we cannot turn the entire mantissa into leading_digits
    # without reaching a negative exponent (for e = 0 we have exactly the exponent we want).
    # then, we extract the leading digits for the natural number and the remainder, and round the natural number
    # using the remainder. then, we use pack to normalize the result.
    sign, mantissa, exponent = __div_core(a, b)

    mantissa, exponent = match_sizes([(mantissa, False), (exponent, True)])
    # this is kind of tricky: we want exponent + m to not overflow.
    # if we increase exponent, we need to increase m too (due to match_sizes), hence there's a recursion.
    # but it turns out that if k bits are enough for exponent + m to not overflow,
    # k + 1 bits are enough for exponent + m + k to not overflow!
    # after calling guarantee_space, we have that m <= 2**(e+k)-2**e.
    # additionally one can see that k <= 2**(e+k)-2**e = 2**e*(2**k-1).
    # it follows that m+k <= 2**(e+1+k)-2**(e+1) = 2*(2**(e+k)-2**e).
    exponent = guarantee_space(exponent, mantissa.size(), True, 1)
    mantissa, exponent = match_sizes([(mantissa, False), (exponent, True)])

    m, e = mantissa.size(), exponent.size()

    float_remainder_digits = BitVecVal(m - r_m, e)
    extra_leading_digits = If(float_remainder_digits < exponent, float_remainder_digits, exponent)
    # note that this value may be negative.
    leading_digits = BitVecVal(r_m, e) + extra_leading_digits
    remainder_digits = BitVecVal(m, e) - leading_digits

    nat = LShR(mantissa, remainder_digits)
    remainder = If(leading_digits < 0, LShR(mantissa, -leading_digits), mantissa << leading_digits)

    nat, round_overflow = round(0, nat, remainder, NearestTieToEven)
    internal_overflow = And(Not(round_overflow), nat == (BitVecVal(1, nat.size()) << leading_digits))
    nat = If(round_overflow, BitVecVal(1 << (nat.size() - 1), nat.size()),
          If(internal_overflow, BitVecVal(1, leading_digits.size()) << (leading_digits - 1), nat))
    exponent = exponent + If(Or(round_overflow, internal_overflow), 
                             BitVecVal(1, exponent.size()), 
                             BitVecVal(0, exponent.size()))

    # this is why we need the internal overflow check: if the nat is suddenly longer than leading_digits,
    # we'd lose the leading 1.
    nat = nat << remainder_digits
    exponent = exponent + r_m - 1

    # no rounding occurs here since we nullified the remainder.
    nat = pack(FloatVar(sign, nat, exponent, FloatSort(m, e)), result_sort, RoundNearestTiesToEven)

    return nat

# Performs the operation a modulo b
def rem(a : DatatypeRef, b : DatatypeRef) -> DatatypeRef:
    ensure_eq_sort(a, b)
    result_sort = get_sort(a)
    old_a = a

    case_a, a = unpack(a)
    case_b, b = unpack(b)

    nat = __int_div(a, b, result_sort)

    r = fma(a, neg(b), nat)
    r = If(Or(case_a == nan_case, 
              case_b == nan_case, 
              case_a == inf_case, 
              case_b == zero_case),
              FloatValNaN(result_sort),
        If(case_a == zero_case,
           FloatValZero(result_sort),
        If(case_b == inf_case, old_a, r)))
    return r


# Performs the square-root operation on a node a
def sqrt(a : DatatypeRef, rounding_mode : DatatypeRef = Truncate) -> DatatypeRef:
    sort = get_sort(a)
    m, e = sizes(sort)

    
    mantissa_result = 0
    exponent_result = sort.exponent(a) / 2 #TODO: check if this is true
    solution = FloatVar(BitVecVal(1,1), mantissa_result, exponent_result, sort)

    #Special cases:
    solution = If(Or(is_zero(a), is_nan(a)), #this shouldn't be necessary for zero, but could speed up the cases. neg_zero is also specifically pointed out in the standard
                    a,
                    If(lt(a, FloatValZero(sort, 1)),
                        FloatValNaN(sort),
                        solution
                    )
                )
    
    return solution

# Performs the operation a + (b * c)
def fma(a : DatatypeRef, b : DatatypeRef, c : DatatypeRef, rounding_mode : DatatypeRef = Truncate) -> DatatypeRef:
    ensure_eq_sort(a, b)
    ensure_eq_sort(a, c)
    result_sort = get_sort(a)
    old_a = a

    case_a, a = unpack(a)
    case_b, b = unpack(b)
    case_c, c = unpack(c)

    unpack_sort = get_sort(a)
    unpack_m, unpack_e = sizes(unpack_sort)

    # handle some special cases preemptively so we don't
    # accidentally lose that information during the operation
    # this is for the multiplication only, addition comes below
    result_case = If(Or(case_b == nan_case, 
                        case_c == nan_case, 
                        And(case_b == inf_case, case_c == zero_case), 
                        And(case_b == zero_case, case_c == inf_case)),
                     nan_case,
                  If(Or(case_b == inf_case, case_c == inf_case), 
                     inf_case,
                  If(Or(case_b == zero_case, case_c == zero_case),
                     zero_case,
                     unpacked_normal_case))) # could still be zero or inf instead after the operation (underflow or overflow)

    sign_mul, mantissa_mul, exponent_mul, mul_sort = __mul_core(b, c)
    mul_result = FloatVar(sign_mul, mantissa_mul, exponent_mul, mul_sort)
    m_mul, e_mul = sizes(mul_sort)

    size_dif = mantissa_mul.size() - unpack_m
    mantissa_a_new = ZeroExt(size_dif, unpack_sort.mantissa(a)) << size_dif #append size_dif many zeros to the right
    extended_a = FloatVar(unpack_sort.sign(a), mantissa_a_new, unpack_sort.exponent(a), mul_sort)

    intermediate_result = pack(mul_result, result_sort, rounding_mode, result_case)


    #resolve troubles due to multiple operations being executed
    mul_sort = FloatSort(m_mul-1, e_mul-2) #-1 due to implicit bit, -2 to get back to original exponent size (revert unpack)
    mul_result = pack(mul_result, mul_sort, Truncate, result_case) #Truncate due to no bits being cut off
    extended_a = pack(extended_a, mul_sort, Truncate, case_a) #Truncate due to no bits being cut off

    # ensure that the first operand is the bigger one
    x = If(gt(abs(intermediate_result), abs(old_a)), mul_result, extended_a)
    y = If(gt(abs(intermediate_result), abs(old_a)), extended_a, mul_result)

    extended_result = add(x, y)
    result_case, result_unpacked = unpack(extended_result)
    sign_result = get_sort(result_unpacked).sign(result_unpacked)

    result = pack(result_unpacked, result_sort, rounding_mode, result_case)
    return result
    
# Returns a node containing a if a <= b and b else
def min(a : DatatypeRef, b : DatatypeRef, rounding_mode : DatatypeRef = Truncate) -> DatatypeRef:
    return If(gte(a, b), b, a)

# Returns a node containing a if a >= b and b else
def max(a : DatatypeRef, b : DatatypeRef, rounding_mode : DatatypeRef = Truncate) -> DatatypeRef:
    return neg(min(neg(a), neg(b)))

def Float_to_z3FP(x : DatatypeRef) -> FPRef:
    sort = get_sort(x)
    m,e = sizes(sort)
    x_bv = to_ieee_bv(x)
    return fpBVToFP(x_bv, FPSort(e, m+1)) #e+1 due to z3 including the sign bit in the mantissa as its a signed bv

def z3FP_to_Float(x: FPRef) -> DatatypeRef:
    x_bv = fpToIEEEBV(x)
    return FloatValBV(x_bv,FloatSort(x.sbits()-1, x.ebits()))#-1 due to z3 including the sign bit in the mantissa as its a signed bv

def rm_to_z3rm(rm: RoundingMode) -> FPRMRef:
    switch = {
        NearestTieToEven : RoundNearestTiesToEven(), 
        NearestTieAwayFromZero : RoundNearestTiesToAway(), 
        Up : RoundTowardPositive(), 
        Down : RoundTowardNegative(), 
        Truncate: RoundTowardZero(),
    }
    return switch.get(rm, -1) #Using -1 as an error return value

def z3rm_to_rm(rm: RoundingMode) -> FPRMRef:
    switch = {
        RoundNearestTiesToEven(): NearestTieToEven,
        RoundNearestTiesToAway(): NearestTieAwayFromZero,
        RoundTowardPositive(): Up,
        RoundTowardNegative(): Down,
        RoundTowardZero(): Truncate,
    }
    return switch.get(rm, -1) #Using -1 as an error return value