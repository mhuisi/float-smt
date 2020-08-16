from z3 import *
from floatsmt.predicates import *
from floatsmt.utils import *

# Special case of unpacked floats
FloatCase, (unpacked_normal_case, zero_case, inf_case, nan_case) = EnumSort('FloatCase', ['unpacked_normal', 'zero', 'inf', 'nan'])

def unpack(f : DatatypeRef) -> (DatatypeRef, DatatypeRef):
    '''
    Unpacks a float, identifying the specific case of float (as above),
    prepending the implicit leading 1 to the mantissa and turning the exponent into
    an unbiased signed representation.
    The float returned by this function is not a real float, in the sense
    that if case != unpacked_normal_case, is_* predicates do not work as expected
    anymore, there's no implicit leading 1 for normal floats and the real exponent
    is one larger than the one yielded by the float of this function.
    '''
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

    # the 0 case in the following if-else should never occur
    round_addition = If(rounding_mode == NearestTiesToEven, round_nearest_tie_even,
                     If(rounding_mode == NearestTiesAwayFromZero, round_nearest_tie_away_zero,
                     If(rounding_mode == Up, round_up,
                     If(rounding_mode == Down, round_down, round_truncate))))
    overflow = Not(BVAddNoOverflow(val, round_addition, False))
    val = val + round_addition
    return val, overflow

def pack(f : DatatypeRef, sort : DatatypeSortRef, rounding_mode : DatatypeRef = Truncate, case : DatatypeRef = unpacked_normal_case) -> DatatypeRef:
    # the mantissa of f is of the form 0...01x...xy...y, where 1x...x are the first m bits of the mantissa (m is the mantissa size in sort),
    # and y...y is the remainder.
    s = get_sort(f)
    m_old, e_old = sizes(s)
    m, e = sizes(sort)
    sign = s.sign(f)
    mantissa = s.mantissa(f)

    exponent = s.exponent(f) + BitVecVal(2**(e-1) - 1, e_old)
    extra_mantissa_bits, _ = matched_sizes([mantissa, exponent])
    mantissa, exponent = match_sizes([(mantissa, False), (exponent, True)])

    added_leading_zeros = -exponent + 1
    added_leading_zeros = If(added_leading_zeros > 0, added_leading_zeros, 0)
    # calculate a sticky bit so that we can retain the remainder bits that we shift out
    sticky_bit = If((mantissa & ((1 << added_leading_zeros) - 1)) == 0, 
                    BitVecVal(0, mantissa.size()), 
                    BitVecVal(1, mantissa.size()))
    mantissa = LShR(mantissa, added_leading_zeros)
    mantissa = mantissa | sticky_bit
    exponent = exponent + added_leading_zeros

    leading_zeros = clz(mantissa) - extra_mantissa_bits
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
    # pack does *not* preserve nan error codes as of now. there's some subtlety to this.
    mantissa = If(Or(case == inf_case, case == zero_case), BitVecVal(0, m), 
               If(case == nan_case, BitVecVal(1, m), mantissa))
    return FloatVar(sign, mantissa, exponent, sort)

def convert_float(a : DatatypeRef, new_sort : DatatypeSortRef, rm : RoundingMode) -> DatatypeRef:
    old_sort = get_sort(a)
    case_a, a = unpack(a)
    unpacked_sort = get_sort(a)

    m_old, e_old = sizes(old_sort)
    m_new, e_new = sizes(new_sort)

    size_dif_m = max(m_new - m_old, 0)
    size_dif_e = max(e_new - e_old, 0)

    mantissa_new = ZeroExt(size_dif_m, unpacked_sort.mantissa(a)) << size_dif_m
    exponent_new = SignExt(size_dif_e, unpacked_sort.exponent(a))
    extended_sort = FloatSort(mantissa_new.size(), exponent_new.size())

    result_extended = FloatVar(unpacked_sort.sign(a), mantissa_new, exponent_new, extended_sort)
    return pack(result_extended, new_sort, rm, case_a)