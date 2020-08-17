from z3 import *
from floatsmt.packing import *

def abs(a : DatatypeRef) -> DatatypeRef:
    s = get_sort(a)
    return FloatVar(BitVecVal(0, 1), s.mantissa(a), s.exponent(a), s)

def neg(a : DatatypeRef) -> DatatypeRef:
    '''
    Inverts the sign bit.
    '''
    s = get_sort(a)
    sign = If(s.sign(a) == 1, BitVecVal(0, 1), BitVecVal(1, 1))
    return FloatVar(sign, s.mantissa(a), s.exponent(a), s)

def __add_core(x: DatatypeRef, y: DatatypeRef):
    old_sort = get_sort(x)
    _, e = sizes(old_sort)

    exponent_diff = old_sort.exponent(x) - old_sort.exponent(y)

    # add additional bits at the end of the mantissas for rounding
    max_shift = 2**e
    mantissa_x, mantissa_y = old_sort.mantissa(x), old_sort.mantissa(y)
    mantissa_x, mantissa_y = ZeroExt(max_shift, mantissa_x), ZeroExt(max_shift, mantissa_y)
    mantissa_x, mantissa_y = mantissa_x << max_shift, mantissa_y << max_shift

    # shift the y mantissa to match the exponent of x
    exponent_diff = ZeroExt(mantissa_y.size() - exponent_diff.size(), exponent_diff)
    mantissa_y_shifted = LShR(mantissa_y, exponent_diff)

    overflow = Not(BVAddNoOverflow(mantissa_x, mantissa_y_shifted, False))
    exponent_result = If(overflow, old_sort.exponent(x) + 1, old_sort.exponent(x))

    mantissa_y_shifted = If(overflow, LShR(mantissa_y_shifted, 1), mantissa_y_shifted)
    mantissa_x = If(overflow, LShR(mantissa_x, 1), mantissa_x)

    mantissa_result = If(old_sort.sign(x) + old_sort.sign(y) == 1, 
                         mantissa_x - mantissa_y_shifted, 
                         mantissa_x + mantissa_y_shifted)
    
    # assumption: x > y
    sign_result = old_sort.sign(x)

    new_sort = FloatSort(mantissa_result.size(), e)
    return sign_result, mantissa_result, exponent_result, new_sort

def add(a : DatatypeRef, b : DatatypeRef, rounding_mode : DatatypeRef = Truncate) -> DatatypeRef:
    ensure_eq_sort(a, b)
    
    # unpack the floats and put the (absolute) bigger one into x
    sort = get_sort(a)
    x = If(gt(abs(a), abs(b)), a, b)
    y = If(gt(abs(a), abs(b)), b, a)
    case_x, x = unpack(x)
    case_y, y = unpack(y)

    result_case = If(Or(case_x == nan_case, case_y == nan_case),
                     nan_case,
                  If(And(case_x == inf_case, case_y == inf_case),
                     If(sort.sign(a) != sort.sign(b),
                        nan_case, # pos_inf + neg_inf
                        inf_case),
                  If(Or(case_x == inf_case, case_y == inf_case), 
                     inf_case,
                     unpacked_normal_case)))
    
    sign_result, mantissa_result, exponent_result, new_sort = __add_core(x, y)

    sign_result = If(neg(a) == b, # check if result is zero
                     If(And(is_zero(a), a==b), # both are zero and equal
                        sort.sign(a),
                        If(rounding_mode == Down,
                           BitVecVal(1, 1),   # negative
                           BitVecVal(0, 1))), # positive
                     sign_result) # result is not zero  

    return pack(FloatVar(sign_result, mantissa_result, exponent_result, new_sort), sort, rounding_mode, result_case)

def sub(a : DatatypeRef, b : DatatypeRef, rounding_mode : DatatypeRef = Truncate) -> DatatypeRef:
    return add(a, neg(b))

def __mul_core(a: DatatypeRef, b: DatatypeRef):
    s = get_sort(a)
    m, _ = sizes(s)

    a_mantissa = ZeroExt(m, s.mantissa(a))
    b_mantissa = ZeroExt(m, s.mantissa(b))
    mantissa_result = a_mantissa * b_mantissa
    exponent_result = s.exponent(a) + s.exponent(b) + 1

    result_sign = s.sign(a) ^ s.sign(b)
    new_sort = FloatSort(mantissa_result.size(), exponent_result.size())
    return result_sign, mantissa_result, exponent_result, new_sort 

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
    return pack(FloatVar(result_sign, mantissa_result, exponent_result, new_sort), result_sort, rounding_mode, result_case)

def __div_core(a : DatatypeRef, b : DatatypeRef):
    s = get_sort(a)
    m, _ = sizes(s)
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
    # and the rest are the remainder (bits for normalization & guard bit).
    padded_quotient = UDiv(a_mantissa, b_mantissa)
    # used to calculate the sticky bit
    padded_remainder = URem(a_mantissa, b_mantissa)

    quotient = Extract(padded_quotient.size()-1, 2*m+1, padded_quotient)
    remainder = Extract(2*m, 0, padded_quotient)
    sticky_bit = If(padded_remainder == 0, BitVecVal(0, 1), BitVecVal(1, 1))
    mantissa_result = Concat(quotient, remainder, sticky_bit)
    exponent_result = s.exponent(a) - s.exponent(b)

    result_sign = s.sign(a) ^ s.sign(b)
    return result_sign, mantissa_result, exponent_result

def div(a : DatatypeRef, b : DatatypeRef, rounding_mode : DatatypeRef = Truncate) -> DatatypeRef:
    ensure_eq_sort(a, b)
    result_sort = get_sort(a)

    case_a, a = unpack(a)
    case_b, b = unpack(b)
    m, _ = sizes(get_sort(a))

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
    return pack(FloatVar(sign, mantissa, exponent, new_sort), result_sort, rounding_mode, result_case)

def __int_div(a : DatatypeRef, b : DatatypeRef, result_sort : DatatypeSortRef):
    a_m, _ = sizes(get_sort(a))

    # first, we calculate a / b with its entire remainder.
    # then, we determine the amount of digits extra_leading_digits that we need to add so that
    # the result is an integer: 
    # min(m - leading_digits, exponent) either turns the entire mantissa into leading_digits,
    # or only uses exponent many digits if we cannot turn the entire mantissa into leading_digits
    # without reaching a negative exponent (for e = 0 we have exactly the exponent we want).
    # then, we extract the leading digits for the natural number and the remainder, and round the natural number
    # using the remainder. then, we use pack to normalize the result.
    sign, mantissa, exponent = __div_core(a, b)
    old_m = mantissa.size()
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
    float_leading_digits = a_m + (m - old_m)
    float_remainder_digits = BitVecVal(m - float_leading_digits, e)
    extra_leading_digits = If(float_remainder_digits < exponent, float_remainder_digits, exponent)
    exponent = exponent - extra_leading_digits
    # note that this value may be negative
    leading_digits = BitVecVal(float_leading_digits, e) + extra_leading_digits
    remainder_digits = BitVecVal(m, e) - leading_digits

    nat = LShR(mantissa, remainder_digits)
    remainder = If(leading_digits < 0, LShR(mantissa, -leading_digits), mantissa << leading_digits)

    nat, round_overflow = round(0, nat, remainder, NearestTiesToEven)
    internal_overflow = And(Not(round_overflow), nat == (BitVecVal(1, nat.size()) << leading_digits))
    nat = If(round_overflow, BitVecVal(1 << (nat.size() - 1), nat.size()),
          If(internal_overflow, BitVecVal(1, leading_digits.size()) << (leading_digits - 1), nat))
    exponent = exponent + If(Or(round_overflow, internal_overflow), 
                             BitVecVal(1, exponent.size()), 
                             BitVecVal(0, exponent.size()))

    # this is why we need the internal overflow check: if the nat is suddenly longer than leading_digits,
    # we'd lose the leading 1.
    nat = nat << remainder_digits
    exponent = exponent + leading_digits - 1

    # no rounding occurs here since we nullified the remainder
    nat = pack(FloatVar(sign, nat, exponent, FloatSort(m, e)), result_sort, NearestTiesToEven)
    return nat

def rem(a : DatatypeRef, b : DatatypeRef) -> DatatypeRef:
    ensure_eq_sort(a, b)
    result_sort = get_sort(a)
    m, e = sizes(result_sort)
    # we don't want __int_div to overflow. a division a / b may need double the space in the exponent.
    # if b is a subnormal, we also need enough space for m, hence in the worst case we need to
    # also guarantee space for 2**(e+1)+m.
    intermediate_sort = FloatSort(m, e + 1 + guaranteed_space(2**(e+1), m, False))
    old_a, old_b = a, b

    case_a, a = unpack(a)
    case_b, b = unpack(b)
    nat = __int_div(a, b, intermediate_sort)    

    larger_old_a = convert_float(old_a, intermediate_sort, Truncate)
    larger_old_b = convert_float(old_b, intermediate_sort, Truncate)
    r = fma(neg(larger_old_b), nat, larger_old_a, NearestTiesToEven)

    # no rounding should occur here, since the mantissa size does not change.
    # this line may however handle underflows/overflows.
    r = convert_float(r, result_sort, Truncate)

    r = If(Or(case_a == nan_case, 
              case_b == nan_case, 
              case_a == inf_case, 
              case_b == zero_case),
              FloatValNaN(result_sort),
        If(Or(case_a == zero_case, is_zero(r)), FloatVarZero(result_sort, result_sort.sign(old_a)),
        If(case_b == inf_case, old_a, r)))
    return r

def sqrt(a : DatatypeRef, rounding_mode : DatatypeRef = Truncate) -> DatatypeRef:
    # this function is work in progress and not fully correct yet.
    rm_it = NearestTiesAwayFromZero
    rm = rounding_mode

    old_sort = get_sort(a)
    m_old, e_old = sizes(old_sort)
    
    m, e = m_old*2, e_old
    extended_sort = FloatSort(m, e) # extend for more precision
    a_extended = convert_float(a, extended_sort, rounding_mode)

    exp = extended_sort.exponent(a_extended)
    man = extended_sort.mantissa(a_extended)
    sig = extended_sort.sign(a_extended)

    two = FloatVal(0, 0, 2**(e-1), extended_sort)
    one = FloatVal(0, 0, 2**(e-1)-1, extended_sort)
    half = FloatVal(0, 0, 2**(e-1)-2, extended_sort)

    e_bias = BitVecVal(2**(e-1) - 1, e)

    # initial guess
    y0 = div(one, FloatVar(sig, man, ((exp - e_bias)/ 2) + e_bias, extended_sort), rounding_mode)
    x0 = mul(a_extended, y0)
    h0 = div(y0, two, rm_it)
    
    x = x0
    h = h0
    r = fma(neg(x), h, half)
    for _ in range(0,int(math.sqrt(m)*2)):
        x = fma(x, r, x, Up)
        h = fma(h, r, h, Up)
        r = fma(neg(x), h, half, Up)
    solution = x

    # the first cond shouldn't be necessary for zero, 
    # but could speed up the cases. 
    # neg_zero is also specifically pointed out in the standard.
    solution = If(Or(is_zero(a_extended), is_nan(a_extended), is_pos_inf(a_extended)),
                  a_extended, # sqrt(-0) = -0 like in the standard, sqrt(nan) = nan, sqrt(+inf)=+inf
                  If(lt(a_extended, FloatValZero(extended_sort, 1)),
                     FloatValNaN(extended_sort),
                     solution))
    return convert_float(solution, old_sort, rm)

def fma(a, b, c, rounding_mode : DatatypeRef = Truncate) -> DatatypeRef:
    '''
    Performs the operation (a * b) + c, only rounding at the end of both.
    '''
    ensure_eq_sort(c, a)
    ensure_eq_sort(c, b)
    result_sort = get_sort(c)
    old_c = c

    case_c, c = unpack(c)
    case_a, a = unpack(a)
    case_b, b = unpack(b)

    unpack_sort = get_sort(c)
    unpack_m, _ = sizes(unpack_sort)

    # handle some special cases preemptively so we don't
    # accidentally lose that information during the operation.
    # this is for the multiplication only, addition comes below
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

    sign_mul, mantissa_mul, exponent_mul, mul_sort = __mul_core(a, b)
    mul_result = FloatVar(sign_mul, mantissa_mul, exponent_mul, mul_sort)
    m_mul, e_mul = sizes(mul_sort)

    size_dif = mantissa_mul.size() - unpack_m
    # append size_dif many zeros to the right
    mantissa_c_new = ZeroExt(size_dif, unpack_sort.mantissa(c)) << size_dif 
    extended_c = FloatVar(unpack_sort.sign(c), mantissa_c_new, unpack_sort.exponent(c), mul_sort)

    intermediate_result = pack(mul_result, result_sort, rounding_mode, result_case)
    
    # resolve troubles due to multiple operations being executed
    # -1 due to implicit bit, -2 to get back to original exponent size (revert unpack)
    mul_sort = FloatSort(m_mul-1, e_mul-2) 
    # Truncate due to no bits being cut off
    mul_result = pack(mul_result, mul_sort, Truncate, result_case) 
    extended_c = pack(extended_c, mul_sort, Truncate, case_c) 

    # ensure that the first operand is the bigger one
    x = If(gt(abs(intermediate_result), abs(old_c)), mul_result, extended_c)
    y = If(gt(abs(intermediate_result), abs(old_c)), extended_c, mul_result)

    extended_result = add(x, y, rounding_mode)
    result_case, result_unpacked = unpack(extended_result)
    result = pack(result_unpacked, result_sort, rounding_mode, result_case)

    # for the rare case that the multiplication overflows and the addition being with the opposite inf
    result = If(And(is_inf(intermediate_result), 
                    Not(case_a == inf_case),
                    Not(case_b == inf_case),
                    case_c == inf_case),
                old_c,
                result)
    return result
    
def min_float(a : DatatypeRef, b : DatatypeRef) -> DatatypeRef:
    return If(is_nan(a), b, 
           If(is_nan(b), a,
           If(And(is_neg_zero(a), is_pos_zero(b)), a,
           If(And(is_pos_zero(a), is_neg_zero(b)), b,
           If(lte(a, b), a, b)))))

def max_float(a : DatatypeRef, b : DatatypeRef) -> DatatypeRef:
    return neg(min_float(neg(a), neg(b)))