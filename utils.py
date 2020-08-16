# Contains some utility functions
from z3 import *
from math import floor, log2

def clz(v: BitVecRef) -> IntNumRef:
    s = v.size()
    zero = BitVecVal(0, s)
    one = BitVecVal(1, s)
    d = zero
    # count leading zeros by shifting bits away to the right
    # until the result of shifting itself is zero.
    # then, count the amount of bits for which the shifting result is zero.
    # e.g. 00100 != 0, 0010 != 0, 001 != 0, 00 == 0, 0 == 0 => 2 leading zeroes 
    for i in range(s):
        d = d + If(LShR(v, i) == 0, one, zero)
    return d

def matched_sizes(xs):
    max_size = max(x.size() for x in xs)
    return [max_size - x.size() for x in xs]

def match_sizes(xs):
    s = zip([x for x, _ in xs], matched_sizes([x for x, _ in xs]), [signed for _, signed in xs])
    return [SignExt(n, x) if signed else ZeroExt(n, x) for x, n, signed in s]

def guaranteed_space(n, added, signed, offset=0):
    # in order for 2**n + added to not overflow, we need
    # 2**(n + k) - 2**n > added <=> 2**n*(2**k - 1) > added <=> k > log2(added/2**n + 1)
    return floor(log2(added/2**n + 1)) + 1 + offset

def guarantee_space(value, added, signed, offset=0):
    '''
    Ensures that value + added cannot overflow.
    '''
    n = value.size()-1 if signed else value.size()
    added_bits = guaranteed_space(n, added, signed, offset)
    value = SignExt(added_bits, value) if signed else ZeroExt(added_bits, value)
    return value