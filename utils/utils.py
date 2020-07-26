# Contains some utility functions
from z3 import *

def clz(v: BitVecRef) -> IntNumRef:
    #TODO: think about substituting this with a binary search instead of linear search
    s = v.size()
    zero = BitVecVal(0, s.bit_length())
    one = BitVecVal(1, s.bit_length())
    d = zero
    # count leading zeros by shifting bits away to the right
    # until the result of shifting itself is zero.
    # then, count the amount of bits for which the shifting result is zero.
    # e.g. 00100 != 0, 0010 != 0, 001 != 0, 00 == 0, 0 == 0 => 2 leading zeroes 
    for i in range(s):
        d = d + If(LShR(v, i) == 0, one, zero)
    return d