# Contains some utility functions
from z3 import *

def clz(v: BitVecRef) -> IntNumRef:
    #TODO: think about substituting this with a binary search instead of linear search
    s = v.size()
    zero = BitVecVal(0, s.bit_length())
    one = BitVecVal(1, s.bit_length())
    d = zero
    for i in range(s):
        d = d + If(LShR(v, i) == 0, one, zero)
    return d