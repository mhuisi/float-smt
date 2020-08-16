from typing import NamedTuple, Callable
from dataclasses import dataclass
from enum import Enum

class ParseState(NamedTuple):
    s : str # string
    i : int # index

@dataclass
class DecFloat(): # sign * mantissa * 10^exponent
    # we need to track the sign to be able to distinguish +0 and -0
    s : int # sign
    m : int # mantissa
    e : int # exponent

def expect(s : ParseState, f : Callable[[ParseState], bool]):
    if not f(s):
        raise ValueError("unexpected input")

def has_next(s : ParseState) -> bool: 
    return s.i < len(s.s)

def next(s : ParseState) -> ParseState: 
    expect(s, has_next)
    return ParseState(s.s, s.i+1)

def peek(s : ParseState) -> str: 
    expect(s, has_next)
    return s.s[s.i]

def is_char(s : ParseState, ch : str) -> bool:
    return has_next(s) and peek(s) == ch

def is_digit(s : ParseState) -> bool:
    return has_next(s) and peek(s).isdigit()

def skip_whitespace(s : ParseState) -> ParseState:
    while has_next(s) and peek(s).isspace():
        s = next(s)
    return s

def parse_digits(s : ParseState) -> (ParseState, int, int):
    v = 0
    last_non_zero = 0
    n = 0
    expect(s, is_digit)
    while has_next(s) and is_digit(s):
        v = 10*v + int(peek(s))
        n += 1
        if peek(s) != '0':
            last_non_zero = n
        s = next(s)
    return (s, v, last_non_zero)

def parse_float(s : str) -> DecFloat:
    s = ParseState(s, 0)
    s = skip_whitespace(s)
    sign = 1
    if is_char(s, '+'):
        s = next(s)
    elif is_char(s, '-'):
        sign = -1
        s = next(s)
    mantissa = 0
    exponent = 0
    if is_char(s, '.'):
        s = next(s)
        s, mantissa, n_digits = parse_digits(s)
        exponent = -n_digits
    else:
        s, mantissa, _ = parse_digits(s)
        if is_char(s, '.'):
            s = next(s)
        if is_digit(s):
            s, decimals, n_digits = parse_digits(s)
            mantissa = mantissa*10**n_digits + decimals
            exponent = -n_digits
    if is_char(s, "e") or is_char(s, "E"):
        s = next(s)
        exp_sign = 1
        if is_char(s, '+'):
            s = next(s)
        elif is_char(s, '-'):
            exp_sign = -1
            s = next(s)
        s, e, _ = parse_digits(s)
        exponent += exp_sign*e
    expect(s, lambda s: not has_next(s))
    return DecFloat(sign, mantissa, exponent)

class RoundingMode(Enum):
    NEAREST_TIE_TO_EVEN = 1
    NEAREST_TIE_AWAY_FROM_ZERO = 2
    UP = 3
    DOWN = 4
    TRUNCATE = 5 

class Float(NamedTuple):
    s : str # sign
    m : str # mantissa
    e : str # exponent

def convert(s : str, r : RoundingMode, m_size : int, e_size : int) -> Float:
    d = parse_float(s)
    sign = '0' if d.s == 1 else '1'
    if d.m == 0:
        return Float(sign, m_size*"0", e_size*"0")
    # normalize the dec float
    while d.m % 10 == 0:
        d.e += 1
        d.m /= 10
    # create a fraction a / b
    a, b = (d.m, 10**(-d.e)) if d.e < 0 else (d.m * 10**d.e, 1)
    # scale a // b into [2**m_size, 2**(m_size+1))
    scale = 0
    # scale smaller numbers up
    while a // b < 2**m_size:
        a *= 2
        scale -= 1
        # check if we've reached subnormal territory
        if 2**(e_size-1)-1 + m_size + scale <= 0:
            a //= 2
            scale += 1
            break
    # scale bigger numbers down
    while a // b >= 2**(m_size+1):
        b *= 2
        scale += 1
    # use remainder of a / b to round accordingly
    sig, rem = a // b, a % b
    if (r == RoundingMode.NEAREST_TIE_TO_EVEN and (rem > b//2 or rem == b//2 and (sig + 1) % 2 == 0) or
        r == RoundingMode.NEAREST_TIE_AWAY_FROM_ZERO and rem >= b//2 or
        r == RoundingMode.UP and rem > 0 and d.s == 1 or
        r == RoundingMode.DOWN and rem > 0 and d.s == -1):
        sig += 1
    # if we rounded to 2**(m_size+1), adjust sig and scale
    if sig == 2**(m_size+1):
        sig = 2**m_size
        scale += 1
    mantissa = ("{0:0%db}" % m_size).format(int(sig))
    # subnormal
    if len(mantissa) <= m_size:
        return Float(sign, mantissa, e_size*"0")
    # discard leading 1 in normalized number
    mantissa = mantissa[1:]
    # account for bias
    exp = ("{0:0%db}" % e_size).format(2**(e_size-1)-1 + m_size + scale)
    # infinity
    if exp == e_size*"1" or len(exp) > e_size:
        return Float(sign, m_size*"0", e_size*"1")
    return Float(sign, mantissa, exp)
