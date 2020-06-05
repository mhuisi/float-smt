from pyboolector import *
from typing import Enum

# IEEE rounding modes
class RoundingMode(Enum):
    # Round to nearest float and on tie to the nearest even number
    NEAREST_TIE_TO_EVEN = 1
    # Round to nearest float and on tie away from zero
    NEAREST_TIE_AWAY_FROM_ZERO = 2
    # Round towards +inf
    UP = 3
    # Round towards -inf
    DOWN = 4
    # Round towards zero
    TRUNCATE = 5 

# IEEE float precisions (mantissa bit width, exponent bit width)
class Precision(Enum):
    SINGLE = (23, 8)
    DOUBLE = (52, 11)
    EXTENDED = (64, 15)

'''
Zeitplan:
       - 09.06.: Float sort & Konstruktoren
09.06  - 16.06.: to_ieee und to_string Konversionen
16.06. - 23.06.: is_* und Vergleichsoperationen
23.06. - 14.07.: Arithmetische Operationen & Overflow checks
14.07. - 21.07 (28.07.): mk_float_bin, tests, beispielimpls, round_to_integral, to_* 
'''

#------------------------------------------------------------------------------------------------
#---------------------------------------------Sorts----------------------------------------------
#------------------------------------------------------------------------------------------------

# 1
# Create a floating point sort of type precision with the rounding mode rounding_mode
def float_sort(precision : Precision, rounding_mode : RoundingMode) -> BoolectorSort: pass


#------------------------------------------------------------------------------------------------
#----------------------------------------Initializations-----------------------------------------
#------------------------------------------------------------------------------------------------

# 2
# Initialize a float constant given a bitvector
def mk_float_bv(bv : pyboolector.BoolectorNode, sort : BoolectorSort) -> BoolectorNode: pass

# 2
# Initialize a float constant given a decimal value in scientific notation as a string
def mk_float_dec(dec_value : str, sort : BoolectorSort) -> BoolectorNode: pass

# --
# Initialize a float constant given a binary value in scientific notation as a string
def mk_float_bin(bin_value : str, sort : BoolectorSort) -> BoolectorNode: pass

# 2
# Initialize the float constant positive Infinte
def mk_float_pos_inf(sort : BoolectorSort) -> BoolectorNode: pass

# 2
# Initialize the float constant negative Infinte
def mk_float_neg_inf(sort : BoolectorSort) -> BoolectorNode: pass

# 2
# Initialize the float constant `not a number` using the standard value
# Optional: use a specific error value
def mk_float_nan(sort : BoolectorSort, nan_value : int = 0) -> BoolectorNode: pass


#------------------------------------------------------------------------------------------------
#----------------------------------------Operations----------------------------------------------
#------------------------------------------------------------------------------------------------

# 6
# Sets the sign bit to 0
def abs(a : BoolectorNode) -> BoolectorNode: pass

# Inverts the sign bit
def neg(a : BoolectorNode) -> BoolectorNode: pass

# Adds the floating point values a & b
def add(a : BoolectorNode, b : BoolectorNode) -> BoolectorNode: pass

# Subtracts b from a
def sub(a : BoolectorNode, b : BoolectorNode) -> BoolectorNode: pass

# Multiplies a with b
def mul(a : BoolectorNode, b : BoolectorNode) -> BoolectorNode: pass

# Divides a by b
def div(a : BoolectorNode, b : BoolectorNode) -> BoolectorNode: pass

# Performs the operation a modulo b
def rem(a : BoolectorNode, b : BoolectorNode) -> BoolectorNode: pass

# Performs the square-root operation on a node a
def sqrt(a : BoolectorNode) -> BoolectorNode: pass

# Performs the operation a + (b * c)
def fma(a : BoolectorNode, b : BoolectorNode, c : BoolectorNode) -> BoolectorNode: pass

# Returns a node containing a if a <= b and b else
def min(a : BoolectorNode, b : BoolectorNode) -> BoolectorNode: pass

# Returns a node containing a if a >= b and b else
def max(a : BoolectorNode, b : BoolectorNode) -> BoolectorNode: pass




#------------------------------------------------------------------------------------------------
#----------------------------------------Rounding----------------------------------------------
#------------------------------------------------------------------------------------------------

# --
# Rounds a float to the next float that represents an integer value
def round_to_integral(a : BoolectorNode) -> BoolectorNode: pass




#------------------------------------------------------------------------------------------------
#----------------------------------------Conversion----------------------------------------------
#------------------------------------------------------------------------------------------------

# --
# Converts a float to a different precision and a different rounding mode
def to_fp(a : BoolectorNode, b : BoolectorSort) -> BoolectorNode: pass

# 3
# Creates a string representation of a floating point value
def to_string(a : BoolectorNode) -> str: pass

# --
# Rounds a float to the next integer and creates an unsigned bitvector that represents that integer
def to_ubv(a : BoolectorNode) -> BoolectorNode: pass

# --
# Rounds a float to the next integer and creates an unsigned bitvector that represents that integer
def to_sbv(a : BoolectorNode) -> BoolectorNode: pass

# 3
# Converts a float to its IEEE bitvector representation
def to_ieee_bv(a : BoolectorNode) -> BoolectorNode: pass



#------------------------------------------------------------------------------------------------
#----------------------------------------Under/Overflow------------------------------------------
#------------------------------------------------------------------------------------------------

# The idea is that every function that can underflow or overflow gets 
# its own check which returns a boolean boolector node indicating 
# if the operation would cause an under/overflow.
# 6
def is_add_underflow(a : BoolectorNode, b : BoolectorNode) -> BoolectorNode: pass
def is_add_overflow(a : BoolectorNode, b : BoolectorNode) -> BoolectorNode: pass

def is_sub_underflow(a : BoolectorNode, b : BoolectorNode) -> BoolectorNode: pass
def is_sub_overflow(a : BoolectorNode, b : BoolectorNode) -> BoolectorNode: pass

def is_mul_underflow(a : BoolectorNode, b : BoolectorNode) -> BoolectorNode: pass
def is_mul_overflow(a : BoolectorNode, b : BoolectorNode) -> BoolectorNode: pass

def is_div_underflow(a : BoolectorNode, b : BoolectorNode) -> BoolectorNode: pass
def is_div_overflow(a : BoolectorNode, b : BoolectorNode) -> BoolectorNode: pass

def is_sqrt_underflow(a : BoolectorNode, b : BoolectorNode) -> BoolectorNode: pass

def is_fma_underflow(a : BoolectorNode, b : BoolectorNode) -> BoolectorNode: pass
def is_fma_overflow(a : BoolectorNode, b : BoolectorNode) -> BoolectorNode: pass




#------------------------------------------------------------------------------------------------
#----------------------------------------Relations-----------------------------------------------
#------------------------------------------------------------------------------------------------

# 5
# Checks whether two floats are equal as floating point numbers
def eq(a : BoolectorNode, b : BoolectorNode) -> BoolectorNode: pass

# Checks whether a is greater than b
def gt(a : BoolectorNode, b : BoolectorNode) -> BoolectorNode: pass

# Checks whether a is greater than or equal to b
def gte(a : BoolectorNode, b : BoolectorNode) -> BoolectorNode: pass

# Checks whether a is less than b
def lt(a : BoolectorNode, b : BoolectorNode) -> BoolectorNode: pass

# Checks whether a is less than or equal to b
def lte(a : BoolectorNode, b : BoolectorNode) -> BoolectorNode: pass



#------------------------------------------------------------------------------------------------
#--------------------------------------------Misc------------------------------------------------
#------------------------------------------------------------------------------------------------

# 4
# Checks whether a is +0 or -0
def is_zero(a : BoolectorNode) -> BoolectorNode: pass

# Checks whether a is +0
def is_pos_zero(a : BoolectorNode) -> BoolectorNode: pass

# Checks whether a is -0
def is_neg_zero(a : BoolectorNode) -> BoolectorNode: pass

# Checks whether a is +inf or -inf
def is_inf(a : BoolectorNode) -> BoolectorNode: pass

# Checks whether a is +inf
def is_pos_inf(a : BoolectorNode) -> BoolectorNode: pass

# Checks whether a is +inf
def is_neg_inf(a : BoolectorNode) -> BoolectorNode: pass

# Checks whether a is a NaN value (and optionally whether it is a specific NaN value)
def is_nan(a : BoolectorNode, nan_value : int = 0) -> BoolectorNode: pass

# Checks whether a is a normal float
def is_normal(a : BoolectorNode) -> BoolectorNode: pass