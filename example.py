from floatsmt.api import *

# Sets the rounding mode globally.
# Must be one of {NearestTiesToEven, NearestTiesAwayFromZero, Up, Down, Truncate}.
set_default_rm(NearestTiesToEven)

# Create the floating point sort which we will use.
# floatsmt supports arbitrary bit widths.
# The first argument denotes the mantissa width,
# the second argument denotes the exponent width.
# (23, 8) is a standard 32 bit float.
sort = FloatSort(23, 8)

# Create a float constant from a specific bit representation.
x = SMTFloat.FloatVal(0, 0b1011, 0b1111, sort)
# Create a float constant from a decimal representation. 
# The mode for rounding from decimal to binary must be one of
# {NEAREST_TIE_TO_EVEN, NEAREST_TIE_AWAY_FROM_ZERO, UP, DOWN, TRUNCATE}.
y = SMTFloat.FloatValDec("0.123e-2", converter.RoundingMode.NEAREST_TIE_TO_EVEN, sort)

# Many operations can be used via Python operator overloading.
print(x)
print(x + y)
print(x * x)
print(x - y) 
print(x / y)

# Constants can be created via FloatConst(name, mantissa_width, exponent_width)
# and then used for proofs.
x = SMTFloat.FloatConst("x", 23, 8)
y = SMTFloat.FloatConst("y", 23, 8)

# E.g. in minifloats always x > 0 & y > 0 => x * y >= 0
zero = SMTFloat.FloatValZero(FloatSort(23, 8))
solver = Solver()
condition = Implies(And(x > zero, y > zero), x * y >= zero)
solver.add(Not(condition))
print(solver.check())