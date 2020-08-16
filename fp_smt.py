from float import *

rm = Truncate #default value

def set_default_rm(rounding_mode: RoundingMode):
    rm = rounding_mode

class SMTFloat:
    def __init__(self, expr):
        self.expr = expr
    
    #Operations: 
    def __add__(self, other) -> SMTFloat:
        return SMTFloat(add(self.expr, other.expr, rm))
        
    def __sub__(self, other) -> SMTFloat:
        return SMTFloat(sub(self.expr, other.expr, rm))

    def __mul__(self, other) -> SMTFloat:
        return SMTFloat(mul(self.expr, other.expr, rm))

    def __truediv__(self, other) -> SMTFloat:
        return SMTFloat(div(self.expr, other.expr, rm))

    def _floordiv__(self, other):
        pass #TODO

    def __mod__(self, other) -> SMTFloat:
        return SMTFloat(rem(self.expr, other.expr))

    def __pow__(self, other):
        pass #TODO: liesse sich eventuell einfach implementieren, solange other ein python int is

    #Comparisons:
    def __lt__(self, other) -> BoolRef:
        return lt(self.expr, other.expr)

    def __gt__(self, other) -> BoolRef:
        return gt(self.expr, other.expr)

    def __le__(self, other) -> BoolRef:
        return lte(self.expr, other.expr)

    def __ge__(self, other) -> BoolRef:
        return gte(self.expr, other.expr)

    def __eq__(self, other) -> BoolRef:
        return eq_float(self.expr, other.expr)

    def __ne__(self, other) -> BoolRef:
        return Not(eq_float(self.expr, other.expr))

    # Assignment ops:
    def __isub__(self, other):
        self.expr = sub(self.expr, other.expr, rm)

    def __iadd__(self, other):
        self.expr = add(self.expr, other.expr, rm)

    def __imul__(self, other):
        self.expr = mul(self.expr, other.expr, rm)

    def __idiv__(self, other):
        self.expr = div(self.expr, other.expr, rm)

    def __ifloordiv__(self, other):
        pass #TODO

    def __imod__(self, other):
        self.expr = rem(self.expr, other.expr, rm)
    
    def __ipow__(self, other):
        pass #TODO

    #Unary:
    def __neg__(self) -> SMTFloat:
        return SMTFloat(neg(self.expr))

    def __pos__(self) -> SMTFloat:
        return SMTFloat(self.expr)

    def __invert__(self):
        pass # TODO

    def __abs__(self) -> SMTFloat:
        return SMTFloat(abs(self.expr))

    #Misc:
    def __str__(self) -> String:
        return str(simplify(self.expr))

    def __repr__(self):
        return super().__repr__()


    # Non-Magic Methods:
    def size(self) -> (int, int):
        return sizes(self.expr)


#-----------------------------------------------------------------------------------------------------------------
    def get_sort(self) -> DatatypeSortRef:
        return get_sort(self.expr)

    def to_ieee_bv(self) -> BitVecNumRef:
        return to_ieee_bv(self.expr)

    def is_pos_zero(self) -> BoolRef:
        return is_pos_zero(self.expr)

    def is_neg_zero(self) -> BoolRef:
        return is_neg_zero(self.expr)

    def is_zero(self) -> BoolRef:
        return is_zero(self.expr)

    def is_inf(self) -> BoolRef:
        return is_inf(self.expr)

    def is_pos_inf(self) -> BoolRef:
        return is_pos_inf(self.expr)

    def is_neg_inf(self) -> BoolRef: 
        return is_neg_inf(self.expr)

    def is_nan(self, nan_value : int = 0) -> BoolRef: 
        return is_nan(self.expr, nan_value)

    def is_subnormal(self) -> BoolRef:
        return is_subnormal(self.expr)

    def is_normal(self) -> BoolRef:
        return is_normal(self.expr)

    @classmethod
    def eq_float(cls, a : SMTFloat, b : SMTFloat) -> BoolRef:
        return eq_float(a.expr, b.expr)

    @classmethod
    def gt(cls, a : SMTFloat, b : SMTFloat) -> BoolRef:
        return gt(a.expr, b.expr)

    @classmethod
    def lt(cls, a : SMTFloat, b : SMTFloat) -> BoolRef:
        return lt(b.expr, a.expr)

    @classmethod
    def gte(cls, a : SMTFloat, b : SMTFloat) -> BoolRef:
        return gte(a.expr, b.expr)

    @classmethod
    def lte(cls, a : SMTFloat, b : SMTFloat) -> BoolRef:
        return lte(a.expr, b.expr)
#--------------------------------
    def abs(self : SMTFloat) -> SMTFloat:
        return SMTFloat(abs(self.expr))

    def neg(self : SMTFloat) -> SMTFloat:
        return SMTFloat(neg(self.expr))

    @classmethod
    def eq_bitwise(cls, a : SMTFloat, b : SMTFloat) -> BoolRef:
        return eq_bitwise(a.expr, b.expr)
    
    @classmethod
    def add(cls, a : SMTFloat, b : SMTFloat, rounding_mode : DatatypeRef = rm) -> SMTFloat:
        return cls(add(a.expr, b.expr, rounding_mode))

    @classmethod
    def sub(cls, a : SMTFloat, b : SMTFloat, rounding_mode : DatatypeRef = rm) -> SMTFloat:
        return cls(sub(a.expr, b.expr, rounding_mode))
 
    @classmethod
    def mul(cls, a : SMTFloat, b : SMTFloat, rounding_mode : DatatypeRef = rm) -> SMTFloat:
        return cls(mul(a.expr, b.expr, rounding_mode))

    @classmethod
    def div(cls, a : SMTFloat, b : SMTFloat, rounding_mode : DatatypeRef = rm) -> SMTFloat:
        return cls(div(a.expr, b.expr, rounding_mode))

    @classmethod
    def rem(cls, a : SMTFloat, b : SMTFloat) -> SMTFloat:
        return cls(rem(a.expr, b.expr))
    
    def sqrt(self : SMTFloat, rounding_mode : DatatypeRef = rm) -> SMTFloat:
        return SMTFloat(sqrt(self.expr, rounding_mode))

    @classmethod
    def fma(cls, a : SMTFloat, b : SMTFloat, c : SMTFloat, rounding_mode : DatatypeRef = rm) -> SMTFloat:
        return cls(fma(a.expr, b.expr, c.expr, rounding_mode))

    @classmethod   
    def min_float(cls, a : SMTFloat, b : SMTFloat) -> SMTFloat:
        return cls(min_float(a.expr, b.expr))

    @classmethod
    def max_float(cls, a : SMTFloat, b : SMTFloat) -> SMTFloat:
        return cls(max_float(a.expr, b.expr))

    #Conversions:
    def to_z3FP(self) -> FPRef:
        return Float_to_z3FP(self.expr)

    @classmethod
    def z3FP_to_SMTFloat(cls, x : FPRef) -> SMTFloat:
        return cls(z3FP_to_Float(x))

    def convert_float(self : SMTFloat, new_sort : DatatypeSortRef, rounding_mode : RoundingMode = rm) -> SMTFloat:
        return SMTFloat(convert_float(self.expr, new_sort, rounding_mode))

    ##Generators:
    @classmethod
    def FloatSort(cls, mantissa_size : int, exponent_size : int) -> DatatypeSortRef: 
        return FloatSort(mantissa_size, exponent_size)

    @classmethod
    def FloatConst(cls, name : str, mantissa_size : int, exponent_size : int) -> SMTFloat:
        return cls(FloatConst(name, mantissa_size, exponent_size))

    @classmethod
    def FloatVar(cls, sign : BitVecRef, mantissa : BitVecRef, exponent : BitVecRef, sort : DatatypeSortRef) -> SMTFloat:
        return cls(FloatVar(sign, mantissa, exponent, sort))

    @classmethod
    def FloatVal(cls, sign : int, mantissa : int, exponent : int, sort : DatatypeSortRef) -> SMTFloat:
        return cls(FloatVal(sign, mantissa, exponent, sort))

    @classmethod
    def FloatValDec(cls, dec_val : str, rounding_mode : converter.RoundingMode, sort : DatatypeSortRef) -> SMTFloat:
        return cls(FloatVal(dec_val, rounding_mode, sort))

    @classmethod
    def FloatValBV(cls, bv : BitVecRef, sort : DatatypeSortRef) -> SMTFloat:
        return cls(FloatValBV(bv, sort))

    @classmethod
    def FloatValPosInf(cls, sort : DatatypeSortRef) -> SMTFloat:
        return cls(FloatValPosInf(sort))

    @classmethod
    def FloatValNegInf(cls, sort : DatatypeSortRef) -> SMTFloat:
        return cls(FloatValNegInf(sort))

    @classmethod
    def FloatValNaN(cls, sort : DatatypeSortRef, value = 1) -> SMTFloat:
        return cls(FloatValNaN(sort, value))

    @classmethod
    def FloatValZero(cls, sort : DatatypeSortRef, sign = 0) -> SMTFloat:
        return cls(FloatValZero(sort, sign))

