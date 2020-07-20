import unittest
from float import *

class Float(unittest.TestCase):
    def test_float_field_sizes(self):
        mantissa_size, exponent_size = 2, 3
        sort = FloatSort(mantissa_size, exponent_size)
        self.assertEqual(mantissa_size, sizes(sort)[0])
        self.assertEqual(exponent_size, sizes(sort)[1])

class is_functions(unittest.TestCase):
    __mantissa_size, __exponent_size = 2, 3
    __sort = FloatSort(__mantissa_size,__exponent_size)
    __pos_zero = FloatVal(0, 0, 0, __sort)
    __neg_zero = FloatVal(1, 0, 0, __sort) 
    __not_zero = FloatVal(1, 1, 1, __sort) 
    __pos_inf = FloatVal(0, 0, 7, __sort)
    __neg_inf = FloatVal(1, 0, 7, __sort)
    __not_inf = FloatVal(1, 1, 1, __sort) 
    __nan1 = FloatVal(0, 1, 7, __sort) # nan with nan value 1
    __subnormal = FloatVal(0, 1, 0, __sort)
    __normal = FloatVal(0, 1, 1, __sort)

    def true(self, predicate_expr):
        self.assertTrue(simplify(predicate_expr))

    def false(self, predicate_expr):
        self.assertFalse(simplify(predicate_expr))

    def test_is_pos_zero(self):
        z = If(BitVecVal(1,1) == BitVecVal(0,1), self.__normal, self.__pos_zero)
        self.true(is_pos_zero(z)) #To test if it works with conditions too

        self.true(is_pos_zero(self.__pos_zero))
        self.false(is_pos_zero(self.__neg_zero))
        self.false(is_pos_zero(self.__not_zero))
        self.false(is_pos_zero(self.__pos_inf))
        self.false(is_pos_zero(self.__neg_inf))
        self.false(is_pos_zero(self.__subnormal))
        self.false(is_pos_zero(self.__nan1))

    def test_is_neg_zero(self):
        self.true(is_neg_zero(self.__neg_zero))
        self.false(is_neg_zero(self.__pos_zero))
        self.false(is_neg_zero(self.__not_zero))
        self.false(is_neg_zero(self.__pos_inf))
        self.false(is_neg_zero(self.__neg_inf))
        self.false(is_neg_zero(self.__subnormal))
        self.false(is_neg_zero(self.__nan1))

    def test_is_zero(self):
        self.true(is_zero(self.__pos_zero))
        self.true(is_zero(self.__neg_zero))
        self.false(is_zero(self.__not_zero))
        self.false(is_zero(self.__pos_inf))
        self.false(is_zero(self.__neg_inf))
        self.false(is_zero(self.__subnormal))
        self.false(is_zero(self.__nan1))

    def test_is_inf(self):
        self.true(is_inf(self.__pos_inf))
        self.true(is_inf(self.__neg_inf))
        self.false(is_inf(self.__neg_zero))
        self.false(is_inf(self.__pos_zero))
        self.false(is_inf(self.__not_inf))
        self.false(is_inf(self.__subnormal))
        self.false(is_inf(self.__nan1))

    def test_is_neg_inf(self):
        self.true(is_neg_inf(self.__neg_inf))
        self.false(is_neg_inf(self.__pos_inf))
        self.false(is_neg_inf(self.__neg_zero))
        self.false(is_neg_inf(self.__pos_zero))
        self.false(is_neg_inf(self.__not_inf))
        self.false(is_neg_inf(self.__subnormal))
        self.false(is_neg_inf(self.__nan1))

    def test_is_pos_inf(self):
        self.true(is_pos_inf(self.__pos_inf))
        self.false(is_pos_inf(self.__neg_inf))
        self.false(is_pos_inf(self.__neg_zero))
        self.false(is_pos_inf(self.__pos_zero))
        self.false(is_pos_inf(self.__not_inf))
        self.false(is_pos_inf(self.__subnormal))
        self.false(is_pos_inf(self.__nan1))
    
    def test_is_nan(self):
        self.true(is_nan(self.__nan1))
        self.true(is_nan(self.__nan1, 1)) # with nan value check
        self.false(is_nan(self.__nan1, 2)) # wrong nan value
        self.false(is_nan(self.__pos_inf))
        self.false(is_nan(self.__neg_inf))
        self.false(is_nan(self.__neg_zero))
        self.false(is_nan(self.__pos_zero))
        self.false(is_nan(self.__not_inf))
        self.false(is_nan(self.__subnormal))

    def test_is_subnormal(self):
        self.true(is_subnormal(self.__subnormal))
        self.false(is_subnormal(self.__pos_zero))
        self.false(is_subnormal(self.__neg_zero))
        self.false(is_subnormal(self.__nan1))
        self.false(is_subnormal(self.__pos_inf))
        self.false(is_subnormal(self.__neg_inf))
        self.false(is_subnormal(self.__normal))

    def test_is_normal(self):
        self.true(is_normal(self.__normal)) 
        self.false(is_normal(self.__subnormal))
        self.false(is_normal(self.__pos_inf))
        self.false(is_normal(self.__neg_inf))
        self.false(is_normal(self.__neg_zero))
        self.false(is_normal(self.__pos_zero))
        self.false(is_normal(self.__nan1))

class ComparisonOps(unittest.TestCase):
    __mantissa_size, __exponent_size = 2, 3
    __sort = FloatSort(__mantissa_size,__exponent_size)
    __pos_zero = FloatVal(0, 0, 0, __sort)
    __neg_zero = FloatVal(1, 0, 0, __sort) 
    __pos_inf = FloatVal(0, 0, 7, __sort)
    __neg_inf = FloatVal(1, 0, 7, __sort)
    __nan1 = FloatVal(0, 1, 7, __sort) # nan with nan value 1
    __subnormal1 = FloatVal(0, 1, 0, __sort)
    __subnormal2 = FloatVal(0, 2, 0, __sort)
    __normal1 = FloatVal(0, 1, 1, __sort)
    __normal2 = FloatVal(0, 1, 2, __sort)
    __normal3 = FloatVal(1, 1, 1, __sort)
    __normal4 = FloatVal(1, 1, 2, __sort)
    __normal5 = FloatVal(0, 0, 1, __sort)
    __normal6 = FloatVal(1, 0, 1, __sort)

    def true(self, predicate_expr):
        self.assertTrue(simplify(predicate_expr))

    def false(self, predicate_expr):
        self.assertFalse(simplify(predicate_expr))

    def test_eq(self):
        self.true(eq_float(self.__pos_zero, self.__neg_zero))
        self.true(eq_float(self.__pos_inf, self.__pos_inf))
        self.true(eq_float(self.__subnormal1, self.__subnormal1))
        self.true(eq_float(self.__normal1, self.__normal1))
        self.false(eq_float(self.__nan1, self.__nan1))
        self.false(eq_float(self.__subnormal1, self.__normal1))
        self.false(eq_float(self.__pos_inf, self.__neg_inf))
        self.false(eq_float(self.__nan1, self.__normal1))

    def test_gt(self):
        self.true(gt(self.__normal2, self.__normal1))
        self.true(gt(self.__normal3, self.__normal4))
        self.true(gt(self.__normal1, self.__normal5))
        self.true(gt(self.__normal6, self.__normal3))
        self.true(gt(self.__pos_inf, self.__pos_zero))
        self.true(gt(self.__pos_inf, self.__neg_zero))
        self.true(gt(self.__pos_zero, self.__neg_inf))
        self.true(gt(self.__neg_zero, self.__neg_inf))
        self.true(gt(self.__subnormal2, self.__subnormal1))
        self.true(gt(self.__normal5, self.__subnormal2))
        self.false(gt(self.__normal1, self.__normal1))
        self.false(gt(self.__pos_zero, self.__neg_zero))
        self.false(gt(self.__pos_inf, self.__nan1))
        self.false(gt(self.__nan1, self.__neg_inf))
        self.false(gt(self.__normal1, self.__normal2))

class Operations(unittest.TestCase):
    __mantissa_size, __exponent_size = 2, 3
    __sort = FloatSort(__mantissa_size,__exponent_size)
    __pos_zero = FloatVal(0, 0, 0, __sort)
    __neg_zero = FloatVal(1, 0, 0, __sort) 
    __pos_inf = FloatVal(0, 0, 7, __sort)
    __neg_inf = FloatVal(1, 0, 7, __sort)
    __nan1 = FloatVal(0, 1, 7, __sort) # nan with nan value 1
    __subnormal1 = FloatVal(0, 1, 0, __sort)
    __subnormal2 = FloatVal(0, 2, 0, __sort)
    __normal1 = FloatVal(0, 1, 1, __sort)
    __neg_normal1 = FloatVal(1, 1, 1, __sort)
    __normal2 = FloatVal(0, 1, 2, __sort)
    __normal3 = FloatVal(1, 1, 1, __sort)
    __normal4 = FloatVal(1, 1, 2, __sort)
    __normal5 = FloatVal(0, 0, 1, __sort)
    __normal6 = FloatVal(1, 0, 1, __sort)

    def true(self, predicate_expr):
        self.assertTrue(simplify(predicate_expr))

    def false(self, predicate_expr):
        self.assertFalse(simplify(predicate_expr))

    def test_abs(self):
        self.true(gte(abs(self.__subnormal1), self.__pos_zero))
        self.true(gte(abs(self.__subnormal2), self.__pos_zero))
        self.true(gte(abs(self.__neg_inf), self.__pos_zero))
        self.true(gte(abs(self.__pos_inf), self.__pos_zero))
        self.true(gte(abs(self.__normal1), self.__pos_zero))
        self.true(gte(abs(self.__normal2), self.__pos_zero))
        self.true(gte(abs(self.__normal3), self.__pos_zero))
        self.true(gte(abs(self.__normal4), self.__pos_zero))
        self.true(gte(abs(self.__normal5), self.__pos_zero))
        self.true(gte(abs(self.__normal6), self.__pos_zero))

        self.false(lt(abs(self.__subnormal1), self.__pos_zero))
        self.false(lt(abs(self.__subnormal2), self.__pos_zero))
        self.false(lt(abs(self.__neg_inf), self.__pos_zero))
        self.false(lt(abs(self.__pos_inf), self.__pos_zero))
        self.false(lt(abs(self.__normal1), self.__pos_zero))
        self.false(lt(abs(self.__normal2), self.__pos_zero))
        self.false(lt(abs(self.__normal3), self.__pos_zero))
        self.false(lt(abs(self.__normal4), self.__pos_zero))
        self.false(lt(abs(self.__normal5), self.__pos_zero))
        self.false(lt(abs(self.__normal6), self.__pos_zero))

        self.true(eq_float(abs(self.__neg_normal1), self.__normal1))
        self.false(eq_float(abs(self.__normal1), self.__neg_normal1))
    
    def test_neg(self):
        self.true(eq_float(neg(neg(self.__normal1)), self.__normal1))
        self.true(eq_float(neg(self.__neg_normal1), self.__normal1))
        self.true(eq_float(neg(self.__normal1), self.__neg_normal1))
        self.false(eq_float(neg(self.__normal2), self.__normal2))

    def test_add(self):
        self.true(add(self.__pos_zero, self.__pos_zero) == self.__pos_zero) # neutrality
        self.true(add(self.__neg_zero, self.__neg_zero) == self.__pos_zero) # neutrality
        self.true(add(self.__pos_zero, self.__neg_zero) == self.__pos_zero) # neutrality
        
        self.true(add(self.__normal1, self.__neg_zero) == self.__normal1) # Identity
        self.true(add(self.__normal1, self.__pos_zero) == self.__normal1) # Identity

        self.true(add(self.__normal1, self.__normal2) == add(self.__normal2, self.__normal1)) # Cummutativity
        self.true(add(self.__normal1, self.__normal6) == add(self.__normal6, self.__normal1)) # Cummutativity


        #self.true(
        #    add(
        #        add(self._normal1, self.__normal2),
        #        self._normal3
        #    )
        #    ==
        #    add(
        #        self._normal1,
        #        add(self._normal2, self.__normal3)                
        #    )
        #)# Associativity is not guaranteed for floating point numbers

        #TODO: add constructed checks with real values

    def test_sub(self):
        pass

    def test_mul(self):
        pass

    def test_div(self):
        pass

    def test_rem(self):
        pass

    def test_squrt(self):
        pass

    def test_fma(self):
        pass
    
    def test_min(self):
        pass
    
    def test_max(self):
        pass

    def test_pack(self):

        case, unpacked = unpack(self.__pos_zero)
        self.true(eq_bitwise(pack(unpacked, self.__sort, Truncate), self.__pos_zero))

        case, unpacked = unpack(self.__neg_zero)
        self.true(eq_bitwise(pack(unpacked, self.__sort,Truncate), self.__neg_zero))

        case, unpacked = unpack(self.__pos_inf)
        self.true(eq_bitwise(pack(unpacked, self.__sort, Truncate), self.__pos_inf))

        case, unpacked = unpack(self.__neg_inf)
        self.true(eq_bitwise(pack(unpacked, self.__sort, Truncate), self.__neg_inf))

        case, unpacked = unpack(self.__normal5)
        self.true(eq_bitwise(pack(unpacked, self.__sort, Truncate), self.__normal5))

        case, unpacked = unpack(self.__normal6)
        self.true(eq_bitwise(pack(unpacked, self.__sort, Truncate), self.__normal6))

        case, unpacked = unpack(self.__nan1)
        self.true(eq_bitwise(pack(unpacked, self.__sort, Truncate), self.__nan1))

        case, unpacked = unpack(self.__normal1)
        self.true(eq_bitwise(pack(unpacked, self.__sort, Truncate), self.__normal1))

        case, unpacked = unpack(self.__neg_normal1)
        self.true(eq_bitwise(pack(unpacked, self.__sort, Truncate), self.__neg_normal1))

        case, unpacked = unpack(self.__normal2)
        self.true(eq_bitwise(pack(unpacked, self.__sort, Truncate), self.__normal2))

        case, unpacked = unpack(self.__normal3)
        self.true(eq_bitwise(pack(unpacked, self.__sort, Truncate), self.__normal3))

        case, unpacked = unpack(self.__normal4)
        self.true(eq_bitwise(pack(unpacked, self.__sort, Truncate), self.__normal4))

        case, unpacked = unpack(self.__subnormal1)
        self.true(eq_bitwise(pack(unpacked, self.__sort, Truncate), self.__subnormal1))

        case, unpacked = unpack(self.__subnormal2)
        self.true(eq_bitwise(pack(unpacked, self.__sort, Truncate), self.__subnormal2))

        x = FloatConst("x", 5, 5)
        case, unpacked = unpack(x)
        result = validate(eq_bitwise(pack(unpacked, FloatSort(5, 5), Truncate), x))
        self.assertTrue(result) #This already includes all the above checks


def validate(statement):
    solver = Solver()
    solver.add(Not(statement))
    return solver.check() == unsat


if __name__ == '__main__':
    unittest.main()