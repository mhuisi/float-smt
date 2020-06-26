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

if __name__ == '__main__':
    unittest.main()