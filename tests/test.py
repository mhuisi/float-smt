from z3 import *
import unittest
import itertools
import cProfile, pstats, io
from pstats import SortKey
from floatsmt import *

set_param('parallel.enable', True)
set_option("parallel.threads.max", 4)

def validate(s, statement):
    solver = Solver()
    set_param("parallel.enable", True)
    set_param("parallel.threads.max", 4)
    solver.add(Not(statement))
    print("\nstarting validation of %s" % s)
    result = solver.check()
    if(result == sat):
        print(solver.model())
    return result == unsat

class Float(unittest.TestCase):
    def test_float_field_sizes(self):
        mantissa_size, exponent_size = 2, 3
        sort = FloatSort(mantissa_size, exponent_size)
        self.assertEqual(mantissa_size, sizes(sort)[0])
        self.assertEqual(exponent_size, sizes(sort)[1])

    def test_conversions_to_Z3(self):
        x = FloatConst("x", 23, 8)
        result = validate("z3_conv",
            Or(eq_bitwise(z3FP_to_Float(Float_to_z3FP(x)), x),
               is_nan(x)))
        self.assertTrue(result)

    def test_precision_conversion(self):
        def test(rm, x, src_sort, dest_sort):
            sign, mantissa, exponent = x
            m_s, e_s = src_sort
            m_d, e_d = dest_sort
            x = FloatVal(sign, mantissa, exponent, FloatSort(m_s, e_s))
            a = simplify(Float_to_z3FP(convert_float(x, FloatSort(m_d, e_d), rm)))
            b = simplify(fpFPToFP(rm_to_z3rm(rm), Float_to_z3FP(x), FPSort(e_d, m_d+1)))
            self.assertTrue(simplify(a == b))

        test(Truncate, (0, 50, 10), (23, 8), (10, 5))
        test(Up, (0, 87, 0), (10, 5), (3, 2))
        test(Up, (0, 192, 3), (10, 5), (3, 2))
        test(Up, (0, 270468, 0), (23, 8), (10, 5))
        test(Truncate, (1, 9, 5), (5, 3), (3, 2))
        
        sizes = ((10, 5), (23, 8), (52, 11))
        for (m, e) in sizes:
            for (m_result, e_result) in sizes:
                result_sort = FloatSort(m_result, e_result)
                result_sort_z3 = FPSort(e_result, m_result+1)
                x = FloatConst("x", m, e)
                x_z3 = Float_to_z3FP(x)

                for rm in (Truncate, Up, Down, NearestTiesToEven, NearestTiesAwayFromZero):
                    r = Float_to_z3FP(convert_float(x, result_sort, rm))
                    r_z3 = fpFPToFP(rm_to_z3rm(rm), x_z3, result_sort_z3)
                    result = validate("convert (%d,%d)->(%d,%d) [%s]" % (m, e, m_result, e_result, rm), r == r_z3)
                    self.assertTrue(result)

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

        z = If(BitVecVal(1, 1) == BitVecVal(0, 1), self.__normal, self.__pos_zero)
        self.true(is_pos_zero(z))

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
        
        x,y = FloatConst("x", 23, 8), FloatConst("y", 23, 8)
        x_z3, y_z3 = Float_to_z3FP(x), Float_to_z3FP(y)
        result = validate("gt", gt(x, y) == fpGT(x_z3, y_z3))
        self.assertTrue(result)

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

    def assert_float_eq(self, rm, a, b, sort, ops):
        sign_a, mantissa_a, exponent_a = a
        sign_b, mantissa_b, exponent_b = b
        m, e = sort
        op, z3_op = ops

        sort = FloatSort(m, e)
        a = FloatVal(sign_a, mantissa_a, exponent_a, sort)
        b = FloatVal(sign_b, mantissa_b, exponent_b, sort)
        x = simplify(Float_to_z3FP(op(a, b, rm)))
        y = simplify(z3_op(rm_to_z3rm(rm), Float_to_z3FP(a), Float_to_z3FP(b)))
        self.assertTrue(x == y)

    def validate_op(self, name, sort, ops):
        m, e = sort
        op, z3_op = ops

        x, y = FloatConst("x", m, e), FloatConst("y", m, e)
        x_z3, y_z3 = Float_to_z3FP(x), Float_to_z3FP(y)
        for rm in (Truncate, Up, Down, NearestTiesToEven, NearestTiesAwayFromZero):
            r = Float_to_z3FP(op(x, y, rm))
            r_z3 = z3_op(rm_to_z3rm(rm), x_z3, y_z3)
            result = validate("%s_(%d,%d)_%s" % (name, m, e, rm), r == r_z3)
            self.assertTrue(result)

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
        test = lambda rm, a, b, sort: self.assert_float_eq(rm, a, b, sort, (add, fpAdd))

        test(Truncate, (0, 7503853, 140), (0, 126166, 145), (23, 8))
        test(Truncate, (0, 7503853, 70), (1, 126166, 70), (23, 8))
        test(Truncate, (0, 7503853, 140), (1, 126166, 70), (23, 8))
        test(Truncate, (0, 585, 21), (1, 64, 21), (10, 5))
        test(Truncate, (0, 1023, 21), (0, 1023, 21), (10, 5))
        test(Truncate, (1, 1, 0), (0, 0, 31), (10, 5))
        test(Truncate, (0, 32, 0), (0, 5, 4), (10, 5))
        test(Truncate, (1, 4, 15), (0, 4, 15), (10, 5))
        test(Truncate, (0, 4, 15), (1, 4, 15), (10, 5))
        test(Truncate, (0, 0, 0), (1, 0, 0), (10, 5))
        test(Truncate, (0, 0, 30), (0, 0, 30), (10, 5))

        # very small floats
        self.validate_op("add", (3, 2), (add, fpAdd))

        # half precision
        self.validate_op("add", (10, 5), (add, fpAdd))

    def test_mul(self):
        test = lambda rm, a, b, sort: self.assert_float_eq(rm, a, b, sort, (mul, fpMul))

        test(Truncate, (0, 136, 2), (0, 1009, 1), (10, 5))
        test(Truncate, (0, 136, 2), (0, 1009, 1), (10, 5))
        test(Truncate, (0, 0, 16), (0, 0, 30), (10, 5))

        self.validate_op("mul", (10, 5), (mul, fpMul))

    def test_div(self):
        self.assert_float_eq(Truncate, (0, 0, 24), (0, 0, 7), (10, 5), (div, fpDiv))

        self.validate_op("div", (10, 5), (div, fpDiv))

    def test_rem(self):
        def test(a, b):
            sign_a, mantissa_a, exponent_a = a
            sign_b, mantissa_b, exponent_b = b
            sort = FloatSort(5, 3)
            a = FloatVal(sign_a, mantissa_a, exponent_a, sort)
            b = FloatVal(sign_b, mantissa_b, exponent_b, sort)
            x = simplify(Float_to_z3FP(rem(a, b)))
            y = simplify(fpRem(Float_to_z3FP(a), Float_to_z3FP(b)))
            self.assertTrue(x == y)
        test((1, 1, 1), (1, 3, 0))
        test((0, 13, 2), (1, 15, 0))
        test((1, 8, 6), (0, 4, 0))

        m, e = 5, 3
        x, y = FloatConst("x", m, e), FloatConst("y", m, e)
        x_z3, y_z3 = Float_to_z3FP(x), Float_to_z3FP(y)
        result = validate("rem_(%d,%d)" % (m, e), Float_to_z3FP(rem(x, y)) == fpRem(x_z3, y_z3))
        self.assertTrue(result)

    def test_sqrt(self):
        def test(rm, a, sort):
            sign_a, mantissa_a, exponent_a = a
            m, e = sort
            sort = FloatSort(m, e)
            a = FloatVal(sign_a, mantissa_a, exponent_a, sort)
            x = simplify(Float_to_z3FP(sqrt(a, rm)))
            y = simplify(fpSqrt(rm_to_z3rm(rm), Float_to_z3FP(a)))
            self.assertTrue(x == y)

        test(Truncate, (0, 100, 10), (10, 5))
        test(Truncate, (0, 100, 30), (10, 5))
        test(Truncate, (0, 10, 10), (10, 5))
        test(Truncate, (0, 0, 0), (10, 5))
        # this case does not work
        # test(NearestTiesToEven, (0, 7, 0), (3, 2))
        test(Truncate, (0, 100, 1022), (52, 11))

        # validation fails
        m, e = 5, 3
        x = FloatConst("x", m, e)
        x_z3 = Float_to_z3FP(x)
        for rm in (NearestTiesToEven, NearestTiesAwayFromZero, Up, Down, Truncate):
            result = validate("sqrt_(%d, %d)_%s" % (m, e, rm), Float_to_z3FP(sqrt(x, rm)) == fpSqrt(rm_to_z3rm(rm), x_z3))
            self.assertTrue(result)

    def test_fma(self):
        def test(rm, a, b, c, sort):
            sign_a, mantissa_a, exponent_a = a
            sign_b, mantissa_b, exponent_b = b
            sign_c, mantissa_c, exponent_c = c
            m, e = sort

            sort = FloatSort(m, e)
            a = FloatVal(sign_a, mantissa_a, exponent_a, sort)
            b = FloatVal(sign_b, mantissa_b, exponent_b, sort)
            c = FloatVal(sign_c, mantissa_c, exponent_c, sort)
            x = simplify(Float_to_z3FP(fma(a, b, c, rm)))
            y = simplify(fpFMA(rm_to_z3rm(rm), Float_to_z3FP(a), Float_to_z3FP(b), Float_to_z3FP(c)))
            self.assertTrue(x == y)

        test(Truncate, (0, 86, 1), (1, 512, 0), (0, 0, 1), (10, 5))
        test(Truncate, (0, 16, 11), (0, 900, 16), (0, 424, 10), (10, 5))
        test(Truncate, (1, 517, 3), (0, 465, 3), (0, 2, 0), (10, 5))
        test(Truncate, (0, 0, 3), (0, 0, 3), (1, 2, 0), (3, 2))
        test(Truncate, (0, 12, 2), (0, 9, 1), (1, 28, 7), (5, 3))
        test(Truncate, (0, 26, 6), (1, 5, 6), (0, 0, 7), (5, 3))
        test(Down, (1, 0, 1), (0, 7, 0), (0, 7, 0), (3, 2))
        test(Up, (1, 21, 4), (1, 13, 0), (0, 25, 0), (5, 3))
        test(Up, (0, 30, 2), (0, 31, 0), (0, 4, 0), (5, 3))
        test(Truncate, (0, 21, 5), (0, 7, 6), (0, 16, 3), (5, 3))

        # more z3 strangeness.
        # the later validation fails with this example.
        # testing it does not show any errors, though!
        for rm in (Truncate, Up, Down, NearestTiesToEven, NearestTiesAwayFromZero):
            test(rm, (0, 28, 0), (0, 29, 0), (0, 10, 3), (5, 3))

        # this validation eventually fails with a model that is not reproducible
        m, e = 5, 3
        x, y, z = FloatConst("x", m, e), FloatConst("y", m, e), FloatConst("z", m, e)
        x_z3, y_z3, z_z3 = Float_to_z3FP(x), Float_to_z3FP(y), Float_to_z3FP(z)
        for rm in (Up, Truncate, Down, NearestTiesToEven, NearestTiesAwayFromZero):
            r = Float_to_z3FP(fma(x, y, z, rm))
            r_z3 = fpFMA(rm_to_z3rm(rm), y_z3, z_z3, x_z3)
            result = validate("fma_(%d,%d)_%s" % (m, e, rm), r == r_z3)
            self.assertTrue(result)
    
    def test_min(self):
        def test(a, b, sort):
            sign_a, mantissa_a, exponent_a = a
            sign_b, mantissa_b, exponent_b = b
            m, e = sort
            sort = FloatSort(m, e)
            a = FloatVal(sign_a, mantissa_a, exponent_a, sort)
            b = FloatVal(sign_b, mantissa_b, exponent_b, sort)
            print(simplify(Float_to_z3FP(a)), simplify(Float_to_z3FP(b)))
            x = simplify(Float_to_z3FP(min_float(a, b)))
            y = simplify(fpMin(Float_to_z3FP(a), Float_to_z3FP(b)))
            self.assertTrue(x == y)

        test((0, 1, 2), (0, 0, 0), (10, 5))

        # z3 does not evaluate min(+0, -0) at all, while we do.
        # the smtlib standard allows for this disparity.
        m, e = 10, 5
        x, y = FloatConst("x", m, e), FloatConst("y", m, e)
        x_z3, y_z3 = Float_to_z3FP(x), Float_to_z3FP(y)
        result = validate("min_(%d,%d)" % (m, e), 
                          Or(And(is_zero(x), is_zero(y)), 
                             Float_to_z3FP(min_float(x, y)) == fpMin(x_z3, y_z3)))
        self.assertTrue(result)
    
    def test_max(self):
        m, e = 10, 5
        x, y = FloatConst("x", m, e), FloatConst("y", m, e)
        x_z3, y_z3 = Float_to_z3FP(x), Float_to_z3FP(y)
        result = validate("max_(%d,%d)" % (m, e), 
                          Or(And(is_zero(x), is_zero(y)), 
                             Float_to_z3FP(max_float(x, y)) == fpMax(x_z3, y_z3)))
        self.assertTrue(result)

    def test_pack(self):
        def test_inv(x):
            case, unpacked = unpack(x)
            self.true(eq_bitwise(pack(unpacked, self.__sort, Truncate, case), x))
        
        test_inv(self.__pos_zero)
        test_inv(self.__neg_zero)
        test_inv(self.__pos_inf)
        test_inv(self.__neg_inf)
        test_inv(self.__normal5)
        test_inv(self.__normal6)
        test_inv(self.__nan1)
        test_inv(self.__normal1)
        test_inv(self.__neg_normal1)
        test_inv(self.__normal2)
        test_inv(self.__normal3)
        test_inv(self.__normal4)
        test_inv(self.__subnormal1)
        test_inv(self.__subnormal2)

        m, e = 5, 5
        x = FloatConst("x", m, e)
        case, unpacked = unpack(x)
        for rm in (Truncate, Up, Down, NearestTiesToEven, NearestTiesAwayFromZero):
            packed = pack(unpacked, FloatSort(m, e), rm, case)
            result = validate("pack_inverse_unpack_%s" % rm, Or(And(is_nan(x), is_nan(packed)), eq_bitwise(packed, x)))
            self.assertTrue(result)

if __name__ == '__main__':
    unittest.main()