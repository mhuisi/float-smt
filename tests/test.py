import unittest
from float import *

class Float(unittest.TestCase):
    def test_float_field_sizes(self):
        mantissa_size, exponent_size = 2, 3
        sort = FloatSort(mantissa_size, exponent_size)
        self.assertEqual(mantissa_size, sizes(sort)[0])
        self.assertEqual(exponent_size, sizes(sort)[1])

    def test_converions_to_Z3(self):
        x,y = FloatConst("x", 23, 8), FloatConst("y", 23, 8)
        result = validate(
            Or(
                eq_bitwise(
                    z3FP_to_Float(Float_to_z3FP(x)),
                    x),
                is_nan(x)
            )
        )
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
        
        x,y = FloatConst("x", 23, 8), FloatConst("y", 23, 8)
        x_z3, y_z3 = Float_to_z3FP(x), Float_to_z3FP(y)

        result = validate(Or(
                                (gt(x, y) == fpGT(x_z3, y_z3)), 
                                Or(is_nan(x), is_nan(y))
                            )
                        )
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

        rm = Truncate
        a = FloatVal(0,7503853,140, FloatSort(23,8))
        b = FloatVal(0,126166,145, FloatSort(23,8))
        x = simplify(Float_to_z3FP(mul(a, b,rm)))
        y = simplify(fpMul(rm_to_z3rm(rm), Float_to_z3FP(a), Float_to_z3FP(b)))
        self.assertTrue(x==y)

        a = FloatVal(0,7503853,70, FloatSort(23,8))
        b = FloatVal(1,126166,70, FloatSort(23,8))
        x = simplify(Float_to_z3FP(mul(a, b,rm)))
        y = simplify(fpMul(rm_to_z3rm(rm), Float_to_z3FP(a), Float_to_z3FP(b)))
        self.assertTrue(x==y)

        a = FloatVal(0,7503853,140, FloatSort(23,8))
        b = FloatVal(1,126166,70, FloatSort(23,8))
        x = simplify(Float_to_z3FP(mul(a, b,rm)))
        y = simplify(fpMul(rm_to_z3rm(rm), Float_to_z3FP(a), Float_to_z3FP(b)))
        self.assertTrue(x==y)



        #-----------------------------------------------------
        
        rm = Truncate
        a = FloatVal(0,1921,0, FloatSort(23,8))
        b = FloatVal(0,0,0, FloatSort(23,8))
        x = simplify(Float_to_z3FP(mul(a, b,rm)))
        y = simplify(fpMul(rm_to_z3rm(rm), Float_to_z3FP(a), Float_to_z3FP(b)))
        #print(x)
        #print(y)
        self.assertTrue(x==y)



        a = FloatVal(1,8,0, FloatSort(23,8))
        b = FloatVal(0,0,255, FloatSort(23,8))
        x = simplify(Float_to_z3FP(mul(a, b,rm)))
        y = simplify(fpMul(rm_to_z3rm(rm), Float_to_z3FP(a), Float_to_z3FP(b)))
        #print(x)
        #print(y)
        self.assertTrue(x==y)


        rm = Truncate#NearestTieToEven
        a = FloatVal(0,2138113,128, FloatSort(23,8))
        b = FloatVal(0,7503884,7, FloatSort(23,8))
        x = simplify(Float_to_z3FP(mul(a, b,rm)))
        y = simplify(fpMul(rm_to_z3rm(rm), Float_to_z3FP(a), Float_to_z3FP(b)))
        #print(x)
        #print(y)
        self.assertTrue(x==y)


        rm = NearestTieToEven
        a = FloatVal(1,3098416,125, FloatSort(23,8))
        b = FloatVal(0,262144,131, FloatSort(23,8))
        x = simplify(Float_to_z3FP(mul(a, b,rm)))
        y = simplify(fpMul(rm_to_z3rm(rm), Float_to_z3FP(a), Float_to_z3FP(b)))
        #print(x)
        #print(y)
        self.assertTrue(x==y)

        
        x = simplify(Float_to_z3FP(mul(FloatVal(0,3588100,128, FloatSort(23,8)), FloatVal(0,8252566,32, FloatSort(23,8)),rm)))
        y = simplify(fpMul(rm_to_z3rm(rm), Float_to_z3FP(FloatVal(0,3588100,128, FloatSort(23,8))), Float_to_z3FP(FloatVal(0,8252566,32, FloatSort(23,8)))))
        self.assertTrue(x==y)


        # rm = Truncate
        # a = FloatVal(0,7,0, FloatSort(10,5))
        # b = FloatVal(0,7,0, FloatSort(10,5))
        # x = simplify(Float_to_z3FP(mul(a, b,rm)))
        # y = simplify(fpMul(rm_to_z3rm(rm), Float_to_z3FP(a), Float_to_z3FP(b)))
        # print(x)
        # print(y)
        # self.assertTrue(x==y)


        # rm = Truncate
        # a = FloatVal(1,612,23, FloatSort(10,5))
        # b = FloatVal(0,7,0, FloatSort(10,5))
        # x = simplify(Float_to_z3FP(mul(a, b,rm)))
        # y = simplify(fpMul(rm_to_z3rm(rm), Float_to_z3FP(a), Float_to_z3FP(b)))
        # print(x)
        # print(y)
        # self.assertTrue(x==y)



        


        # rm = Truncate
        # a = FloatVal(0,15,24, FloatSort(6,5))
        # b = FloatVal(0,63,23, FloatSort(6,5))
        # x = simplify(Float_to_z3FP(mul(a, b,rm)))
        # y = simplify(fpMul(rm_to_z3rm(rm), Float_to_z3FP(a), Float_to_z3FP(b)))
        # print(x)
        # print(y)
        # self.assertTrue(x==y)


        rm = Truncate
        a = FloatVal(0,7503853,15, FloatSort(23,8))
        b = FloatVal(1,126166,15, FloatSort(23,8))
        x = simplify(Float_to_z3FP(mul(a, b,rm)))
        y = simplify(fpMul(rm_to_z3rm(rm), Float_to_z3FP(a), Float_to_z3FP(b)))
        #print(x)
        #print(y)
        self.assertTrue(x==y)




        # rm = Truncate
        # a = FloatVal(1,3756948,8, FloatSort(23,8))
        # b = FloatVal(1,2096763,99, FloatSort(23,8))
        # x = simplify(Float_to_z3FP(mul(a, b,rm)))
        # y = simplify(fpMul(rm_to_z3rm(rm), Float_to_z3FP(a), Float_to_z3FP(b)))
        # print(x)
        # print(y)
        # self.assertTrue(x==y)


        rm = Truncate
        a = FloatVal(1,4094928,56, FloatSort(23,8))
        b = FloatVal(0,3307058,79, FloatSort(23,8))
        x = simplify(Float_to_z3FP(mul(a, b,rm)))
        y = simplify(fpMul(rm_to_z3rm(rm), Float_to_z3FP(a), Float_to_z3FP(b)))
        #print(x)
        #print(y)
        self.assertTrue(x==y)


        rm = Truncate
        a = FloatVal(1,7717123,127, FloatSort(23,8))
        b = FloatVal(0,9589,127, FloatSort(23,8))
        x = simplify(Float_to_z3FP(mul(a, b,rm)))
        y = simplify(fpMul(rm_to_z3rm(rm), Float_to_z3FP(a), Float_to_z3FP(b)))
        #print(x)
        #print(y)
        self.assertTrue(x==y)



        rm = Truncate
        a = FloatVal(1,3699894,192, FloatSort(23,8))
        b = FloatVal(0,3186977,223, FloatSort(23,8))
        x = simplify(Float_to_z3FP(mul(a, b,rm)))
        y = simplify(fpMul(rm_to_z3rm(rm), Float_to_z3FP(a), Float_to_z3FP(b)))
        print(x)
        print(y)
        self.assertTrue(x==y)



        x, y = FloatConst("x", 23, 8), FloatConst("y", 23, 8)
        x_z3, y_z3 = Float_to_z3FP(x), Float_to_z3FP(y)

        for rm in (Up, Down, Truncate, NearestTieToEven, NearestTieAwayFromZero):
            result = validate(
                Or(
                    ( Float_to_z3FP(mul(x, y, rm)) == fpMul(rm_to_z3rm(rm), x_z3, y_z3) ),
                    Or(is_nan(x), is_nan(y)),
                    Or(is_subnormal(x), is_subnormal(y)),
                    Or(is_subnormal(z3FP_to_Float(fpMul(rm_to_z3rm(rm), x_z3, y_z3))))
                )
            )
            self.assertTrue(result)

        rm = Truncate
        a = FloatVal(1,3564550,0, FloatSort(23,8))
        b = FloatVal(1,5578753,131, FloatSort(23,8))
        x = simplify(Float_to_z3FP(mul(a, b,rm)))
        y = simplify(fpMul(rm_to_z3rm(rm), Float_to_z3FP(a), Float_to_z3FP(b)))
        print(x)
        print(y)
        self.assertTrue(x==y)

    def test_div(self):
        x, y = FloatConst("x", 10, 5), FloatConst("y", 10, 5)
        x_z3, y_z3 = Float_to_z3FP(x), Float_to_z3FP(y)

        for rm in (Up, Down, Truncate, NearestTieToEven, NearestTieAwayFromZero):
            result = validate(
                Or(
                    ( Float_to_z3FP(div(x, y, rm)) == fpDiv(rm_to_z3rm(rm), x_z3, y_z3) ),
                    Or(is_nan(x), is_nan(y)),
                    Or(is_subnormal(x), is_subnormal(y)),
                    Or(is_subnormal(z3FP_to_Float(fpDiv(rm_to_z3rm(rm), x_z3, y_z3))))
                )
            )
            self.assertTrue(result)

    def test_rem(self):
        pass

    def test_squrt(self):
        x = FloatConst("x", 23, 8)
        x_z3 = Float_to_z3FP(x)

        for rm in (NearestTieToEven, NearestTieAwayFromZero, Up, Down, Truncate):
            result = validate((Float_to_z3FP(sqrt(x, rm)) == fpSqrt(rm_to_z3rm(rm), x_z3)))
            self.assertTrue(result)

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

        #now do that for the other rounding modes:
        
        result = validate(eq_bitwise(pack(unpacked, FloatSort(5, 5), NearestTieAwayFromZero), x))
        self.assertTrue(result)

        result = validate(eq_bitwise(pack(unpacked, FloatSort(5, 5), Up), x))
        self.assertTrue(result)

        result = validate(eq_bitwise(pack(unpacked, FloatSort(5, 5), Down), x))
        self.assertTrue(result)

        result = validate(eq_bitwise(pack(unpacked, FloatSort(5, 5), NearestTieToEven), x))
        self.assertTrue(result)


def validate(statement):
    solver = Solver()
    solver.add(Not(statement))
    result = solver.check()
    if(result == sat):
        print(solver.model())
    return result == unsat


if __name__ == '__main__':
    unittest.main()