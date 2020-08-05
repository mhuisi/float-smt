from z3 import *

sort = Float16()
a = FPVal(2, sort)
b = FPVal(3.277E4, sort)
print(a, b)
c = a*b
print(simplify(c))

print(FPVal(2 * 3.277E4), sort)

print((2 * 3.277E4)/2**15)


print("________________")

print(FPVal(3.277E4 + 3.277E4), sort)
print(simplify(b + b))

def validate(statement):
    solver = Solver()
    solver.add(Not(statement))
    result = solver.check()
    if(result == sat):
        print(solver.model())
    return result == unsat

