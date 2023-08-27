#!/usr/bin/env python3

import sympy as sym

x = sym.Symbol('x')
y = sym.Symbol('y')

n = (12 - (x * sym.exp(y)))
print("numerator wrt x", n.diff(x).evalf(subs={x:3, y:5}))
print("numerator wrt y", n.diff(y).evalf(subs={x:3, y:5}))

d = (45 + (x * y * sym.exp(-x)))
print("denominator wrt x", d.diff(x).evalf(subs={x:3, y:5}))
print("denominator wrt y", d.diff(y).evalf(subs={x:3, y:5}))

z = n / d
print ("z wrt x", z.diff(x).evalf(subs={x:3, y:5}))
print ("z wrt y", z.diff(y).evalf(subs={x:3, y:5}))
