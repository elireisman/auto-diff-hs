#!/usr/bin/env python3

import sympy as sym

x = sym.Symbol('x')
y = sym.Symbol('y')

n = (12 - (x * sym.exp(y)))
d = (45 + (x * y * sym.exp(-x)))
z = n / d

print ("z wrt x", z.diff(x).evalf(subs={x:3, y:5}))
print ("z wrt y", z.diff(y).evalf(subs={x:3, y:5}))
