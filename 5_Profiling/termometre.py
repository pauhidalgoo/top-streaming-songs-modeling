from PyDesmos import Graph
import sympy as sp



s=0
a=3
b=7
m=10
with Graph('my graph') as G:
    y, x = G.y, G.x
    
    G(s, "<", x, "<", a, min=0, max=2,color="#FF0000")
    G.append("b>x>a{y>0}")
    G(m,">",x,">",b)

