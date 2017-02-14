syms fx fdx fd2x fd3x fd4x fd5x A B C D h
eq1 = 1*fx-h*fdx+h^2/2*fd2x-h^3/6*fd3x+h^4/24*fd4x-h^5/120*fd5x == A
eq2 = 1*fx+h*fdx+h^2/2*fd2x+h^3/6*fd3x+h^4/24*fd4x+h^5/120*fd5x == B 
eq3 = 1*fx+2*h*fdx+(4*h^2)/2*fd2x+(8*h^3)/6*fd3x+(16*h^4)/24*fd4x+(32*h^5)/120*fd5x == C
eq4 = 1*fx+3*h*fdx+(9*h^2)/2*fd2x+(27*h^3)/6*fd3x+(81*h^4)/24*fd4x+(243*h^5)/120*fd5x == D
sol = solve([eq1, eq2, eq3, eq4], [fdx])

