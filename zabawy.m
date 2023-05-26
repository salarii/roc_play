x =  0.01 : 0.01 : 10

plot( x  , 1./(1+x) )


a = 1;
b = 9;
O =1

A = [ 1  a; b  -1]
B = [O  0]

B* inv(A)