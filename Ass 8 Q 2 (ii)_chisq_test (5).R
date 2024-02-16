#  _______ Q 1.__________

O=c(12,8,20,2,14,10,15,6,9,4)
E=c(10,10,10,10,10,10,10,10,10,10)

# Hypothesis
# H0 : equally distributed.
# Ha: not equally distributed.


W = 0
for ( i in 1:10){
  W = W + ( (O[i]-E[i] ) ^2 ) /  10 
  
}

# w should follow chi_square (9) distribution.
# according to the table ..chi square 9,0.05 = 16.92.

if( W <=16.92 && sum(O) == sum(E) ) {
  print( " we failed to reject the hypothesis and the value of W is " )
  print ( W )
}else{
  print(" we reject the hypothesis and value of W is ") 
  print( W )
} 




