x1<-c()
library(nleqslv)

c1=1
k1=0.5
#sample generation
num_samples=50
for(i  in 1:num_samples){
  y<-runif(1)
  
  f<-function(x){
    
    return ( y - 1 + ( 1 + x^c1 ) ^(-k1 ) )
  }
  x1[i]<-nleqslv(1.2,f)$x
}
# x1 is my sample space
x1

# setting up mle eqn.
mle<-function( x ){
  c<-x[1]
  k<-x[2]
  # calculated using likelihood function 
  sum1=sum( log( x1 ) )
  sum2=sum( ( (x1 ^ c) * log(x1) ) /(1+x1^c) )
  sum3= sum( log ( 1+ x1^c ))
  
  eqn1<-(num_samples /c) + sum1 -(k+1) * sum2
  
  eqn2<-(num_samples/k) - sum3
  
  return(c(eqn1,eqn2))
} 

ansc <- nleqslv(c(1.2 , 1.2 ), mle)$x[1]
ansk <-nleqslv(c(1.2 , 1.2 ), mle)$x[2]

print(paste("mle of c is ",ansc ))
print(paste("mle of k is",ansk ))


#calculating bias
print(paste("bias of c is ", abs(ansc-c1)))
print(paste("bias of k is ", abs(ansk-k1)))

# mean squared error
print(paste(" means squared error of c is ", (ansc-c1)^2/num_samples))
print(paste(" means squared error of k is ", (ansk-k1)^2/num_samples))




