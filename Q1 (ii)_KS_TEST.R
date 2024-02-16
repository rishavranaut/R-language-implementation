
#______________(ii)_____________
library(nleqslv)
lindley_dist_kstest<-function(t,n){
  k=c() ; x=c() ; Fn=c() ; Fo=c() ; D_pos=c() ; Fn_x_k_1=c() ; D_neg=c()
  x <- c()
  
  for (i in 1:n) {
    y <- runif(1)
    
    f <- function(x) {
      return(y - 1 + ((t + 1 + t * x) / (t + 1)) * exp(-t * x))
    }
    
    x[i] <- nleqslv( 1.2,f)$x      # using $x to retrieve root only 
    
  }
  x=sort(x)
  for ( i in 1:n){
    k[i]<-i
    Fn[i]<-i/n
    Fo[i]<- 1-( (t+1+t*x[i])/(t+1) ) *exp( -t * x[i] )
    D_pos[i]<-abs(Fo[i]-Fn[i])
    Fn_x_k_1[i]<-(i-1)/n
    D_neg[i]<-abs(Fo[i]-Fn_x_k_1[i])
  }
  D = max(max(D_pos),max(D_neg))
  if(D<0.240){
    print("hypothesis accepted")
    return(D)
  }
  else{
    print("oops!! hypothesis not accepted")
  }
}
lindley_dist_kstest(1,30)


