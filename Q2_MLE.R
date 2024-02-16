# Q2
#______________(i)__________#
library(nleqslv)
sample_s_norm_dist_mle<-function(mew,sigma,num_samples){
  x=c()
  
  for(i in 1:num_samples){
    x[i]<-rnorm(1,mew,sigma)
  }
  mew_MLE=mean(x)
 
  
  sigma_MLE=0
  
  for(i in 1:num_samples){
    sigma_MLE<-sigma_MLE+(x[i]-mean(x))^2
  }
  
  sigma_MLE=sqrt(sigma_MLE/num_samples)
  print("mew_MLE,sigma_MLE is")
  return(c(mew_MLE,sigma_MLE))
}


sample_s_norm_dist_mle(2,3,1000)

#________(ii)___________#

library(nleqslv)

Sample_expo_dist_mle<-function(beta1,alpha,num_samples){
  x<-c()
  for(i in 1:num_samples){
    y<-runif(1)
    x[i]<- (-1)*(1/beta1)*log(1-(y^(1/alpha)))
  }
    
    
    beta_eqn<-function(beta){
      sum1=sum( x )
      sum2=sum( log( 1- exp(-beta * x) ) )
      sum3=sum( ( x *exp(-beta*x) ) / ( 1 - exp (-beta * x) ) )
      
      eqn<-( num_samples / beta )- sum1 -( ( ( num_samples ) / sum2 ) + 1 )*sum3
      return(eqn)
    }
    beta_mle = nleqslv( 1, beta_eqn )$x
    
    alpha_mle=( -num_samples ) / sum ( log ( 1 - exp (-beta_mle * x ) ) )
    
    print("beta_mle , alpha_mle is")
    
    return(c(beta_mle,alpha_mle))
}
Sample_expo_dist_mle(4,6,1000)    


#___________(iii)__________#

library(nleqslv)
sample_space_ks_dist_mle<-function( alpha2 , beta2 , num_samples ){
  
  x = c()
  
  for(i in 1:num_samples){
    
    y<-runif(1)
    
    x[i]<-((1-(1-y)^1/beta2))^1/alpha2
    
  }
  alpha_eqn_1 <- function(alpha){
    
    s1 =  sum( log(x) )
    
    s2 = sum( log (1 - (x^alpha) ) ) 
    
    s3 = sum( ((x^alpha) * log(x)) )
    
    s4 = sum((1-x^alpha))
    
    eqn <- (num_samples/alpha) + s1 + ( (num_samples/s2) + 1 ) * (s3/s4)
    
    return(eqn)
  }
  alpha_mle1 = nleqslv( 1.2 , alpha_eqn_1 )$x
  
  beta_mle1= - num_samples/( sum ( log (1 - x^alpha_mle1 ) ) )
  
  print("alpha mle,beta mle is")
  
  return(c( alpha_mle1 , beta_mle1 ))
}
sample_space_ks_dist_mle( 1 , 1 , 1000 )

    


