# Q1._____________(i)__________#
library(nleqslv)

# theta is t here.
sample_space_lindley_dist_mle <- function(t, num_samples) {
  x <- c()
  
  for (i in 1:num_samples) {
    y <- runif(1)
    
    f <- function(x) {
      return(y - 1 + ((t + 1 + t * x) / (t + 1)) * exp(-t * x))
    }
    
    x[i] <- nleqslv( 1.2,f)$x      # using $x to retrieve root only 
    
  }
  theta_eqn<-function(theta){
    sum1=sum(x)
    theta_eqn<- sum1 - (num_samples *(theta+2))/(theta*(theta+1))
    return(theta_eqn)
    
  }
  theta_mle=nleqslv(1.1,theta_eqn)$x
  print("theta mle is")
  return(theta_mle)
}

sample_space_lindley_dist_mle(2,1000)

#________________(ii)______________#


library(nleqslv)

sample_space_weibull_dist <- function(beta, gamma, lambda, num_samples) {
  x <- c()  
  
  for (i in 1:num_samples) {
    y <- runif(1)
    
    fun <- function(x) {
      return((x^gamma) * exp(lambda * x) + (1/beta) * log(1 - y))
    }
    
    x[i] <- nleqslv(1.2, fun)$x
  }
  return(x)
}
  
 

# Example usage:
result <- sample_space_weibull_dist(1, 5, 3, 1000)
print(result)





















