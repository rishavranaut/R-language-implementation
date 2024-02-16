#rishav_ranaut
#2311mc12


# _______ Q3 (iii)_________ kumara swamy dist.

# k is the number of intervals we are breaking in.

# Hypothesis
# H0 : sample fits the data
# Ha: not .


chisquare_test <- function( k , beta , alpha , num_samples){
  
  #generating sample space
  sample <-c()
  for(i in 1:num_samples ){
    
    y<-runif(1)
    
    sample[i]<- (1-(1-y)^(1/beta))^(1/alpha)
    
  }
  
  maxi = max ( sample )
  mini = min ( sample )
  
  #increment is to break the x-range into k equal parts..
  O=c() ; E=c() ; increment = ( maxi- mini)/k  ; W=0
  
  # counting the observed number of elements in the desired range.
  for ( j in 1:k){
    m1=mini
    mini = m1 + increment
    freq = 0
    for( i in 1:num_samples){
      if(sample[i]>=m1 && sample[i]<=mini){
        
        freq = freq + 1
      }
      
    }
    O[j]<-freq
    
    #finding expected frequency
    e =   num_samples * ( (( 1-(1-mini^alpha)^beta  )) - (( 1-(1-m1^alpha)^beta )) )
     
    
    E[j]<-e
  }
  
  print(" Observed frequency  is")
  print(O)
  
  print(" Expected frequency is  ")
  print(E)
  
  #finding W
  for( p in 1:k){
    W = W + (O[p]-E[p])^2/E[p]
  }
  print("W equivalent to chi-square(k-1) is")
  
  print(W)
  
  # from chi square table for k-1 = 4 ,0.05 
  # w should be < =9.49
  
  if( W <=9.49  ) {
    print( " we failed to reject the hypothesis and the value of W is " )
    print ( W )
  }else{
    print(" we reject the hypothesis and value of W is ") 
    print( W )
  }
}
#pass k the number of intervals we are breaking and num of samples required 
#for the samples and alpha, beta for sample generation

chisquare_test(5,4,6,1000)






