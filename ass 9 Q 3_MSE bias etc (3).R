Burr_X_dist<-function(num_samples,k){
  E<-c(); O<-c() ; w=0 ;Fn<-c();F0<-c(); D_pos<-c(); Fn_xk_1<-c();D_neg<-c()
  x1<-c(0.70, 0.84, 0.58, 0.50, 0.55, 0.82, 0.59, 0.71, 0.72, 0.61, 0.62, 0.49, 0.54, 0.36, 0.36, 0.71, 0.35,
        0.64, 0.85, 0.55, 0.59, 0.29, 0.75, 0.46, 0.46, 0.60, 0.60, 0.36, 0.52, 0.68, 0.80, 0.55, 0.84, 0.34,
        0.34, 0.70, 0.49, 0.56, 0.71, 0.61, 0.57, 0.73, 0.75, 0.44, 0.44, 0.81, 0.80, 0.87, 0.29, 0.50)
  sort(x1)
  mle<-function(x){
    c<-x[1]
    k<-x[2]
    
    eqn1 <-  (2* num_samples/c)- 2*sum(c*x1^2) + (k-1)* sum( (2*c^2*x1*exp(-(c*x1)^2) ) / ( 1-exp(-(c*x1)^2) ) )
    eqn2 <-  (num_samples/k) + sum( log ( 1- exp(-(c*x1)^2) ) )
    return(c(eqn1,eqn2))
  }
  
  ansc<-nleqslv(c(1.2,1.2),mle)$x[1]
  ansk<-nleqslv(c(1.2,1.2),mle)$x[2]
  print(paste("mle of c is ",ansc))
  print(paste("mle of k is ",ansk))
  
  mini=min(x1)
  maxi=max(x1)
  step=(maxi-mini)/k
  
  for( j in 1:k){
    m1=mini
    mini=m1+step
    freq=0
    for( i in 1:num_samples){
      if(x1[i]>=m1 && x1[i]<=mini){
        freq=freq+1
      }
      O[j] <- freq
      e <- num_samples * (( ( 1 - exp( -(ansc*mini)^2 ) )^ansk - ( 1 - exp( -(ansc*m1)^2 ) )^ansk ))
      E[j] <- e
    }
  }
  print("observed frequency is")
  print(O)
  print("expected frequency is")
  print(E)
  
  for( p in 1:k){
    w = w + (( O[p]-E[p] )^2)/E[p]
  }
  print("value of w is ")
  print(w)
  print("according to chisquare test _W_ is equivalent to chisq(k-1-2) i.e chisq(7) if k=10 at 0.05  level of significance ")
  if (w<14.07){
    print("we failed to reject the hypothesis that given data fits BURR_X distn")
  }else{
    print("we reject the hypothesis that given data fits BURR_X distn")
  }
  for( z in 1:num_samples){
    
    Fn[z]<-z/num_samples
    
    F0[z]<-(1 - exp(-(ansc*x1[z])^2 ) )
    
    D_pos[z]<-abs(Fn[z]-F0[z])
    
    Fn_xk_1[z]<-(z-1)/num_samples
    
    D_neg[z]<-abs(F0[z]-Fn_xk_1[z])
  }
  # print("empirical cdf is")
  # print(Fn)
  # print("theoretical cdf is")
  # print(F0)
  # print("d pos is")
  # print(D_pos)
  # print("lower empirical cdf is")
  # print(Fn_xk_1)
  # print("D neg is")
  # print(D_neg)
  
  D=max(D_pos,D_neg)
  print("according to ks test n=50 and at 0.05 level of significance value should be 0.192")
  print("value of D is ")
  print (D)
  
  
  if(D<0.192){
    print("we failed to reject the hypothesis that empirical cdf is equal to theoretical cdf ")
  }else{
    print("we reject the hypothesis that empirical cdf is equal  theoretical cdf")
  }
  
}
Burr_X_dist(50,10)







