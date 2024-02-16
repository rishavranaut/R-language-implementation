Gompertz_dist<-function(num_samples,k){
  E<-c(); O<-c() ; w=0 ;Fn<-c();F0<-c(); D_pos<-c(); Fn_xk_1<-c();D_neg<-c()
  x1<-c(0.080, 0.084, 0.102, 0.124, 0.326, 0.358, 0.412, 0.444, 0.456, 0.504, 0.498, 0.564, 0.648,
        0.666, 0.682, 0.732, 0.770, 0.814, 0.840, 0.862, 0.882, 0.922, 0.924, 0.964, 1.034, 1.034,
        1.048, 1.128, 1.134, 1.172, 1.238, 1.240, 1.242, 1.244, 1.294, 1.302, 1.372, 1.522, 1.526)
  sort(x1)
  mle<-function(x){
    beta<-x[1]
    eeta<-x[2]
    
    eqn1 <-  (num_samples/beta) + sum(x1) - eeta * sum( exp(beta*x1-1) * x1)
    eqn2 <-  (num_samples/eeta) -sum(exp(beta * x1 -1) )
    return(c(eqn1,eqn2))
  }
  
  ans_beta<-nleqslv(c(1.2,1.2),mle)$x[1]
  ans_eeta<-nleqslv(c(1.2,1.2),mle)$x[2]
  print(paste("mle of beta is ",ans_beta))
  print(paste("mle of eeta is ",ans_eeta))
  
  mini=min(x1)
  maxi=max(x1)
  step=(maxi-mini)/k
  
  for( j in 1:k){
    m1=mini
    mini=m1+step
    freq=0
    for( i in 1:num_samples){
      if (x1[i]>=m1 && x1[i]<=mini){
         freq=freq+1
      }
      O[j] <- freq
      e <- num_samples * ( (1- exp(-ans_eeta*(exp(ans_beta*mini-1)))) - (1- exp(-ans_eeta*(exp(ans_beta*m1-1)))) )
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
    
    F0[z]<-(1- exp(-ans_eeta*(exp(ans_beta*x1[z]-1))))
    
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
Gompertz_dist(39,10)








