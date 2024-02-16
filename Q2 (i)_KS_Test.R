
# #verifying if p norm working or not
# x=c()
# n=10
# for(i in 1:n){
#   x[i]<-rnorm(1,0,1)
# }
# x=sort(x)
# x
# pnorm(0.03778840, mean = 0, sd = 1)       should be near to 0.512



# Q2. ____________(i)_____________
nomral_dist_kstest<-function(mew,sigma,n){
  k=c() ; x=c() ; Fn=c() ; Fo=c() ; D_pos=c() ; Fn_x_k_1=c() ; D_neg=c()
  x=c()
  
  for(i in 1:n){
    x[i]<-rnorm(1,mew,sigma)
  }
  x=sort(x)
  for ( i in 1:n){
    k[i]<-i
    
    Fn[i]<-i/n
    
    Fo[i]<- pnorm(x[i], mean = mew, sd = sigma) # for finding integration <= given point x[i]
    
    D_pos[i]<-abs(Fo[i]-Fn[i])
    
    Fn_x_k_1[i]<-(i-1)/n
    
    D_neg[i]<-abs(Fo[i]-Fn_x_k_1[i])
  }
  D = max(max(D_pos),max(D_neg))
  if(D<0.270){
    print("hypothesis accepted")
    return(D)
  }
  else{
    print(D)
    print("oops!! hypothesis not accepted")
  }

}
nomral_dist_kstest(1,2,25)






