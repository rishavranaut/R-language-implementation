# Q2 __________(ii)_______________

expo_dist_kstest<-function(alpha,beta,n){
  k=c() ; x=c() ; Fn=c() ; Fo=c() ; D_pos=c() ; Fn_x_k_1=c() ; D_neg=c()
  x<-c()
  for(i in 1:n){
    y<-runif(1)
    x[i]<- (-1)*(1/beta)*log(1-(y^(1/alpha)))
  }
  x=sort(x)
  for ( i in 1:n){
    k[i]<-i
    
    Fn[i]<-i/n
    
    Fo[i]<-  (1 - exp(- beta * x[i]) )^alpha
      
    D_pos[i]<-abs(Fo[i]-Fn[i])
    
    Fn_x_k_1[i]<-(i-1)/n
    
    D_neg[i]<-abs(Fo[i]-Fn_x_k_1[i])
  }
  D = max(max(D_pos),max(D_neg))
  if(D<0.215){
    print("hypothesis accepted")
    return(D)
  }
  else{
    print(D)
    print("oops!! hypothesis not accepted")
  }
  
}
expo_dist_kstest(1,2,40)





