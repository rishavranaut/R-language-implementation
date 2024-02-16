
# 
# # method to check if given cdf will fit or not
# x=c()
# for(i in 1:10){
#   x[i]<-runif(1,2,5)
# }
# x=sort(x)
# F<-function(y){
#   return((y-2)/3)
# }
# 
# ks.test(x,F)



# Q1 ______________(i)______________
#if n=10 ,significance =0.05 then value should be <0.410
uniform_dist_Kstest<-function(n){
  k=c() ; x=c() ; Fn=c() ; Fo=c() ; D_pos=c() ; Fn_x_k_1=c() ; D_neg=c()
  
  for(i in 1:n){
    x[i]<-runif(1,2,5)
  }
  x=sort(x)
  for ( i in 1:n){
    k[i]<-i
    Fn[i]<-i/n
    Fo[i]<-(x[i]-2)/3
    D_pos[i]<-abs(Fo[i]-Fn[i])
    Fn_x_k_1[i]<-(i-1)/n
    D_neg[i]<-abs(Fo[i]-Fn_x_k_1[i])
  }
  D=max(max(D_pos),max(D_neg))
  if(D<0.410){
    print("hypothesis accepted")
    return(D)
  }
  else{
    print("oops!! hypothesis not accepted")
  }
}
uniform_dist_Kstest(10) 



