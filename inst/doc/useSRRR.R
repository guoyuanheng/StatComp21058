## -----------------------------------------------------------------------------
SRRR<-function(X,Y,lambda,r){
  
  n=nrow(X)
  p=ncol(X)
  q=ncol(Y)
  f0<-c()
  for(l in 1:p){
    f0[l]<-t(X[,l])%*%X[,l]+0.000001
  }
  int<-0
  
  Sxx<-t(X)%*%X/n
  Sxy<-t(X)%*%Y/n
  Syx<-t(Y)%*%X/n
  S<-Syx%*%solve(Sxx)%*%Sxy           
  A<-eigen(S)$vector[,1:r]
  A<-as.matrix(A)
  B<-solve(Sxx)%*%Sxy%*%A
  Bhat<-matrix(0,ncol = r,nrow = p)
  B0<-matrix(1,ncol = r,nrow = p)
  check=c(10000,0)
  
  while(abs(check[1]-check[2])>0.0001){
    
    A<-svd(t(Y)%*%X%*%B)$u%*%t(svd(t(Y)%*%X%*%B)$v)
    
    while(norm(B0,"F")>0.0001){
      for(l in 1:p){
        Rl<-Y%*%A-X%*%B+X[,l]%*%t(B[l,])
        s<-lambda[l]/2/(norm(t(X[,l])%*%Rl,"F")+0.000001)
        if(s<1){
          Bhat[l,]<-t(X[,l])%*%Rl*(1-s)/f0[l]
        }else Bhat[l,]<-rep(0,r)
      }
      B0<-B-Bhat
      B<-Bhat
      int<-int+1
      if(int==500) return(list(A,B,int))
    }
    
    
    c=0
    for(i in 1:p){
      c=c+lambda[i]*norm(as.matrix(B[i,]),"F")
    }
    check[1]<-check[2]
    check[2]<-norm(Y-X%*%B%*%t(A),"2")+c
  }
  return(list(A,B,int))
}
SRRR2<-function(X,Y,lambda,r){
  n=nrow(X)
  p=ncol(X)
  q=ncol(Y)
  int<-0
  B<-matrix(0,ncol = r,nrow = p)
  for(i in 1:r){B[i,i]<-1}
  Bhat<-matrix(0,ncol = r,nrow = p)
  B0<-matrix(1,ncol = r,nrow = p)
  
  check=c(10000,0)
  
  while(abs(check[1]-check[2])>0.01){
    A<-svd(t(Y)%*%X%*%B)$u%*%t(svd(t(Y)%*%X%*%B)$v)
    while(norm(B0,"F")>0.01){
      mu<-c()
      lammu<-c()
      for(i in 1:p){
        mu[i]<-1/(norm(as.matrix(B[i,]),"F")+0.00000001)
        lammu[i]<-lambda[i]*mu[i]
      }
      Bhat<-solve(t(X)%*%X+diag(lammu,nrow=p,ncol=p)/2)%*%t(X)%*%Y%*%A
      B0<-B-Bhat
      B<-Bhat
      int<-int+1
      if(int==5000) return(list(A,B,int))
    }
    
    c=0
    for(i in 1:p){
      c=c+lambda[i]*norm(as.matrix(B[i,]),"F")
    }
    check[1]<-check[2]
    check[2]<-norm(Y-X%*%B%*%t(A),"2")+c
  }
  return(list(A,B,int))
}

## -----------------------------------------------------------------------------
library(MASS)
n<-100
p=30
p0=10
q=10
r=3
rowx=0
rowe=0
sigmaX<-diag(1,nrow = p,ncol = p)
X<-mvrnorm(n,rep(0,p),sigmaX)
Bt<-matrix(c(rnorm(p0*r,0,1),rep(0,(p-p0)*r)),nrow = r,ncol = p)
B<-t(Bt)
A<-matrix(rnorm(q*r,0,1),nrow = q,ncol = r)
C=B%*%t(A)
sigma2<-sum(diag((t(C)%*%C)))/q
sigmaE<-diag(sqrt(sigma2),nrow = q,ncol = q)
E<-mvrnorm(n,rep(0,q),sigmaE)
Y<-X%*%B%*%t(A)+E
lambda<-rep(55,p)
SRRR(X,Y,lambda,3)

## -----------------------------------------------------------------------------
SRRR2(X,Y,lambda,3)

