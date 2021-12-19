## -----------------------------------------------------------------------------
a<-"hello world"
a

substr(a,7,11)

aa<-c("hello","world")
aa
aa[1]


## -----------------------------------------------------------------------------
b<-rnorm(100)
plot(1:100,sort(b),type="l")


## -----------------------------------------------------------------------------
c<-table(c(1,2,3,4,5,6,5,4,3,2,1))
c



## -----------------------------------------------------------------------------
set.seed(2021917)
matrix(rexp(9),nrow = 3,ncol = 3)


## -----------------------------------------------------------------------------
d<-data.frame(1,1:10)

d[1:10,2]

## -----------------------------------------------------------------------------
n <- 1000
sigma=0.1
u <- runif(n)
x <- sqrt(-2*sigma^2*log(1-u))
hist(x, prob = TRUE, main = expression(f(x)==x/sigma^2*exp(-x^2/(2*sigma^2))))
y <- seq(0, 0.4, .01)
lines(y, y/sigma/sigma*exp(-y^2/2/sigma/sigma),col=2)
legend(0.3,3,legend="sigma=0.1")

## -----------------------------------------------------------------------------
n <- 1000
sigma=0.3
u <- runif(n)
x <- sqrt(-2*sigma^2*log(1-u))
hist(x, prob = TRUE, main = expression(f(x)==x/sigma^2*exp(-x^2/(2*sigma^2))))
y <- seq(0, 1.4, .01)
lines(y, y/sigma/sigma*exp(-y^2/2/sigma/sigma),col=2)
legend(0.8,1.5,legend="sigma=0.3")

## -----------------------------------------------------------------------------
n <- 1000
sigma=2
u <- runif(n)
x <- sqrt(-2*sigma^2*log(1-u))
hist(x,breaks = 15, prob = TRUE, main = expression(f(x)==x/sigma^2*exp(-x^2/(2*sigma^2))))
y <- seq(0, 6, .001)
lines(y, y/sigma/sigma*exp(-y^2/2/sigma/sigma),col=2)
legend(6,0.1,legend="sigma=2")

## -----------------------------------------------------------------------------
n<-1000
x <- rnorm(n) 
y <- rnorm(n,mean = 3)
p1=0.75
p2=0.25
xx<-sample(x,size = n*p1)
yy<-sample(y,size = n*p2)
a<-c(xx,yy)
hist(a)

## -----------------------------------------------------------------------------
n<-1000
x <- rnorm(n) 
y <- rnorm(n,mean = 3)
p1=0.25
p2=0.75
xx<-sample(x,size = n*p1)
yy<-sample(y,size = n*p2)
a<-c(xx,yy)
hist(a)

## -----------------------------------------------------------------------------
n<-1000
x <- rnorm(n) 
y <- rnorm(n,mean = 3)
p1=0.5
p2=0.5
xx<-sample(x,size = n*p1)
yy<-sample(y,size = n*p2)
a<-c(xx,yy)
hist(a)

## -----------------------------------------------------------------------------
a<-c()
for(j in 1:100){
N<-rpois(10,5)
x0<-rgamma(N,1)
a[j]<-(sum(x0[1:10]))
}
a
mean(a)
var(a)

## -----------------------------------------------------------------------------
a<-c()
for(j in 1:100){
N<-rpois(10,5)
x0<-rgamma(N,3)
a[j]<-(sum(x0[1:10]))
}
mean(a)
var(a)

## -----------------------------------------------------------------------------
a<-c()
for(j in 1:100){
N<-rpois(10,10)
x0<-rgamma(N,3)
a[j]<-(sum(x0[1:10]))
}
a
mean(a)
var(a)

## -----------------------------------------------------------------------------
pbetaa<-function(xx){
  m=1e4
  x=runif(m,min=0,max=xx)
  theta=mean(30*xx*x^2*(1-x)^2)
  return(theta)
}
print(c(pbetaa(0.1),pbeta(0.1,3,3)))
print(c(pbetaa(0.2),pbeta(0.2,3,3)))
print(c(pbetaa(0.3),pbeta(0.3,3,3)))
print(c(pbetaa(0.4),pbeta(0.4,3,3)))
print(c(pbetaa(0.5),pbeta(0.5,3,3)))
print(c(pbetaa(0.6),pbeta(0.6,3,3)))
print(c(pbetaa(0.7),pbeta(0.7,3,3)))
print(c(pbetaa(0.8),pbeta(0.8,3,3)))
print(c(pbetaa(0.9),pbeta(0.9,3,3)))


## -----------------------------------------------------------------------------
Ra<-1000
x<-1
u <- runif(Ra/2)
v <- 1 - u
g<- numeric(length=(Ra/2))
sigma=0.1

for (i in 1:(Ra/2)) {
g[i] <- x*u[i] * exp(-u[i]^2 / (2*sigma^2))/sigma^2
g[i] <- g[i]+x*v[i] * exp(-v[i]^2 / (2*sigma^2))/sigma^2
g[i] <- g[i]/2
}
var(g)
g1<-var(g)

u <- runif(Ra/2)
v <- runif(Ra/2)
g<- numeric(length=(Ra/2))

for (i in 1:(Ra/2)) {
g[i] <- x*u[i] * exp(-u[i]^2 / (2*sigma^2))/sigma^2
g[i] <- g[i]+x*v[i] * exp(-v[i]^2 / (2*sigma^2))/sigma^2
g[i] <- g[i]/2
}
var(g)

g2<-var(g)
(g2-g1)/g2*100


## -----------------------------------------------------------------------------
m <- 1000
theta.hat <- se <- numeric(5)
g <- function(x) {
exp(-x^2/2 +log(x^2/sqrt(2*pi)))*(x>1)
}

g0 <- function(x) {
exp(-x^2/2 +log(x^2/sqrt(2*pi)))*(x<1)*(x>0)
}

x <- runif(m) #f0=1
fg <- 1/2-g0(x)
theta.hat[1] <- mean(fg)
se[1] <- sd(fg)

u<-runif(m)
x <- -log(1-u) #f1=e^-x
fg <- 1/2-g0(x) / exp(-x)
theta.hat[2] <- mean(fg)
se[2] <- sd(fg)

u <- runif(m,0,1) #f2=xe^{-x^2/2}, x>0,inverse transform method
x <- sqrt(- 2*log(1-u))
fg <-1/2- g0(x) / x/(exp(-x^2/2) )
theta.hat[3] <- mean(fg)
se[3] <- sd(fg)

u <- runif(m,0,1) #f3=2xe^{-x^2/2}, x>0,inverse transform method
x <- sqrt(- 2*log(1-u/2))
fg <-1/2-g0(x) / x/(exp(-x^2/2) )/2
theta.hat[4] <- mean(fg)
se[4] <- sd(fg)

u <- runif(m,0,1) #f4=3xe^{-x^2/2}, x>0,inverse transform method
x <- sqrt(- 2*log(1-u/3))
fg <-1/2-g0(x) / x/(exp(-x^2/2) )/3
theta.hat[5] <- mean(fg)
se[5] <- sd(fg)

theta.hat
se


## -----------------------------------------------------------------------------
n<-20
alpha=0.05
t<-qt(0.975,(n-2))
re<-0
for(i in 1:1000){
  x<-rchisq(n,2)
  if(((mean(x)-t*sd(x))<2)&(2<(mean(x)+t*sd(x)))) re<-re+1
  else re<-re
}
re/1000

## -----------------------------------------------------------------------------
alpha <- 1; beta <- 0 # null hypothesis!
m <- 1e3; n <- 10; set.seed(123)
beta.hat <- beta.se <- p.val1 <- numeric(m)
x <- rchisq(n,1)
for(i in 1:m){
y <- alpha + beta * x + rt(n,2)
coe <- summary(lm(y~x))$coef
beta.hat[i] <- coe[2,1];beta.se[i] <- coe[2,2]
p.val1[i] <- coe[2,4] # t-test p-value
}
p.val2 <- 2*(1-pt(abs(beta.hat/beta.se),n-2))
print(c(mean(p.val1<=0.05),mean(p.val2<=0.05)))

## -----------------------------------------------------------------------------
alpha <- 1; beta <- 0 # null hypothesis!
m <- 1e3; n <- 10; set.seed(123)
beta.hat <- beta.se <- p.val1 <- numeric(m)
x <- runif(n,0,2)
for(i in 1:m){
y <- alpha + beta * x + rt(n,2)
coe <- summary(lm(y~x))$coef
beta.hat[i] <- coe[2,1];beta.se[i] <- coe[2,2]
p.val1[i] <- coe[2,4] # t-test p-value
}
p.val2 <- 2*(1-pt(abs(beta.hat/beta.se),n-2))
print(c(mean(p.val1<=0.05),mean(p.val2<=0.05)))

## -----------------------------------------------------------------------------
alpha <- 1; beta <- 0 # null hypothesis!
m <- 1e3; n <- 10; set.seed(123)
beta.hat <- beta.se <- p.val1 <- numeric(m)
x <- rexp(n)
for(i in 1:m){
y <- alpha + beta * x + rt(n,2)
coe <- summary(lm(y~x))$coef
beta.hat[i] <- coe[2,1];beta.se[i] <- coe[2,2]
p.val1[i] <- coe[2,4] # t-test p-value
}
p.val2 <- 2*(1-pt(abs(beta.hat/beta.se),n-2))
print(c(mean(p.val1<=0.05),mean(p.val2<=0.05)))

## -----------------------------------------------------------------------------
a<-matrix(c(6510,3490,6760,3240),ncol = 2)
xq<-0
for(i in 1:2){
  for(j in 1:2){
   xq<-xq+a[i,j]^2/sum(a[i,])/sum(a[,j])
  }
}
20000*(xq-1)

## -----------------------------------------------------------------------------
library(mvtnorm)
n <- c(10, 20, 30, 50, 100, 500) #sample sizes
d<-2
cv <- qchisq(0.95,d*(d+1)*(d+2)/6) #crit. values for each n
p.reject <- numeric(length(n)) #to store sim. results
m <- 1000 #num. repl. each sim.

sk <- function(x,n) {#computes the sample skewness coeff.
xbar <- colMeans(x)
sig<-solve((n-1)*cov(x)/n)
bb<-function(x) x-xbar
y<-apply(x,1,bb)
y<-t(y)
z<-(y%*%sig%*%t(y))^3
return(mean(z)/6)
}

for (i in 1:length(n)) {
sktests <- numeric(m) #test decisions
for (j in 1:m) {
x <- rmvnorm(n[i],rep(0,d),diag(d))
#test decision is 1 (reject) or 0
sktests[j] <- as.integer(n[i]*abs(sk(x,n[i])) >= cv )
}
p.reject[i] <- mean(sktests) #proportion rejected
}
p.reject

## -----------------------------------------------------------------------------
alpha <- .1
n <- 30
m <- 500
epsilon <- c(seq(0, .15, .01), seq(.15, 1, .05))
N <- length(epsilon)
pwr <- numeric(N)
x<-matrix(0,nrow=n,ncol=d)
#critical value for the skewness test
cv <- qchisq(1-alpha, d*(d+1)*(d+2)/6)
for (j in 1:N) { #for each epsilon
  e <- epsilon[j]
  sktests <- numeric(m)
  
  for (i in 1:m) { #for each replicate
    sigma <- sample(c(1, 10), replace = TRUE,size = n, prob = c(1-e, e))
    for(k in 1:n) {
      t<-rmvnorm(1, rep(0,d),diag(rep(sigma[k],d)))
      x[k,] <-t(as.matrix(t[1,]))
      }
    sktests[i] <- as.integer(n*abs(sk(x,n)) >= cv)
}
pwr[j] <- mean(sktests)
}
#plot power vs epsilon
plot(epsilon, pwr, type = "b",
xlab = bquote(epsilon), ylim = c(0,1))
abline(h = .1, lty = 3)
se <- sqrt(pwr * (1-pwr) / m) #add standard errors
lines(epsilon, pwr+se, lty = 3)
lines(epsilon, pwr-se, lty = 3)

## -----------------------------------------------------------------------------
data(patch, package = "bootstrap")
n <- nrow(patch)
B<-100
fpc<-c()
for(b in 1:B){
xstar <- sample(c(1:n),replace=TRUE)
patchstar<-patch[xstar,2:6]
r<-eigen((n-1)*cov(patchstar)/n)$values
fpc[b]<-r[1]/sum(r)
}
r0<-eigen((n-1)*cov(patch[,2:6])/n)$values
fpc0<-r0[1]/sum(r0)
fpcstar<-mean(fpc)
sefpc<-sd(fpc)
round(c(original=fpc0,bias=fpcstar-fpc0,se=sefpc),3)

## -----------------------------------------------------------------------------
fpcj<-numeric(n)
for (i in 1:n){
patchi<-patch[-i,2:6]
r<-eigen((n-1)*cov(patchi)/n)$values
fpcj[i]<-r[1]/sum(r)
}
bias <- (n - 1) * (mean(fpcj) - fpc0)
se <- sqrt((n-1)*mean((fpcj - mean(fpcj))^2))
round(c(original=fpc0,bias=bias,se=se),3)

## -----------------------------------------------------------------------------
library(boot)
boot.fpc<-function(x,i){
  n<-nrow(x)
  r<-eigen((n-1)*cov(x[i,1:5])/n)$values
  fpc<-r[1]/sum(r)
  fpc
}
de<-boot(data = patch[,2:6],statistic = boot.fpc,R = 999)
ci<-boot.ci(de,type = c("perc","bca"))
ci.perc<-ci$percent[4:5]
ci.bca<-ci$bca[4:5]
ci.perc
ci.bca

## -----------------------------------------------------------------------------
boot.ske<-function(x,i) mean(((x[i]-mean(x[i]))/sd(x[i]))^3)
ci.norm<-ci.basic<-ci.perc<-ci.bca<-matrix(NA,1000,2)

for(i in 1:100){
  x<-rnorm(100)
  de<-boot(data = x,statistic = boot.ske,R=99)
  ci<-boot.ci(de,type = c("norm","basic","perc"))
  ci.norm[i,]<-ci$norm[2:3]
  ci.basic[i,]<-ci$basic[4:5]
  ci.perc[i,]<-ci$percent[4:5]
}
c(mean(ci.norm[,1]),mean(ci.norm[,2]))
c(mean(ci.basic[,1]),mean(ci.basic[,2]))
c(mean(ci.perc[,1]),mean(ci.perc[,2]))

mean(ci.norm[,1]>0)
mean(ci.norm[,2]<0)
mean(ci.basic[,1]>0)
mean(ci.basic[,2]<0)
mean(ci.perc[,1]>0)
mean(ci.perc[,2]<0)

1-mean(ci.norm[,1]>0)-mean(ci.norm[,2]<0)
1-mean(ci.basic[,1]>0)-mean(ci.basic[,2]<0)
1-mean(ci.perc[,1]>0)-mean(ci.perc[,2]<0)


## -----------------------------------------------------------------------------
a<-sqrt(8/5)
for(i in 1:100){
  x<-rchisq(100,5)
  de<-boot(data = x,statistic = boot.ske,R=99)
  ci<-boot.ci(de,type = c("norm","basic","perc"))
  ci.norm[i,]<-ci$norm[2:3]
  ci.basic[i,]<-ci$basic[4:5]
  ci.perc[i,]<-ci$percent[4:5]
}
c(mean(ci.norm[,1]),mean(ci.norm[,2]))
c(mean(ci.basic[,1]),mean(ci.basic[,2]))
c(mean(ci.perc[,1]),mean(ci.perc[,2]))

mean(ci.norm[,1]>a)
mean(ci.norm[,2]<a)
mean(ci.basic[,1]>a)
mean(ci.basic[,2]<a)
mean(ci.perc[,1]>a)
mean(ci.perc[,2]<a)

1-mean(ci.norm[,1]>a)-mean(ci.norm[,2]<a)
1-mean(ci.basic[,1]>a)-mean(ci.basic[,2]<a)
1-mean(ci.perc[,1]>a)-mean(ci.perc[,2]<a)


## -----------------------------------------------------------------------------
for(j in 1:10){
x<-rchisq(30,5)
y<-rchisq(30,10)
R <- 99;z <- c(x, y);K <- 1:60;n<-length(x);
reps <- numeric(R);t0 <- cor(x, y,method = "spearman")
for (i in 1:R) {
k <- sample(K, size = n, replace = FALSE)
x1 <- z[k];y1 <- z[-k] #complement of x1
reps[i] <- cor(x1, y1,method = "spearman")
}
p <- mean(abs(c(t0, reps)) >= abs(t0))
p0<-cor.test(x,y,method = "spearman")$p.value
print(c(t0,p,p0))
}

## -----------------------------------------------------------------------------

library(RANN)
library(boot)
library(energy)
library(Ball)
Tn <- function(z, ix, sizes,k) {
n1 <- sizes[1]; n2 <- sizes[2]; n <- n1 + n2
if(is.vector(z)) z <- data.frame(z);
z <- z[ix, ];
NN <- nn2(data=z, k=k+1) # What is the first column?
block1 <- NN$nn.idx[1:n1,-1]
block2 <- NN$nn.idx[(n1+1):n,-1]
i1 <- sum(block1 <= n1); i2 <- sum(block2 > n1)
(i1 + i2) / (k * n)
}
m <- 100; k<-3; p<-2; set.seed(12345)
n1 <- n2 <- 10; R<-99; n <- n1+n2; N = c(n1,n2)
eqdist.nn <- function(z,sizes,k){
boot.obj <- boot(data=z,statistic=Tn,R=R,
sim = "permutation", sizes = sizes,k=k)
ts <- c(boot.obj$t0,boot.obj$t)
p.value <- mean(ts>=ts[1])
list(statistic=ts[1],p.value=p.value)}
p.values <- matrix(NA,m,3)
for(i in 1:m){
x <- matrix(rnorm(n1*p,0,1),ncol=p)
y <- matrix(rnorm(n1*p,0,3),ncol=p)
z <- rbind(x,y)
p.values[i,1] <- eqdist.nn(z,N,k)$p.value
p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
p.values[i,3] <- bd.test(x=x,y=y,num.permutations=R,
seed=i*12345)$p.value
}
alpha <- 0.1;
pow <- colMeans(p.values<alpha)
pow

## -----------------------------------------------------------------------------

m <- 100; k<-3; p<-2; set.seed(12345)
n1 <- n2 <- 10; R<-99; n <- n1+n2; N = c(n1,n2)
eqdist.nn <- function(z,sizes,k){
boot.obj <- boot(data=z,statistic=Tn,R=R,
sim = "permutation", sizes = sizes,k=k)
ts <- c(boot.obj$t0,boot.obj$t)
p.value <- mean(ts>=ts[1])
list(statistic=ts[1],p.value=p.value)}
p.values <- matrix(NA,m,3)
for(i in 1:m){
x <- matrix(rnorm(n1*p,0,1),ncol=p)
y <- matrix(rnorm(n1*p,1,3),ncol=p)
z <- rbind(x,y)
p.values[i,1] <- eqdist.nn(z,N,k)$p.value
p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
p.values[i,3] <- bd.test(x=x,y=y,num.permutations=R,
seed=i*12345)$p.value
}
alpha <- 0.1;
pow <- colMeans(p.values<alpha)
pow

## -----------------------------------------------------------------------------

m <- 100; k<-3; p<-2;set.seed(12345)
n1 <- n2 <- 10; R<-99; n <- n1+n2; N = c(n1,n2)
eqdist.nn <- function(z,sizes,k){
boot.obj <- boot(data=z,statistic=Tn,R=R,
sim = "permutation", sizes = sizes,k=k)
ts <- c(boot.obj$t0,boot.obj$t)
p.value <- mean(ts>=ts[1])
list(statistic=ts[1],p.value=p.value)}
p.values <- matrix(NA,m,3)
for(i in 1:m){
x <- matrix(rt(n1*p,1),ncol=p)
y <- cbind(rnorm(n2),rnorm(n2,1,1))
z <- rbind(x,y)
p.values[i,1] <- eqdist.nn(z,N,k)$p.value
p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
p.values[i,3] <- bd.test(x=x,y=y,num.permutations=R,
seed=i*12345)$p.value
}
alpha <- 0.1;
pow <- colMeans(p.values<alpha)
pow

## -----------------------------------------------------------------------------

m <- 100; k<-3; p<-2;set.seed(12345)
n1 <-1; n2 <- 10; R<-99; n <- n1+n2; N = c(n1,n2)
eqdist.nn <- function(z,sizes,k){
boot.obj <- boot(data=z,statistic=Tn,R=R,
sim = "permutation", sizes = sizes,k=k)
ts <- c(boot.obj$t0,boot.obj$t)
p.value <- mean(ts>=ts[1])
list(statistic=ts[1],p.value=p.value)}
p.values <- matrix(NA,m,3)
for(i in 1:m){
x <- matrix(rnorm(n1*p,0,1),ncol=p)
y <- matrix(rnorm(n2*p,0,1),ncol=p)
z <- rbind(x,y)
p.values[i,1] <- eqdist.nn(z,N,k)$p.value
p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
p.values[i,3] <- bd.test(x=x,y=y,num.permutations=R,
seed=i*12345)$p.value
}
alpha <- 0.1;
pow <- colMeans(p.values<alpha)
pow

## -----------------------------------------------------------------------------
f<-function(x){
  return(1/pi/(1+x^2))
}
m<-10000
x<-numeric(m)
x[1]<-rnorm(1,0,1)
k<-0
u<-runif(m)
for(i in 2:m){
  xt<-x[i-1]
  y<-rnorm(1,xt,1)
  num<-f(y)*dnorm(xt,y,1)
  den<-f(xt)*dnorm(y,xt,1)
  if(u[i]<=(num/den)) 
    x[i]<-y
  else{
    x[i]<-xt
    k<-k+1
  }
}
print(k/m)

## -----------------------------------------------------------------------------
index<-1001:10000
y1<-x[index]
plot(index,y1,type = "l")

## -----------------------------------------------------------------------------
a<-ppoints(100)
QR<-tan(pi*(a-0.5))
Q<-quantile(y1,a)
qqplot(QR,Q,xlab="li lun",ylab="yang ben")
hist(y1,freq = FALSE,ylim = c(0,0.35))
lines(QR,f(QR))

## -----------------------------------------------------------------------------
a<-c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
QRp<-tan(pi*(a-0.5))
Qp<-quantile(y1,a)
QRp
Qp

## -----------------------------------------------------------------------------
N<-5000
burn<-1000
X<-matrix(0,N,2)
a<-2
b<-4
n<-10
X[1,]<-c(5,0.5)
for(i in 2:N){
  x2<-X[i-1,2]
  X[i,1]<-rbinom(1,n,x2)
  x1<-X[i,1]
  X[i,2]<-rbeta(1,x1+a,n-x1+b)
}
X0<-X[(burn+1):N,]
plot(X0,cex=.5)

## -----------------------------------------------------------------------------
 Gelman.Rubin <- function(psi) {
        # psi[i,j] is the statistic psi(X[i,1:j])
        # for chain in i-th row of X
        psi <- as.matrix(psi)
        n <- ncol(psi)
        k <- nrow(psi)
        psi.means <- rowMeans(psi)     #row means
        B <- n * var(psi.means)        #between variance est.
        psi.w <- apply(psi, 1, "var")  #within variances
        W <- mean(psi.w)               #within est.
        v.hat <- W*(n-1)/n + (B/n)     #upper variance est.
        r.hat <- v.hat / W             #G-R statistic
        return(r.hat)
 }
chain1<-function(m,sigma=1){
f<-function(x){
  return(1/pi/(1+x^2))
}
x<-numeric(m)
x[1]<-rnorm(1,0,sigma)
u<-runif(m)
for(i in 2:m){
  xt<-x[i-1]
  y<-rnorm(1,xt,sigma)
  num<-f(y)*dnorm(xt,y,sigma)
  den<-f(xt)*dnorm(y,xt,sigma)
  if(u[i]<=(num/den)) 
    x[i]<-y
  else{
    x[i]<-xt
  }
}
return(x)
}
k<-4
n<-10000
b<-1000
X<-matrix(0, nrow=k, ncol=n)
for (i in 1:k){
  X[i, ] <- chain1(n,1)
}
  
#trace plots
plot(1:n,X[1,],type = "l")
lines(1:n,X[2,],type = "l",col=2)
lines(1:n,X[3,],type = "l",col=3)
lines(1:n,X[4,],type = "l",col=4)
#compute diagnostic statistics
psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
    psi[i,] <- psi[i,] / (1:ncol(psi))
#plot psi for the four chains

for (i in 1:k)
    plot((b+1):n,psi[i, (b+1):n], type="l",
            xlab='Index', ylab=bquote(phi))
rhat <- rep(0, n)
for (j in (b+1):n)
    rhat[j] <- Gelman.Rubin(psi[,1:j])
plot(rhat[(b+1):n], type="l", xlab="", ylab="R")
abline(h=1.2, lty=2)

## -----------------------------------------------------------------------------
n<-10000
X<-matrix(0, nrow=k, ncol=n)
for (i in 1:k){
  X[i, ] <- chain1(n,5)
}
  
#trace plots
plot(1:n,X[1,],type = "l")
lines(1:n,X[2,],type = "l",col=2)
lines(1:n,X[3,],type = "l",col=3)
lines(1:n,X[4,],type = "l",col=4)
#compute diagnostic statistics
psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
    psi[i,] <- psi[i,] / (1:ncol(psi))
#plot psi for the four chains

for (i in 1:k)
    plot((b+1):n,psi[i, (b+1):n], type="l",
            xlab='Index', ylab=bquote(phi))
rhat <- rep(0, n)
for (j in (b+1):n)
    rhat[j] <- Gelman.Rubin(psi[,1:j])
plot(rhat[(b+1):n], type="l", xlab="", ylab="R")
abline(h=1.2, lty=2)

## -----------------------------------------------------------------------------
chain2<-function(a,b,n,N){
A<-c()
B<-c()
A[1]<-n/2
B[1]<-0.5
for(i in 2:N){
  x2<-B[i-1]
  A[i]<-rbinom(1,n,x2)
  x1<-A[i]
  B[i]<-rbeta(1,x1+a,n-x1+b)
}
return(A)
}
k<-4
n<-5000
b<-1000
X<-matrix(0, nrow=k, ncol=n)
for (i in 1:k){
  X[i, ] <- chain2(2,4,10,n)
}
  
#trace plots
plot(1:n,X[1,],type = "l")
plot(1:n,X[2,],type = "l",col=2)
plot(1:n,X[3,],type = "l",col=3)
plot(1:n,X[4,],type = "l",col=4)
#compute diagnostic statistics
psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
    psi[i,] <- psi[i,] / (1:ncol(psi))
#plot psi for the four chains

for (i in 1:k)
    plot((b+1):n,psi[i, (b+1):n], type="l",
            xlab='Index', ylab=bquote(phi))
rhat <- rep(0, n)
for (j in (b+1):n)
    rhat[j] <- Gelman.Rubin(psi[,1:j])
plot(rhat[(b+1):n], type="l", xlab="", ylab="R")
abline(h=1.2, lty=2)

## -----------------------------------------------------------------------------
chain2<-function(a,b,n,N){
A<-c()
B<-c()
A[1]<-n/2
B[1]<-0.5
for(i in 2:N){
  x2<-B[i-1]
  A[i]<-rbinom(1,n,x2)
  x1<-A[i]
  B[i]<-rbeta(1,x1+a,n-x1+b)
}
return(B)
}
k<-4
n<-5000
b<-1000
X<-matrix(0, nrow=k, ncol=n)
for (i in 1:k){
  X[i, ] <- chain2(2,4,10,n)
}
  
#trace plots
plot(1:n,X[1,],type = "l")
plot(1:n,X[2,],type = "l",col=2)
plot(1:n,X[3,],type = "l",col=3)
plot(1:n,X[4,],type = "l",col=4)
#compute diagnostic statistics
psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
    psi[i,] <- psi[i,] / (1:ncol(psi))
#plot psi for the four chains

for (i in 1:k)
    plot((b+1):n,psi[i, (b+1):n], type="l",
            xlab='Index', ylab=bquote(phi))
rhat <- rep(0, n)
for (j in (b+1):n)
    rhat[j] <- Gelman.Rubin(psi[,1:j])
plot(rhat[(b+1):n], type="l", xlab="", ylab="R")
abline(h=1.2, lty=2)

## -----------------------------------------------------------------------------
set.seed(12345)
kth<-function(a,k,d){
  s<-1
  if(k%%2==1) s<--1
  g<-exp(lgamma((d+1)/2)+lgamma(k+3/2)-lgamma(k+d/2+1))
  e<-0
  for(j in 1:d){
    e<-e+a[j]^2
  }
  f<-0
  fac<-1
  for(i in 2:k){
    fac<-fac+log(i)
  }
  f<-(k+1)*log(e)-fac-k*log(2)
  f<-exp(f)
  f<-f*s*g/(2*k+1)/(2*k+2)
  return(f)
}
k<-10
d<-10
a<-runif(d,-5,5)
kth(a,k,d)
a<-runif(d,-3,3)
kth(a,k,d)

k<-100
d<-10
a<-runif(d,-5,5)
kth(a,k,d)
a<-runif(d,-3,3)
kth(a,k,d)

k<-10
d<-2
a<-runif(d,-5,5)
kth(a,k,d)
a<-runif(d,-3,3)
kth(a,k,d)

## -----------------------------------------------------------------------------
kths<-function(a,kk,d){
  ff<-c(length=kk)
  e<-0
  for(j in 1:d){
      e<-e+a[j]^2
    }
  for(k in 1:kk){
    s<-1
    if(k%%2==1) s<--1
    g<-exp(lgamma((d+1)/2)+lgamma(k+3/2)-lgamma(k+d/2+1))
    f<-e*g
    for(i in 1:k){
      f<-f*e/(k+1-i)/2
    }
    ff[k]<-f*s/((2*k+1)*(2*k+2))
  }
  return(sum(ff)+e/2*exp(lgamma((d+1)/2)+lgamma(3/2)-lgamma(d/2+1)))
}

k<-1000
d<-5
a<-runif(d,-2,2)
kths(a,k,d)

## -----------------------------------------------------------------------------
d<-2
k<-1000
a<-c(1,2)
kths(a,k,d)

## -----------------------------------------------------------------------------
Sk<-function(a){
  ri1<-sqrt(a^2*(k-1)/(k-a^2))
  y1<-pt(ri1,k-1,0,lower.tail = FALSE)
  ri2<-sqrt(a^2*k/(k+1-a^2))
  y2<-pt(ri2,k,0,lower.tail = FALSE)
  y1-y2
}
k<-25
uniroot(Sk,c(0.01,sqrt(k)))
k<-100
uniroot(Sk,c(0.01,sqrt(k)))
k<-500
uniroot(Sk,c(0.01,3))
k<-1000
uniroot(Sk,c(0.01,3))
k<-1000
a<-seq(0,sqrt(k),length.out = 1000)
ri1<-sqrt(a^2*(k-1)/(k-a^2))
y1<-pt(ri1,k-1,0,lower.tail = FALSE)
ri2<-sqrt(a^2*k/(k+1-a^2))
y2<-pt(ri2,k,0,lower.tail = FALSE)
plot(a,y1-y2,type="l")

## -----------------------------------------------------------------------------
k<-25
S1<-function(a){
  f1<-function(x) (1+x^2/(k-1))^(-k/2)
  b1<-integrate(f1,0,sqrt(a^2*(k-1)/(k-a^2)))$value
  c1<-exp(lgamma(k/2)-lgamma(k/2-1/2))/sqrt(k-1)
  f2<-function(x) (1+x^2/k)^(-k/2-1/2)
  b2<-integrate(f2,0,sqrt(a^2*k/(k+1-a^2)))$value
  c2<-exp(lgamma(k/2+1/2)-lgamma(k/2))/sqrt(k)
  b1*c1-b2*c2
}
uniroot(S1,c(0.01,3))
k<-100
uniroot(S1,c(0.01,3))
k<-500
uniroot(S1,c(0.01,3))
k<-1000
uniroot(S1,c(0.01,3))
a<-seq(0.01,5,length.out = 1000)
d<-c()
for(i in 1:1000){
  f1<-function(x) (1+x^2/(k-1))^(-k/2)
  b1<-integrate(f1,0,sqrt(a[i]^2*(k-1)/(k-a[i]^2)))$value
  c1<-exp(lgamma(k/2)-lgamma(k/2-1/2))/sqrt(k-1)
  f2<-function(x) (1+x^2/k)^(-k/2-1/2)
  b2<-integrate(f2,0,sqrt(a[i]^2*k/(k+1-a[i]^2)))$value
  c2<-exp(lgamma(k/2+1/2)-lgamma(k/2))/sqrt(k)
  d[i]<-b1*c1-b2*c2
}
plot(a,d,type = "l")

## -----------------------------------------------------------------------------
y<-c(0.54,0.48,0.33,0.43,1,1,0.91,1,0.21,0.85)
tau<-1
EM<-function(tau=1,y,max.it=10000,eps=1e-5){
  lam2<-0.1
  lam1<-1
  n<-length(y)
  e=y[which(y[]==1)]
  m<-length(e)
  while(abs(lam1-lam2)>=eps){
    lam1<-lam2
    lam2<-(sum(y)-m+m*(tau+lam2))/n
    print(round(c(lam2),5))
    if(i==max.it) break
    i<-i+1
  }
  return(lam2)
}
EM(1,y)

## -----------------------------------------------------------------------------
y<-c(0.54,0.48,0.33,0.43,1,1,0.91,1,0.21,0.85)
e=y[which(y[]!=1)]
m<-length(e)
tau<-1
ml<-function(lam=1){
  return(m*log(lam)+
           sum(y)/lam)
}
sum(y)/m
library(stats4)
fiy<-mle(ml)
fiy@coef

## -----------------------------------------------------------------------------
set.seed(0)
trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(100)
sum(x)
median(x)
lapply(trims, function(trim) mean(x, trim = trim))
lapply(trims, mean, x = x)
lapply(trims, sum, x = x)
lapply(trims, quantile, x = x)

## -----------------------------------------------------------------------------
set.seed(20211126)
data(mtcars)

formulas <- list(
mpg ~ disp,
mpg ~ I(1 / disp),
mpg ~ disp + wt,
mpg ~ I(1 / disp) + wt
)
form<-lapply(formulas,function(formula) lm(formula,data = mtcars))

bootstraps <- lapply(1:10, function(i) {
rows <- sample(1:nrow(mtcars), rep = TRUE)
mtcars[rows, ]})

formulas2<-list(mpg~disp)
lmb<-list()
suppressWarnings(
for(i in 1:10){
  lmb[i]<-lapply(formulas2,function(formula) lm(formula,data = bootstraps[[i]]))
}
)
rsq <- function(mod) summary(mod)$r.squared
a<-c()
b<-c()
for(i in 1:4) a[i]<-rsq(form[[i]])
for(i in 1:10) b[i]<-rsq(lmb[[i]])
a
b
mean(b)

## -----------------------------------------------------------------------------
vapply(mtcars,sd,numeric(1))

## -----------------------------------------------------------------------------
mt<-mtcars
mt[,"mpg"]<-as.character(mt[,"mpg"])
mt[,"mpg"]<-c("a")
mt

vapply(mt[,which(vapply(mt,is.numeric,logical(1))==TRUE)],sd,numeric(1))

## ----warning=FALSE------------------------------------------------------------
mcsapply<-function(x,a){
library(parallel)
cores <- detectCores()
cluster <- makePSOCKcluster(cores)
c<-unlist(parLapply(cluster, x, a))
return(c)
}
mcvapply<-function(x,a,ty){
library(parallel)
cores <- detectCores()
cluster <- makePSOCKcluster(cores)
c<-unlist(parLapply(cluster, x, a))
for(i in 1:length(c)){
  if(class(c[i])==class(ty[i])) c[i]<-c[i]
    else {c[i]<-as.logical(c[i]);c[i]=FALSE}
}
return(c)
}
a<-function(x) x^2+exp(x)
x<-1:10
system.time(mcsapply(x,a))
system.time(sapply(x,a))

library(parallel)
cl=makeCluster(2)
system.time(parSapply(cl,x,FUN=function(x) x^2))
stopCluster(cl)

b<-numeric(length = 10)
mcvapply(x,a,b)

## -----------------------------------------------------------------------------
gibbsR <- function(N,a,b,n) {
  mat <- matrix(nrow = N, ncol = 2)
  x <- y <- 0
  for (i in 1:N) {
    x<-rbinom(1,n,y)
    y<-rbeta(1,x+a,n-x+b)
    mat[i, ] <- c(x, y)
  }
  mat
}

## -----------------------------------------------------------------------------
library(Rcpp)
dir_cpp <- 'H:/Rp/StatComp21058/src/'
sourceCpp(paste0(dir_cpp,"gibbsC.cpp"))
N<-5000
X1<-X2<-matrix(nrow = N, ncol = 2)
X1<-gibbsC(N,2,2,4,10)
X2<-gibbsR(N,2,4,10)
qqplot(X1[,1],X2[,1],type="l")
qqplot(X1[,2],X2[,2],type="l")

## -----------------------------------------------------------------------------
library(microbenchmark)
t<-microbenchmark(X1<-gibbsC(N,2,2,4,10),X2<-gibbsR(N,2,4,10))
t

