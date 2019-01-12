## ------------------------------------------------------------------------
inv_F<-function(x){
  if(x<=0.1)
    F<-0
  else if(x<=0.3)
    F<-1
  else if(x<=0.5)
    F<-2
  else if(x<=0.7)
    F<-3
  else if(x<=1)
    F<-4
  return(F)
} #Use the inverse transform method
x<-runif(1000) #generate a uniform distribution sample
sam<-sapply(x, inv_F)
p0<-length(which(sam==0))/1000
p1<-length(which(sam==1))/1000
p2<-length(which(sam==2))/1000
p3<-length(which(sam==3))/1000
p4<-length(which(sam==4))/1000 #get the empirical probabilities
## relative frequency table
table(sam)
## the empirical probabilities
p0
p1
p2
p3
p4
## the empirical probabilities are close to the corresponding theoretical probabilities
## Repeat using the R sample function
x<-runif(1000) #generate a uniform distribution sample
sam<-sapply(x, inv_F)
p0<-length(which(sam==0))/1000
p1<-length(which(sam==1))/1000
p2<-length(which(sam==2))/1000
p3<-length(which(sam==3))/1000
p4<-length(which(sam==4))/1000 #get the empirical probabilities
## relative frequency table
table(sam)
## the empirical probabilities
p0
p1
p2
p3
p4
## the empirical probabilities are close to the corresponding theoretical probabilities

## ------------------------------------------------------------------------
fun1<-function(n,a,b){
j<-k<-0;
y <- numeric(n) 
while (k < n)
  { u <- runif(1) 
  j <- j + 1 
  x <- runif(1) #random variate from g 
  if (x^(a-1)* (1-x)^(b-1) > u)
    { 
    #we accept x 
    k <- k + 1 
    y[k] <- x 
    }
 } 
j
}#a function to generate a random sample
fun1(1000,3,2)
#the Beta(3,2)
n<-1000
j<-k<-0
y <- numeric(n) 
while (k < n)
  { u <- runif(1) 
  j <- j + 1 
  x <- runif(1) #random variate from g 
  if (x*x* (1-x) > u)
    { 
    #we accept x 
    k <- k + 1 
    y[k] <- x 
    }
 } 
j
hist(y,prob = TRUE)#histogram of the sample
z <- seq(0, 1, 0.01) 
lines(z, 12*z*z*(1-z))#the theoretical Beta(3,2) density superimposed

## ------------------------------------------------------------------------
n <- 1000
r <- 4
beta <- 2
lambda <- rgamma(n, r, beta) #Gamma(r,??) distribution
x <- rexp(n, lambda)#Exp(??) distribution
x

## ------------------------------------------------------------------------
MCF<-function(a){
  m<-1e4
  t<-runif(m,0,a)# get the uniform sample
  theta.hat <- mean(30*a*t^2*(1-t)^2)# get the Monto Carlo estimate
  print(theta.hat)
}
pv<-c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
for(x in pv){
 print(x)
 MCF(x)
 print(pbeta(x,3,3))#use the pbeta function and compare with MCF
}

## ------------------------------------------------------------------------
m<-1e4
x<-runif(m,0,1)
a<-(sqrt(-2*log(1-x))+sqrt(-2*log(x)))/2
b<-sqrt(-2*log(1-x))
c(var(a),var(b),var(b)/var(a))#compare the results

## ------------------------------------------------------------------------
m<-1e4
x<-runif(m,0,1)
a<-1/(1-x)
b<-sqrt(1-2*log(1-x))
theta1<-sum(a^4*exp(-a^2/2))/sqrt(2*pi)/m#get the mean
theta2<-sum(b/sqrt(2*pi*exp(1)))/m
print(c(theta1,theta2))
var1<-var(a^4*exp(-a^2/2)/sqrt(2*pi))#get the variance
var2<-var(b/sqrt(2*pi*exp(1)))
print(c(var1,var2))

## ------------------------------------------------------------------------
m<-1e4
x<-runif(m,0,1)
b<-sqrt(1-2*log(1-x))
theta<-sum(b/sqrt(2*pi*exp(1)))/m#get the mean
print(theta)
var2<-var(b/sqrt(2*pi*exp(1)))#get the variance
print(var2)

## ------------------------------------------------------------------------
m<-1e3
n<-100 
G.hat<-numeric(m)
for(i in 1:m){ 
  x<-rlnorm(n)#X is standard lognormal
  mu<-mean(x)
  sum0<-0
  for(j in 1:n){
    for(k in 1:n){
      sum0<-sum0+abs(x[j]-x[k])
    }
  }
  G.hat[i]<-sum0/(2*n^2*mu)
}
print(c(G.mean=mean(G.hat),G.median=median(G.hat),G.deciles=quantile(G.hat,c(0.1,0.9))))
hist(G.hat,freq = FALSE)
lines(density(G.hat))

## ------------------------------------------------------------------------
m<-1e3
n<-10 
G.hat<-numeric(m)
for(i in 1:m){ 
  x<-runif(n)#uniform distribution
  mu<-mean(x)
  sum0<-0
  for(j in 1:n){
    for(k in 1:n){
      sum0<-sum0+abs(x[j]-x[k])
    }
  }
  G.hat[i]<-sum0/(2*n^2*mu)
}
print(c(G.mean=mean(G.hat),G.median=median(G.hat),G.deciles=quantile(G.hat,c(0.1,0.9))))
hist(G.hat,freq = FALSE)
lines(density(G.hat))

## ------------------------------------------------------------------------
m<-1e3
n<-10 
G.hat<-numeric(m)
for(i in 1:m){ 
  x<-rbinom(n,m,0.1)#Bernoulli(0.1)
  mu<-mean(x)
  sum0<-0
  for(j in 1:n){
    for(k in 1:n){
      sum0<-sum0+abs(x[j]-x[k])
    }
  }
  G.hat[i]<-sum0/(2*n^2*mu)
}
print(c(G.mean=mean(G.hat),G.median=median(G.hat),G.deciles=quantile(G.hat,c(0.1,0.9))))
hist(G.hat,freq = FALSE)
lines(density(G.hat))

## ------------------------------------------------------------------------
n<-50
m<-100
k<-200
G1<-numeric(m)
Grlnorm<-function(n){
  x<-rlnorm(n)
  mu<-mean(x)
  sum0<-0
  for(j in 1:n){
    for(k in 1:n){
      sum0<-sum0+abs(x[j]-x[k])
    }
  }
  return(sum0/(2*n^2*mu))
}
for(i in 1:m){
  G1[i]<-Grlnorm(n)
}
gamma.mean<-mean(G1)
gamma.sd<-sd(G1)
a<-gamma.mean-qt(0.95,df=m-1)*gamma.sd/sqrt(m)
b<-gamma.mean+qt(0.95,df=m-1)*gamma.sd/sqrt(m)#confidence interval(a,b)
gamma1<-numeric(k)
for(i in 1:k){
  G2<-numeric(m)
  for(j in 1:m){
    G2[j]<-Grlnorm(n)
  }
  gamma1[i]<-mean(G2)
}
cat('The confidence interval of gamma is (',round(a,4),',',round(b,4),'),and the coverage rate is ',round(sum(gamma1>a&gamma1<b)/k,3)*100,'%',sep = '')

## ------------------------------------------------------------------------
library(MASS)
n<-100
m<-1000
pearson.p<-numeric(m)
kendall.p<-numeric(m)
spearman.p<-numeric(m)

sigma0<-matrix(c(1,0.2,0.2,1),ncol = 2)#bivariate normal
mu<-c(0,1)
for(i in 1:m){
  mlnorm<-mvrnorm(n,mu,sigma0)
  x<-mlnorm[,1]
  y<-mlnorm[,2]
  pearson<-cor.test(x,y,method = 'pearson')
  kendall<-cor.test(x,y,method = 'kendall')
  spearman<-cor.test(x,y,method = 'spearman')
  pearson.p[i]<-pearson$p.value
  kendall.p[i]<-kendall$p.value
  spearman.p[i]<-spearman$p.value
}
cat('pearson:',sum(pearson.p<0.05)/m,
    'kendall:',sum(kendall.p<0.05)/m,
    'spearman:',sum(spearman.p<0.05)/m)#reject the null hypothesis

## ------------------------------------------------------------------------
library(MASS)
n<-100
m<-1000
pearson.p<-numeric(m)
kendall.p<-numeric(m)
spearman.p<-numeric(m)

sigma0<-matrix(c(1,0,0,1),ncol = 2)#X and Y are dependent
mu<-c(0,1)
for(i in 1:m){
  mlnorm<-mvrnorm(n,mu,sigma0)
  x<-mlnorm[,1]
  y<-mlnorm[,2]
  pearson<-cor.test(x,y,method = 'pearson')
  kendall<-cor.test(x,y,method = 'kendall')
  spearman<-cor.test(x,y,method = 'spearman')
  pearson.p[i]<-pearson$p.value
  kendall.p[i]<-kendall$p.value
  spearman.p[i]<-spearman$p.value
}
cat('pearson:',sum(pearson.p<0.05)/m,
    'kendall:',sum(kendall.p<0.05)/m,
    'spearman:',sum(spearman.p<0.05)/m)#accept the null hypothesis

## ------------------------------------------------------------------------
library(bootstrap)
n<-nrow(law)    #sample size
theta.jack<-numeric(n) #storage for replicates
b.cor<-function(x,i) cor(x[i,1],x[i,2])
theta.hat<-b.cor(law,1:n)
for(i in 1:n){
  theta.jack[i]<-b.cor(law,(1:n)[-i])
}
bias.jack<-(n-1)*(mean(theta.jack)-theta.hat) 
se.jack<-sqrt((n-1)*mean((theta.jack-theta.hat)^2)) 
round(c(original=theta.hat,bias=bias.jack, se=se.jack),3)

## ------------------------------------------------------------------------
library(bootstrap)
n<-nrow(law)    #sample size
theta.jack<-numeric(n) #storage for replicates
b.cor<-function(x,i) cor(x[i,1],x[i,2])
theta.hat<-b.cor(law,1:n)
for(i in 1:n){
  theta.jack[i]<-b.cor(law,(1:n)[-i])
}
bias.jack<-(n-1)*(mean(theta.jack)-theta.hat) 
se.jack<-sqrt((n-1)*mean((theta.jack-theta.hat)^2)) 
round(c(original=theta.hat,bias=bias.jack, se=se.jack),3)

## ------------------------------------------------------------------------
library(boot)
set.seed(1)
n<-nrow(aircondit) #sample size
mean(aircondit[,1])
boot.mean<-function(x,i) mean(x[i,1]) #give the mean function
boot.out<-boot(data=aircondit,statistic=boot.mean,R=1000)
boot.ci(boot.out,conf=0.95)

## ------------------------------------------------------------------------
library(bootstrap)
n<-nrow(scor)
theta.jack<-numeric(n)
n<-nrow(scor)
theta.jack<-numeric(n)
scor.pr<-function(x,i){
  a<-eigen(cor(x[i,]))
  a$values[1]/sum(a$values) # the proportion of variance 
}
theta.hat<-scor.pr(scor,1:n)
for(j in 1:n){
  theta.jack[j]<-scor.pr(scor,(1:n)[-j])
}
bias.jack<-(n-1)*(mean(theta.jack)-theta.hat) 
se.jack<-sqrt((n-1)*mean((theta.jack-theta.hat)^2)) 
round(c(original=theta.hat,bias=bias.jack, se=se.jack),3)

## ------------------------------------------------------------------------
library(DAAG)
attach(ironslag)
n <- length(magnetic) #in DAAG ironslag 
e1 <- e2 <- e3 <- e4 <- numeric((n-1)*n) #the size of samples
k=1 
for(i in 1:(n-1)){
  for(j in (i+1):n){   
  y<- magnetic[-i][-(j-1)]
  x<- chemical[-i][-(j-1)] #leave-two-out samples

  
  J1 <- lm(y ~ x) 
  yhat1 <- J1$coef[1] + J1$coef[2] * chemical[i] 
  e1[k] <- magnetic[i] - yhat1  #check out the modle
  
  J2 <- lm(y ~ x + I(x^2)) 
  yhat2 <- J2$coef[1] + J2$coef[2] * chemical[i] + J2$coef[3] * chemical[i]^2 
  e2[k] <- magnetic[i] - yhat2
  
  J3 <- lm(log(y) ~ x) 
  logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[i] 
  yhat3 <- exp(logyhat3) 
  e3[k] <- magnetic[i] - yhat3
  
  J4 <- lm(log(y) ~ log(x)) 
  logyhat4 <- J4$coef[1] + J4$coef[2] * log(chemical[i]) 
  yhat4 <- exp(logyhat4) 
  e4[k] <- magnetic[i] - yhat4
  
  k<-k+1

  yhat1 <- J1$coef[1] + J1$coef[2] * chemical[j] 
  e1[k] <- magnetic[j] - yhat1 
  
  yhat2 <- J2$coef[1] + J2$coef[2] * chemical[j] + J2$coef[3] * chemical[j]^2 
  e2[k] <- magnetic[j] - yhat2
  
  logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[j] 
  yhat3 <- exp(logyhat3) 
  e3[k] <- magnetic[j] - yhat3
  
  logyhat4 <- J4$coef[1] + J4$coef[2] * log(chemical[j]) 
  yhat4 <- exp(logyhat4) 
  e4[k] <- magnetic[j] - yhat4
  
  k<-k+1
  
  }
}
c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2)) #get the square error
lm(formula = magnetic ~ chemical + I(chemical^2))

## ------------------------------------------------------------------------
set.seed(1)
attach(chickwts) 
x <- sort(as.vector(weight[feed == "soybean"])) 
y <- sort(as.vector(weight[feed == "linseed"])) 
detach(chickwts)
n<-length(x)
m<-length(y)
R<-999 #repeat
z<-c(x,y)
h<-1:26
reps<-numeric(R)
cvm<-function(a,b){
  F<-ecdf(a)
  G<-ecdf(b)
  m*n*(sum((F(a)-G(a))^2)+sum((F(b)-G(b))^2))/((m+n)^2)
}
w0<-cvm(x,y)
for (i in 1:R) { 
  k <- sample(h, size = n, replace = FALSE) 
  x1 <- z[k]
  y1 <- z[-k]
  reps[i] <-cvm(x1,y1)
}#permutation test
p <- mean(abs(c(w0, reps)) >= abs(w0)) 
round(p,3)

## ------------------------------------------------------------------------
library(Ball)
library(energy)
library(boot)
library(RANN)
m <- 1e3
k<-3
p<-2
mu <- 0.5
set.seed(12345) 
n1 <- n2 <- 20 
R<-999
n <- n1+n2
N = c(n1,n2) 
eqdist.nn <- function(z,sizes,k){ 
  boot.obj <- boot(data=z,statistic=Tn,R=R, sim = "permutation", sizes = sizes,k=k) 
  ts <- c(boot.obj$t0,boot.obj$t) 
  p.value <- mean(ts>=ts[1]) 
  list(statistic=ts[1],p.value=p.value) 
} 
p.values <- matrix(NA,m,3)
Tn <- function(z, ix, sizes,k){ 
  n1 <- sizes[1] 
  n2 <- sizes[2] 
  n <- n1 + n2 
  if(is.vector(z)) 
    z <- data.frame(z,0); 
  z <- z[ix, ]
  NN <- nn2(data=z, k=k+1)
  block1 <- NN$nn.idx[1:n1,-1] 
  block2 <- NN$nn.idx[(n1+1):n,-1] 
  i1 <- sum(block1 < n1 + .5) 
  i2 <- sum(block2 > n1+.5) 
  (i1 + i2) / (k * n) 
} 
for(i in 1:m){ 
  x <- matrix(rnorm(n1*p),ncol=p); 
  y <- cbind(rnorm(n2),rnorm(n2,mean=mu)); 
  z <- rbind(x,y)
  p.values[i,1] <- eqdist.nn(z,N,k)$p.value 
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value 
  p.values[i,3] <- bd.test(x=x,y=y,R=999,seed=i*12345)$p.value 
}#get p.values by the NN, energy, and ball methods.
alpha <- 0.1
pow <- colMeans(p.values<alpha)
round(pow,3)

## ------------------------------------------------------------------------
library(Ball)
library(energy)
library(boot)
library(RANN)
m <- 1e3
k<-3
p<-2
sd1<-2
set.seed(12345) 
n1 <- n2 <- 20 
R<-999
n <- n1+n2
N = c(n1,n2) 
eqdist.nn <- function(z,sizes,k){ 
  boot.obj <- boot(data=z,statistic=Tn,R=R, sim = "permutation", sizes = sizes,k=k) 
  ts <- c(boot.obj$t0,boot.obj$t) 
  p.value <- mean(ts>=ts[1]) 
  list(statistic=ts[1],p.value=p.value) 
} 
p.values <- matrix(NA,m,3)
Tn <- function(z, ix, sizes,k){ 
  n1 <- sizes[1] 
  n2 <- sizes[2] 
  n <- n1 + n2 
  if(is.vector(z)) 
    z <- data.frame(z,0)
  z <- z[ix, ]
  NN <- nn2(data=z, k=k+1) 
  block1 <- NN$nn.idx[1:n1,-1] 
  block2 <- NN$nn.idx[(n1+1):n,-1] 
  i1 <- sum(block1 < n1 + .5) 
  i2 <- sum(block2 > n1+.5) 
  (i1 + i2) / (k * n) 
} 
for(i in 1:m){ 
  x <- matrix(rnorm(n1*p),ncol=p)
  y <- cbind(rnorm(n2),rnorm(n2,sd=sd1))
  z <- rbind(x,y)
  p.values[i,1] <- eqdist.nn(z,N,k)$p.value 
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value 
  p.values[i,3] <- bd.test(x=x,y=y,R=999,seed=i*12345)$p.value 
}
alpha <- 0.1
pow <- colMeans(p.values<alpha)
round(pow,3)

## ------------------------------------------------------------------------
library(Ball)
library(energy)
library(boot)
library(RANN)
m <- 1e3
df<-1
df2<-2
k<-3
p<-2
set.seed(12345) 
n1 <- n2 <- 20
R<-999
n <- n1+n2
N = c(n1,n2) 
eqdist.nn <- function(z,sizes,k){ 
  boot.obj <- boot(data=z,statistic=Tn,R=R, sim = "permutation", sizes = sizes,k=k) 
  ts <- c(boot.obj$t0,boot.obj$t) 
  p.value <- mean(ts>=ts[1]) 
  list(statistic=ts[1],p.value=p.value) 
} 
p.values <- matrix(NA,m,3)
Tn <- function(z, ix, sizes,k){ 
  n1 <- sizes[1] 
  n2 <- sizes[2] 
  n <- n1 + n2 
  if(is.vector(z)) 
    z <- data.frame(z,0)
  z <- z[ix, ]
  NN <- nn2(data=z, k=k+1)
  block1 <- NN$nn.idx[1:n1,-1] 
  block2 <- NN$nn.idx[(n1+1):n,-1] 
  i1 <- sum(block1 < n1 + .5) 
  i2 <- sum(block2 > n1+.5) 
  (i1 + i2) / (k * n) 
} 
for(i in 1:m){ 
  x <- matrix(rt(n1*p,df=df),ncol=p)
  y <- cbind(rt(n2,df=df),rt(n2,df=df2))
  z <- rbind(x,y)
  p.values[i,1] <- eqdist.nn(z,N,k)$p.value 
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value 
  p.values[i,3] <- bd.test(x=x,y=y,R=999,seed=i*12345)$p.value 
}
alpha <- 0.1
pow <- colMeans(p.values<alpha)
round(pow,3)

## ------------------------------------------------------------------------
library(Ball)
library(energy)
library(boot)
library(RANN)
m <- 1e3
t<-0.8
k<-3
p<-2
set.seed(12345) 
n1 <- n2 <- 20
R<-999
n <- n1+n2
N = c(n1,n2) 
eqdist.nn <- function(z,sizes,k){ 
  boot.obj <- boot(data=z,statistic=Tn,R=R, sim = "permutation", sizes = sizes,k=k) 
  ts <- c(boot.obj$t0,boot.obj$t) 
  p.value <- mean(ts>=ts[1]) 
  list(statistic=ts[1],p.value=p.value) 
} 
p.values <- matrix(NA,m,3)
Tn <- function(z, ix, sizes,k){ 
  n1 <- sizes[1] 
  n2 <- sizes[2] 
  n <- n1 + n2 
  if(is.vector(z)) 
    z <- data.frame(z,0)
  z <- z[ix, ]
  NN <- nn2(data=z, k=k+1)
  block1 <- NN$nn.idx[1:n1,-1] 
  block2 <- NN$nn.idx[(n1+1):n,-1] 
  i1 <- sum(block1 < n1 + .5) 
  i2 <- sum(block2 > n1+.5) 
  (i1 + i2) / (k * n) 
} 
for(i in 1:m){ 
  x <- matrix(rbinom(n1*p,1,0.5),ncol=p)
  y <- cbind(rbinom(n2,1,0.5),rbinom(n2,1,t))
  z <- rbind(x,y)
  p.values[i,1] <- eqdist.nn(z,N,k)$p.value 
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value 
  p.values[i,3] <- bd.test(x=x,y=y,R=999,seed=i*12345)$p.value 
}
alpha <- 0.1
pow <- colMeans(p.values<alpha)
round(pow,3)

## ------------------------------------------------------------------------
library(Ball)
library(energy)
library(boot)
library(RANN)
m <- 1e3
k<-3
p<-2
set.seed(12345) 
n1<-20
n2<-30
R<-999
n <- n1+n2
N = c(n1,n2) 
eqdist.nn <- function(z,sizes,k){ 
  boot.obj <- boot(data=z,statistic=Tn,R=R, sim = "permutation", sizes = sizes,k=k) 
  ts <- c(boot.obj$t0,boot.obj$t) 
  p.value <- mean(ts>=ts[1]) 
  list(statistic=ts[1],p.value=p.value) 
} 
p.values <- matrix(NA,m,3)
Tn <- function(z, ix, sizes,k){ 
  n1 <- sizes[1] 
  n2 <- sizes[2] 
  n <- n1 + n2 
  if(is.vector(z)) 
    z <- data.frame(z,0); 
  z <- z[ix, ]
  NN <- nn2(data=z, k=k+1)
  block1 <- NN$nn.idx[1:n1,-1] 
  block2 <- NN$nn.idx[(n1+1):n,-1] 
  i1 <- sum(block1 < n1 + .5) 
  i2 <- sum(block2 > n1+.5) 
  (i1 + i2) / (k * n) 
} 
for(i in 1:m){ 
  x <- matrix(rnorm(n1*p),ncol=p)
  y <- cbind(rnorm(n2),rnorm(n2))
  z <- rbind(x,y)
  p.values[i,1] <- eqdist.nn(z,N,k)$p.value 
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value 
  p.values[i,3] <- bd.test(x=x,y=y,R=999,seed=i*12345)$p.value 
}
alpha <- 0.1
pow <- colMeans(p.values<alpha)
round(pow,3)

## ------------------------------------------------------------------------
set.seed(1)

m<-20000 #length of the chain
x<-numeric(m)
y<-numeric(m-1000)
x[1]<-runif(1)

standard_cauchy<-function(x){
  return(1/(pi*(1+x^2)))
}

for(i in 1:(m-1)){
  proposal<-x[i]+runif(1,min=-1,max=1)
  accept<-runif(1)<=standard_cauchy(proposal)/standard_cauchy(x[i])
  x[i+1]<-ifelse(accept==T,proposal,x[i])
}

y<-x[1001:m]

quantile(y,probs = seq(0.1,0.9,0.1))

qcauchy(seq(0.1,0.9,0.1))

qqnorm(y)

## ------------------------------------------------------------------------
set.seed(1)
m<-10000 #length of the chain
w<-0.25 #width
u<-runif(m)
v<-runif(m,-w,w)
group<-c(125,18,20,34)
x<-numeric(m)

prob<-function(theta){
  if(theta<0||theta>=0.8) 
    return(0)
  else
    return((1/2+theta/4)^group[1]*((1-theta)/4)^group[2]*((1-theta)/4)^group[3]*(theta/4)^group[4])
}

x[1]<-0.3 #initial value
for(i in 2:m){
  theta<-x[i-1]+v[i]
  if(u[i]<=prob(theta)/prob(x[i-1]))
    x[i]<-theta
  else
    x[i]<-x[i-1]
}

theta.hat<-mean(x[1001:m])
print(theta.hat)

## ------------------------------------------------------------------------
b=1000
m=10000
N=b+m
Gelman.Rubin <- function(psi){ 
  # psi[i,j] is the statistic psi(X[i,1:j]) 
  # for chain in i-th row of X 
  psi <- as.matrix(psi) 
  n <- ncol(psi) 
  k <- nrow(psi)
  psi.means <- rowMeans(psi) #row means 
  B <- n * var(psi.means) #between variance est. 
  psi.w <- apply(psi, 1, "var") #within variances 
  W <- mean(psi.w) #within est. 
  v.hat <- W*(n-1)/n + (B/(n*k)) #upper variance est. 
  r.hat <- v.hat / W #G-R statistic return(r.hat) 
}
iden<-function(theta){
  125*log(2+theta)+38*log(1-theta)+34*log(theta)
}
MCMC<-function(x,print_acc=F){
  y=1:N
  acc=0
  for(i in 1:N){
    p=runif(1)
    y[i]=x=if(runif(1)<exp(iden(p)-iden(x))){acc=acc+1;p}else x
  }
  if(print_acc) print(acc/N)
  y[(b+1):N]
}
plot(MCMC(0.5,print_acc = T))
M1=cumsum((MC1=MCMC(0.2)))/1:m
M2=cumsum((MC2=MCMC(0.4)))/1:m
M3=cumsum((MC3=MCMC(0.6)))/1:m
M4=cumsum((MC4=MCMC(0.8)))/1:m
psi=rbind(M1,M2,M3,M4)
plot((R=sapply(1:100,function(i)Gelman.Rubin(psi[,1:i]))),main="R value of Gelman.Rubin method",ylim=c(1,2))

c(mean(c(MC1[1:10000],MC2[1:10000],MC3[1:10000],MC4[1:10000])),var(c(MC1[1:10000],MC2[1:10000],MC3[1:10000],MC4[1:10000])))

## ------------------------------------------------------------------------
alpha<-rep(1,25)
b<-c(4:25,100,500,1000)
for(i in 1:25){
  k=b[i]
inter<-function(a){
  pt(sqrt(a*a*(k-1)/(k-a*a)),df=k-1,lower.tail=F,log.p=T)-pt(sqrt(a*a*k/(k+1-a*a)),df=k,lower.tail=F,log.p=T)
}
solution <- uniroot(inter,lower=0.0001,upper=sqrt(k)-0.0001)
alpha[i]<- solution$root
print(alpha[i])
}#intersection points

## ------------------------------------------------------------------------
cdfCauchy = function(q, eta=0, theta=1) {
pdf = function(x,eta,theta){ 1/(theta*pi*(1 + ((x-eta)/theta)^2)) }
 result = integrate( f=pdf,
 lower=-Inf, upper=q,
 rel.tol=.Machine$double.eps^.25,
 eta=eta, theta=theta )
# return( list( value=result$value, abs.error=result$abs.error ) )
 return( result$value )
 } #end function 

## ------------------------------------------------------------------------
x = matrix( seq(-4,4), ncol=1 )
cbind( x, apply(x, MARGIN=1, FUN=cdfCauchy), pcauchy(x) )

## ------------------------------------------------------------------------
cbind( x, apply( x, MARGIN=1, FUN=cdfCauchy, eta=2 ),
 pcauchy(x,location=2) ) 

## ------------------------------------------------------------------------
cbind( x, apply( x, MARGIN=1, FUN=cdfCauchy, theta=2 ),
 pcauchy(x,scale=2) ) 

## ------------------------------------------------------------------------
library(stats4)
nA<-28;nB<-24;nOO<-41;nAB<-70;
n<-nA+nB+nOO+nAB;
i=0;
p1<-0.3;q1<-0.3;
p0<-0;q0<-0;
options(warn=-1)
while(!isTRUE(all.equal(p0,p1,tolerance=10^(-7)))||!isTRUE(all.equal(q0,q1,tolerance=10^(-7))))#EM?<U+3DE8>
{
  p0<-p1;
  q0<-q1;
mlogL<-function(p,q){
  # minus log-likelihood
return(-(2*n*p0^2*log(p)+2*n*q0^2*log(q)+2*nOO*log(1-p-q)+(nA-n*p0^2)*log(2*p*(1-p-q))+(nB-n*q0^2)*(log(2*q*(1-p-q)))+nAB*log(2*p*q)))
}
fit<-mle(mlogL,start=list(p=0.2,q=0.3))
p1<-fit@coef[1]
q1<-fit@coef[2]
i=i+1
}
print(c(i,p1,q1))

## ------------------------------------------------------------------------
set.seed(1)
attach(mtcars)
formulas <- list( 
mpg ~ disp, 
mpg ~ I(1 / disp), 
mpg ~ disp + wt, 
mpg ~ I(1 / disp) + wt 
)
n<-length(formulas)
# for loop
for(i in 1:n){
  print(lm(formulas[[i]]))
}

# lapply
lapply(formulas,lm)
#we can see the values are equal

detach(mtcars)

## ------------------------------------------------------------------------
set.seed(1)
bootstraps <- lapply(1:10, function(i) {
rows <- sample(1:nrow(mtcars), rep = TRUE) 
mtcars[rows, ] 
})
# for loop
for(i in 1:10){
  print(lm(bootstraps[[i]]$mpg~bootstraps[[i]]$disp))
}
# lapply
for(i in 1:10){
  bootstraps[[i]]<-subset(bootstraps[[i]],select=c(mpg,disp))
}
lapply(bootstraps,lm)
#we can see the values are equal

## ------------------------------------------------------------------------
# Exercise 3
set.seed(1)
attach(mtcars)
formulas <- list( 
mpg ~ disp, 
mpg ~ I(1 / disp), 
mpg ~ disp + wt, 
mpg ~ I(1 / disp) + wt 
)
n<-length(formulas)
# for loop
for(i in 1:n){
  print(summary(lm(formulas[[i]]))$r.squared)
}

# lapply
lapply(formulas,function(x){
  summary(lm(x))$r.squared
})
#we can see the values are equal
detach(mtcars)

# Exercise 4
set.seed(1)
bootstraps <- lapply(1:10, function(i) {
rows <- sample(1:nrow(mtcars), rep = TRUE) 
mtcars[rows, ] 
})
# for loop
for(i in 1:10){
  print(summary(lm(bootstraps[[i]]$mpg~bootstraps[[i]]$disp))$r.squared)
}
# lapply
for(i in 1:10){
  bootstraps[[i]]<-subset(bootstraps[[i]],select=c(mpg,disp))
}
lapply(bootstraps,function(x){
  summary(lm(x))$r.squared
})
#we can see the values are equal


## ------------------------------------------------------------------------
set.seed(1)
trials <- replicate( 100, t.test(rpois(10, 10), rpois(7, 10)), simplify = FALSE )
sapply(trials,function(x){
  return(x$p.value)
})

## ------------------------------------------------------------------------
library(foreach)
x<-foreach(a=1:3,b=rep(10,3),.combine = "rbind")%do%{
  x1<-(a+b)
  x2<-(a*b)
  c(x1,x2)
}
x

## ------------------------------------------------------------------------
library(microbenchmark)
a=c(12,24)
b=c(25,10)
e=c(11,30)
f=c(40,34)
# chisq.test
tableR<-cbind(a,b)
chisq1=chisq.test(tableR)$statistic
# a faster version of chisq.test
chisq.test2<-function(x1,x2){
  x<-cbind(x1,x2)
  sr <- rowSums(x)
  sc <- colSums(x)
  n<-sum(sr)
  E <- outer(sr, sc, "*")/n
  YATES <- min(0.5, abs(x - E))
  STATISTIC <- sum((abs(x - E) - YATES)^2/E)
  return(STATISTIC)
}
chisq2=chisq.test2(a,b)
round(c(chisq1,chisq2),5)

chisq1=chisq.test(cbind(e,f))$statistic
chisq2=chisq.test2(e,f)
round(c(chisq1,chisq2),5)
# chisq2 is faster
ts <- microbenchmark(chisq1=chisq.test(tableR)$statistic,chisq2=chisq.test2(a,b)) 
summary(ts)[,c(1,3,5,6)]

## ------------------------------------------------------------------------
library(microbenchmark)
a=c(0,1,2,1,1,0,0,1)
b=c(0,0,1,1,1,0,1,1)
# a faster version of table()
table2 <- function(x, y) {
  x_val <- unique(x)
  y_val <- unique(y)
  mat <- matrix(0L, length(x_val), length(y_val))
  for (i in seq_along(x)) {
    mat[which(x_val == x[[i]]), which(y_val == y[[i]])]<-mat[which(x_val == x[[i]]),which(y_val == y[[i]])] + 1L
  }
  dimnames <- list(x_val, y_val)
  names(dimnames) <- as.character(as.list(match.call())[-1])  # R has names for dimnames... :/
  tab <- array(mat, dim = dim(mat), dimnames = dimnames)
  class(tab) <- "table"
  tab
}
table(a,b)
table2(a,b)
identical(table(a,b),table2(a,b))

d=c(1,2,3,2,2,5,3,1)
f=c(1,2,3,5,2,2,2,3)
table(d,f)
table2(d,f)
identical(table(d,f),table2(d,f))
# table2 is faster
ts <- microbenchmark(table=table(d,f),table2=table2(d,f)) 
summary(ts)[,c(1,3,5,6)]

