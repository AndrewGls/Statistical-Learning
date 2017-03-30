require(ISLR)
require(boot)
load("~/Documents/Courses/Statistical Learning/Ch5/5.R.RData")

#attach(Xy)

## linear regression
lm.fit=glm(y~X1+X2, data=Xy)
summary(lm.fit)

matplot(Xy,type="l", col=c("red","green","blue"))

## 5.R.R2
## Our estimate of s.e.(beta1) is too low

## Bootstrap
## 5.R.R3 Now, use the (standard) bootstrap to estimate s.e.(beta1). To within 10%, what do you get?

boot.fn=function(data,index) {
  return (coef(glm(y~X1+X2, data=data, subset=index)))
}

sample.size=dim(Xy)[1]

boot.fn(Xy, 1:sample.size)

set.seed(1)
#boot.out=boot(Xy,function(Xy,idx){ coef(glm(y~X1+X2,data=Xy[idx,]))[[2]] },sample.size)
boot.fn (Xy,sample(1:sample.size,sample.size,replace=TRUE))
boot.out=boot(Xy,boot.fn,R=1000)
boot.out
plot(boot.out)

## 5.R.R4
new.rows = c(101:200, 401:500, 101:200, 901:1000, 301:400, 1:100, 1:100, 801:900, 201:300, 701:800)

# generate set of indexes as replicated 10 blocks by 100 elements:
set.seed(1)
new.rows = rep(sample(0:9, 10, replace=TRUE) * 100,each=100) + 1:100

new.Xy = Xy[new.rows, ]

bboot.fn=function(data, index) {
  index <- rep(sample(0:9,10,replace=T) * 100,each= 100) + 1:100
  return (coef(glm(y~X1+X2, data=data, subset=index)))
}

bboot.out=boot(new.Xy, bboot.fn, R=1000)
bboot.out
plot(bboot.out)


##=================
new.rows = c(101:200, 401:500, 101:200, 901:1000, 301:400, 1:100, 1:100, 801:900, 201:300, 701:800)
set.seed(1)
new.rows = rep(sample(0:9,10,replace=T) * 100,each= 100) + 1:100
new.Xy = Xy[new.rows, ]

bboot.out=boot(Xy
     ,function(tptp,idx){
       idx <- rep(sample(0:9,10,replace=T) * 100,each= 100) + 1:100
       coef(glm(y~X1+X2,data=tptp[idx,]))#[[2]]
     }
     ,R=1000)

bboot.out
plot(bboot.out)

