library(grplasso)
library(glmnet)
source("grplasso_supp.R")
netmat=function(A,width){
  ## this function conduct the logistics regression for each colomn of A on
  ## the other colomns that are not in the same group. 
  ## A is the binary matrix representing the original data
  ##width is a vector representing the freedom of given taxa,width=M-1
  n=length(width) ##the number of taxa
  A=as.matrix(A)
  gamma=0.9
lam=0.1
  nlambda=200
  netmat=matrix(nrow = n,ncol = n)
  CoeVector=rep(0,n-1)
  for (i in 1:1){
    ## regression for the first taxa
    print(i)
    x=A[,-(1:width[1])]
    x=as.matrix(cbind(1,x))
    width1=width[-1]
    group1=rep(1:(n-1),width1)
    group1=c(NA,group1)
    
    for (j in 1:width1[1]) {
      y=as.vector(A[,j])
      #y=ifelse(y==-1,0,1)
      lambda=lambdamax(x,y=y,index=group1,penscale = sqrt,model = LogReg())*gamma^(0:nlambda)
      print(lambda)
      gg=grplasso(x,y=y,index=group1,lambda=lambda,model=LogReg(),penscale=sqrt,control=grpl.control(update.hess = "lambda",trace = 0))
      beta=gg$coefficients
      no=1:length(group1)
      beta=cbind(no,beta)
      aa=vector("list",length(lambda))
      dev=rep(0,(length(lambda)))
      eBIC=rep(0,(length(lambda)))
      for (k in 1:length(lambda)) {
        aa[[k]]=beta[beta[,k+1]!=0,1]
        dev[k]=glm.fit(x=x[,aa[[k]]],y=y,family = binomial(),intercept = T)$deviance
        eBIC[k]=dev[k]+length(aa[[k]])*(log(dim(A)[1])+lam*log(length(group1)))
      }
      k=which.min(eBIC)
      beta=beta[-1,]
      group=group1[-1]
      d=glength(beta[,k+1],group)
      CoeVector=c(1,d)
    }
  }
  netmat[1,]=CoeVector
  for (i in n:n){
    print(i)
    CoeVector=rep(0,n-1)
    cum=cumsum(width)
    x=A[,-(((cum[i-1]+1):cum[i]))]
    x=as.matrix(cbind(1,x))
    widthi=width[-i]
    group1=rep(1:(n-1),widthi)
    group1=c(NA,group1)
    for (j in 1:width[i]) {
      y=as.vector(A[,cum[i-1]+j])
      #y=ifelse(y==-1,0,1)
      lambda=lambdamax(x=x,y=y,index=group1,penscale = sqrt,model = LogReg())*gamma^(0:nlambda)
      print(lambda)
      gg=grplasso(x,y=y,index=group1,lambda=lambda,model=LogReg(),penscale=sqrt,control=grpl.control(update.hess = "lambda",trace = 0))
      beta=as.matrix(gg$coefficients)
      no=1:length(group1)
      beta=cbind(no,beta)
      aa=vector("list",length(lambda))
      dev=rep(0,(length(lambda)))
      eBIC=rep(0,(length(lambda)))
      for (k in 1:length(lambda)) {
        aa[[k]]=beta[beta[,k+1]!=0,1]
        dev[k]=glm.fit(x=x[,aa[[k]]],y=y,family = binomial(),intercept = T)$deviance
        eBIC[k]=dev[k]+length(aa[[k]])*(log(dim(A)[1])+lam*log(length(group1)))
      }
      k=which.min(eBIC)
      beta=beta[-1,]
      group=group1[-1]
      d=glength(beta[,k+1],group)
      CoeVector=CoeVector+d
    }
    CoeVector=c(CoeVector[1:(i-1)],1)
  }
  netmat[n,]=CoeVector
  for (i in 2:(n-1)){
    print(i)
    CoeVector=rep(0,n-1)
    ##regression for the remaining taxa
    cum=cumsum(width)
    x=A[,-(((cum[i-1]+1):cum[i]))]
    x=as.matrix(cbind(1,x))
    widthi=width[-i]
    group1=rep(1:(n-1),widthi)
    group1=c(NA,group1)
    for (j in 1:width[i]) {
      y=as.vector(A[,cum[i-1]+j])
      #y=ifelse(y==-1,0,1)
      lambda=lambdamax(x=x,y=y,index=group1,penscale = sqrt,model = LogReg())*gamma^(0:nlambda)
      print(lambda)
      gg=grplasso(x,y=y,index=group1,lambda=lambda,model=LogReg(),penscale=sqrt,control=grpl.control(update.hess = "lambda",trace = 0))
      
      beta=as.matrix(gg$coefficients)
      no=1:length(group1)
      beta=cbind(no,beta)
      aa=vector("list",length(lambda))
      dev=rep(0,(length(lambda)))
      eBIC=rep(0,(length(lambda)))
      for (k in 1:length(lambda)) {
        aa[[k]]=beta[beta[,k+1]!=0,1]
        dev[k]=glm.fit(x=x[,aa[[k]]],y=y,family = binomial(),intercept = T)$deviance
        eBIC[k]=dev[k]+length(aa[[k]])*(log(dim(A)[1])+lam*log(length(group1)))
      }
      k=which.min(eBIC)
      beta=beta[-1,]
      group=group1[-1]
      d=glength(beta[,k+1],group)
      CoeVector=CoeVector+d
    }
    CoeVector=c(CoeVector[1:(i-1)],1,CoeVector[i:(n-1)])
    netmat[i,]=CoeVector
  }
  return(netmat)
}
# TwoLevel=read.table("twolevel.cvs",sep=" ")
# M=scan("M.cvs")
# eBICnet=NetVector(A=TwoLevel,width = M-1,i=409)
# write(eBICnet,file="eBICnet.csv")
