
## Find the cuting point for the classification 
##X denotes the sample vector and N denotes the maxmum number 
##of the classification
source("transformation_supp.R")
CuttingPoint=function(X,N){
  size=length(X)
  cutp=seq(N-1)
  m=Lvalue(X)
  a=m[[1]]
  b=m[[2]]
  cutp[N-1]=b[size,N]
  if (N>2){
    for (i in (N-2):1) {
      k=cutp[i+1]
      cutp[i]=b[k,i+1 ]
    } 
  }
  
  sortX=sort(X)
  CuttingValue=c(sortX[cutp])+10^(-10)
  ClassifyFunc=stepfun(CuttingValue,seq(N))
  label1=as.factor(ClassifyFunc(X))
  ##silwidth=cluster.stats(dist(X),label1)$avg.silwidth
  return(list(LossFunction=a[size,N],CuttingPoint=cutp,class=label1,CuttingValue=CuttingValue))
}
## classify each sample





