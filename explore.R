ClassNumber=function(data,MaxGroup=6, size=0.01){
  ##MaxGroup denote the upper bound of the number of class
  ##size also controll the  maximum number of class
  ##threshold control minimum decrease in sum of square within groups 
  n1=dim(data)[1]
  n2=dim(data)[2]
  MultiLevel=data.frame(matrix(nrow=n1,ncol=n2))
  #container for the number of class for all taxa and the default is 2
  N=rep(2,n2)
  for (j in 1:n2) {
print(j)
    l=length(data[,j][data[,j]!=0])
    M=ifelse (l/n1<size,2,MaxGroup)
    if (M<=2){
MultiLevel[,j]=ifelse(data[,j]<=median(data[,j]),0,1)
    ##  MultiLevel[,j]=CuttingPoint(data[,j],2)[[3]]
    }
    else{
      b=rep(0,M-1)
      d=matrix(nrow = n1,ncol = M-1)
      for (k in 2:M) {
        cp=CuttingPoint(data[,j],k)
        b[k-1]=cp[[1]]
        d[,k-1]=cp[[3]]
        }
      c=-(diff(b)-mean(diff(b))) 
N[j]=min(which(c<0))+1
     N[j]=ifelse(N[j]==1,2,N[j]) 
     MultiLevel[,j]=d[,N[j]]      
  }
  }
  return(list(N,MultiLevel))
} 




