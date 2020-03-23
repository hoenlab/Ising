library(Rcpp)
cppFunction('NumericMatrix Dmatrix(NumericVector x){
            int i,j,n=x.size(),k1,k2;
            double sum=0, mean=0, dd=0;
            NumericMatrix M(n,n);
            for (j=0; j<=(n-1); j++){
            for (i=j; i<=(n-1); i++){
            mean=0;
            dd=0;
            sum=0;
            for (k1=j; k1<=i; k1++){
            sum+=x(k1);
            }
            mean=sum/(i-j+1);
            for (k2=j; k2<=i; k2++){
            dd+=(x(k2)-mean)*(x(k2)-mean);
            }
            M(i,j)=dd;
            }
            }
            return M;
            }')


##compute the loss function for  no classification
##The result from this function should be the first column of the matrix
## resulted from the function Dmatrix
Lvalue1=function(x){
  a=sort(x)
  size=length(a)
  v=seq(size)
  for (i in 1:size) {
    v[i]=t(a[1:i]-mean(a[1:i]))%*%(a[1:i]-mean(a[1:i]))
  }
  return(list(v=v))
}


##compute the minimum loss function for  binary classification
Lvalue2=function(x){
  x=sort(x)
  #values:
  #Minloss[j] is the loss if the samples 1:j are divided into 2 classes
  #Dmatrix[i,j] is the diameter for the subsample between x_i and x_j
  #Minpoint[j] is the optimal cutting point if the first j samples are classied into 2 classes 
  size=length(x)
  v=seq(size-1)   ##store the loss value 
  point=seq(size-1)
  Dmat=Dmatrix(x)
  for (j in 2:size) {
    value=seq(j-1)
    for (i in 1:(j-1)) {   ##i denote the number of the sample counted from the 1th one
      value[i]=Dmat[i,1]+Dmat[j,i+1]
    }
    v[j-1]=min(value)
    point[j-1]=which.min(value)#
    
  }
  return(list(MinLoss=v,Dmatrix=Dmat,MinPoint=point))
  
}

##compute the minimum loss function and positions for n>2 classfication. 
Lvalue=function(x){
  x=sort(x)
  #MinLoss[j,k] denote the minimum loss if x_1 to x_k 
  #are divided into k classes
  #MinPoint[j,k] denote the last cutting point if 
  #x_1 to x_k are divided into k classes
  #Dmat denote the diameter
  size=length(x)
  ##minimum loss matrix
  MinLoss=matrix(nrow = size,ncol = size)
  ##the corresponding last position
  MinPoint=matrix(nrow=size,ncol=size)
  b=Lvalue2(x)
  MinLoss[,1]=Lvalue1(x)[[1]]
  MinLoss[2:size,2]=b[[1]]
  MinPoint[,1]=seq(size)
  MinPoint[2:size,2]=b[[3]]
  Dmat=b[[2]]
  for (k in 3:size) {  ##k denotes the number of classifcation
    for (j in k:size) {  ##j denots the number of samples involved 
      v=seq(j-k+1) 
      for (i in k:j) {   ## i denotes the last cutting point 
        v[i-k+1]=MinLoss[i-1,k-1]+Dmat[j,i]
      }
      MinLoss[j,k]=min(v)
      MinPoint[j,k]=which.min(v)+k-2
    }
  }
  
  return(list(MinLoss,MinPoint,Dmat))
}