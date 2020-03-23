
Binay=function(MultiLevel,N){
  ##Multilevel denotes the matrix each columm of which is the transformed 
  ##observation for each taxa (be factor). Vector N denotes the classification number
  ##for each taxa 
  ##values:Matrix BinaryLevel denotes the dummy variables matrix, vector
  ##VariateGroup denotes the clasification dummy variable 
  n1=dim(MultiLevel)[1]  ##n1 is the number of taxa
  n2=length(N)  ##n2 is the number of observations
  n3=sum(N)-n2     ##n3 is the net number of dummy variables
  cum=cumsum(N)-seq(n2) ##cum is used to index the dummy variables
  BinaryLevel=data.frame(matrix(nrow=n1,ncol =n3))
  Multi1=as.factor(MultiLevel[,1])
  BinaryLevel[,1:cum[1]]=model.matrix(~Multi1)[,-1]
  for (j in 1:(n2-1)) {
    Multij=as.factor(MultiLevel[,j+1])
    BinaryLevel[,(cum[j]+1):cum[j+1]]=model.matrix(~Multij)[,-1]
  }
  group=rep(1:n2,N-1)
 #BinaryLevel=ifelse(BinaryLevel==0,-1,1)
  return(list(BinaryLevel=BinaryLevel,VariateGroup=group))
}
   