glength=function(x,group){
  ##this function compute the piecewise norm according to the value of gruop
  ##group can only assume the the value 1,2,.....
  A=matrix(nrow = length(x),ncol = 2)
  A[,1]=x
  A[,2]=group
  m=max(group)
  d=rep(0,m)
  for (i in 1:m) {
    a=A[,1][A[,2]==i]
    d[i]=sqrt(crossprod(a))
  }
  return(d)
}