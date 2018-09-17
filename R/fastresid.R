fastresid <-
function(X,y){
  p=dim(X)[1]
  n=dim(X)[2]
  xbar=rowMeans(X)
  ybar=mean(y)
  varx=(n/(n-1))*(rowMeans(X^2)-rowMeans(X)^2)
  sdx=sqrt(varx)
  sdy=sd(y)
  r=cor(t(X),y)
  beta1=as.vector(r*sdx/sdy)
  beta0=xbar-beta1*ybar
  Xhat=beta0+outer(beta1,y)
  Xresid=X-Xhat
  return(Xresid)
 }
