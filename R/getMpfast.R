getMpfast <-
function(y,x,num.perms=1e3){
 n=length(y)
 mycor=cor(t(x),t(x)*y)
 sdx1=apply(x,1,sd)
 x1x2bar=x%*%t(x)/n
 sdx1x2=sqrt(x^2%*%t(x^2)/n-x1x2bar^2)*sqrt(n/(n-1))
 sdx2y=apply(t(x)*y,2,sd)
 sumx1x2y=mycor*outer(sdx1,sdx2y)*(n-1)
 mycor2=sumx1x2y/((n-1)*sdx1x2*sd(y))
 Mobs=max(abs(mycor2))
 M=rep(0,num.perms)
 for (i in (1:num.perms)){
  yperm=sample(y)
  mycor=cor(t(x),t(x)*yperm)
  sdx2y=apply(t(x)*yperm,2,sd)
  sumx1x2y=mycor*outer(sdx1,sdx2y)*(n-1)
  mycor2=sumx1x2y/((n-1)*sdx1x2*sd(yperm))
  M[i]=max(abs(mycor2))
  }
 pval=mean(M>=Mobs)
 return(list(Mobs=Mobs,pval=pval))
 }
