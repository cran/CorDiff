\name{CorDiff-package}
\alias{CorDiff-package}
\alias{CorDiff}
\docType{package}
\title{
Set-based differential covariance testing for genomics
}
\description{
We describe four different summary statistics, to ensure power and flexibility under various settings. This is a uniform framework to test association of covariance matrices with an experimental variable, whether discrete or continuous. (1) A sumation statistic S which is to detect global changes in covariances that are concordantly associated with the experimental variable y; (2) A quadratic form statistic Q which is sensitive to changes that are not directionally concordant; (3) A connectivity statistic C which reflects the tendency for the aggregate magnitude of feature-feature correlations to be associated with y; (4) A maximum statistic M.
}

\author{
Yi-Hui Zhou

Maintainer: Yi-Hui Zhou <yihui_zhou@ncsu.edu>
}
\references{
Set-based differential covariance testing for genomics, Yi-Hui Zhou,  under review
}

\keyword{ package }

\examples{
library(mcc)

n1=5
n2=5
y=c(rep(1/n1,n1),rep(-1/n2,n2))
data(x)
w=(colSums(x))^2
output=getbetap.A(getAmoment(rbind(y,y),w,z=NULL),A=NULL,fix.obs=TRUE)
S.p=output$twosidedp[1]
\donttest{
Qresult=Qresid(y,x,numperms=1e6,thresh=10)
Q.p=Qresult$myp

newx=(t(x)\%*\%x)^2
v=colSums(newx)
output2=getbetap.A(getAmoment(rbind(y,y),v,z=NULL),A=NULL,fix.obs=TRUE)
C.p=output2$twosidedp[1]
M.p=getMpfast(y,x,num.perms=1e4)$pval
}
}
