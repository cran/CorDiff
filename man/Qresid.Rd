\name{Qresid}
\alias{Qresid}
\title{
Calculate statistic Q.
}
\description{
For the purposes of computing type I error and power, we only need care about p-values that are smallish. If the pvalue is large, we do not care if it's 0.8 or 0.9. When we hit ratio=10, then our current pvalue is 10 standard deviations larger than zero, which is a safe crierion to stop and say we have enough permutations. Therefore we saved a ton of time. 
}
\usage{
Qresid(y, X, numperms = 10000, thresh = 10)
}
\arguments{
   \item{y}{
Experimental condition/phenotypes, it can be discrete or continuous
}
  \item{X}{
The data matrix, each column is for each sample and each row is for different feature. 
}
  \item{numperms}{
The number of permuations.
}
  \item{thresh}{
The threshold we set up to stop the permutation. The default value is 10 which comes from a 10 standard deviation criterion.
}
}
\value{
\item{myp}{P value}
\item{i}{The ith permutaion we stopped the algorithm.}
}
\references{
Set-based differential covariance testing for genomics
}
\author{
Yi-Hui Zhou
}

