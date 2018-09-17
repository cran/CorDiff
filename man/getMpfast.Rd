\name{getMpfast}
\alias{getMpfast}
\title{
Calculate the statistic M
}
\description{
This function provides the permutation algorithm to calculate the maximum statistic M
}
\usage{
getMpfast(y, x, num.perms = 1000)
}
\arguments{
  \item{y}{
Experimental condition/phenotypes, it can be discrete or continuous
}
  \item{x}{
The data matrix, each column is for each sample and each row is for different feature. 
}
  \item{num.perms}{
You can specifiy the number of permutation in the calculation. The default is 1000. 
}
}

\value{
\item{Mobs}{M statistic}
\item{pval}{p value under permutation}
}
\references{
Set-based differential covariance testing for genomics
}
\author{
Yi-Hui Zhou
}

