\name{fastresid}
\alias{fastresid}
\title{
Residulize the effect of y away from x
}
\description{
This function is to prepare for the next Q calculation. Basically, Q does not like phenotype y to add complication. Therefore we use this function to get rid of the impact of y.
}
\usage{
fastresid(X, y)
}
\arguments{
  \item{X}{
The data matrix, each column is for each sample and each row is for different feature. 
}
  \item{y}{
Experimental condition/phenotypes, it can be discrete or continuous
}
}
\value{
\item{Xresid}{The new x after residulizing y}
}
\references{
Set-based differential covariance testing for genomics
}
\author{
Yi-Hui Zhou
}