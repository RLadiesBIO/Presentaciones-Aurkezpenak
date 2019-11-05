\name{z.test}
\alias{z.test}


\title{
z test
}
\description{
These functions calculate the z.test for normal data with known variance.
}
\usage{
z.test(x, mu = 0, sigma = 1)
}

\arguments{
  \item{x}{
Vector following a normal distribution.
}
  \item{mu}{
Numeric. Mean.
}
  \item{sigma}{
Numeric. Standard deviation.
}
}

\value{
\item{mean}{Numeric. Mean}
\item{z}{Numeric. Z statistic}
\item{p-value}{Numeric. P-value}
}

\examples{
z.test(dataset)
}

