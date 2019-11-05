\name{IC.z.test}
\alias{IC.z.test}

\title{
Confidence Interval for the z.test.
}

\description{
These functions calculate the confidence interval for normal data with known variance.
}
\usage{
IC.z.test(x, sigma, conf.level = 0.95)
}


\arguments{
  \item{x}{
Vector following a normal distribution.
}
  \item{sigma}{
Numeric. Standard deviation.
}
  \item{conf.level}{
Numeric. Confidence Level.
}
}

\value{
\item{IC}{Returns the confidence interval}
}


\examples{
z.test(dataset)
IC.z.test(datasets, sd(dataset), 0.95)
}

