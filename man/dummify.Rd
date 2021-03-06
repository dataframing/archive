% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pre-processing.R
\name{dummify}
\alias{dummify}
\title{Convert categorical variables into dummy/indicator variables}
\usage{
dummify(data, y = "")
}
\arguments{
\item{data}{any dataframe-esque object containing a mix of factor and numerical columns.}

\item{y}{A response column. Pass this value to the function if the response (y) column
is a factor within :data that you would like to NOT be dummified. If not passed, we assume
all factors are free to be dummified. See 'Details'.}
}
\description{
This function is the R equivalent of Python's pandas' :get_dummmies(). It's a fairly
simple wrapper around \code{\link[caret]{dummyVars}}. This function binarizes all explicitly-factor
columns within the \code{data} argument. This function can be used in one of two ways
-- see 'Details'. TODO: support y as a quosure. TODO: change :y to :ignore vector.
}
\details{
\enumerate{
  \item \code{dummify(x)}: no y parameter passed, so all factor columns are binarized
  \item \code{dummify(x, y = 'col_name')}: y parameter passed, so all factor columns except y are binarized.
  \itemize{
    \item{Note: }{For this second approach, the y column MUST be a factor. You've been warned.}
  }
}
}
\examples{
x <- data.frame(
  id     = seq(1, 10),
  age    = rpois(n=10, lambda=20),
  gender = rep(c('M', 'F'), 5),
  blood.type = factor(rep(c('A', 'B'), 5))
)

# Only expand `blood.type` factor column (`gender` is ignored because it's a character)
dummify(x)

x$gender <- factor(x$gender)

# Expand all columns into binary representations.
dummify(x)

# Ignoring `gender`, binarize all other factors.
dummify(x, y = 'blood.type')

}
