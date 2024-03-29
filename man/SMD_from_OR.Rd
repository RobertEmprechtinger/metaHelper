% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SMD.R
\name{SMD_from_OR}
\alias{SMD_from_OR}
\title{Standardized Mean Difference from Odds Ratio}
\usage{
SMD_from_OR(OR)
}
\arguments{
\item{OR}{odds ratio}
}
\value{
Standardized Mean Difference
}
\description{
Approximates SMD from OR.
}
\examples{
# Transform an OR of 0.3 to SMD
SMD_from_OR(0.3)

}
\references{
Borenstein, M., Hedges, L.V., Higgins, J.P.T. and Rothstein, H.R. (2009). Converting Among Effect Sizes. In Introduction to Meta-Analysis (eds M. Borenstein, L.V. Hedges, J.P.T. Higgins and H.R. Rothstein). https://doi.org/10.1002/9780470743386.ch7
}
