% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/palms_build_palmsplus.R
\name{palms_build_palmsplus}
\alias{palms_build_palmsplus}
\title{Build the palmsplus dataset}
\usage{
palms_build_palmsplus(data, verbose = TRUE)
}
\arguments{
\item{data}{The PALMS data obtained using \code{\link{read_palms}}.}

\item{verbose}{Print progress to console after each iteration. Default is \code{TRUE}.}
}
\description{
Build the \code{palmsplus} dataset by adding additional columns to the PALMS dataset.
The additional columns are specified using \code{\link{palms_add_field}}.
}
\examples{
data("palms")

palms_add_field("weekend", "dow > 5")
palmsplus <- palms_build_palmsplus(palms)

}
