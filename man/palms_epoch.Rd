% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{palms_epoch}
\alias{palms_epoch}
\title{Get the epoch length of the PALMS dataset}
\usage{
palms_epoch(data)
}
\arguments{
\item{data}{The PALMS data obtained using \code{\link{read_palms}}. The
\code{palmsplus} dataset also works.}
}
\value{
An integer representing the seconds between the first and second
data points.
}
\description{
Get the epoch length of the PALMS dataset
}
\examples{
data("palms")

epoch <- palms_epoch(palms)

epoch

}
