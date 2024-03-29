% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/palms_build_trajectories.R
\name{palms_build_trajectories}
\alias{palms_build_trajectories}
\title{Build trajectories from the palmsplus dataset}
\usage{
palms_build_trajectories(data)
}
\arguments{
\item{data}{The palmsplus data obtained from \code{\link{palms_build_palmsplus}}.}
}
\value{
A table of individual trips represented as \code{LINESTRING} geometry.
}
\description{
Build trajectories (trips) from the \code{palmsplus} dataset. This
returns a \code{sf data.frame} with \code{LINESTRING} geometry. Three columns
are returned by default (\code{identifier}, \code{tripnumber}, and \code{geometry}).
Additional columns can be specified with \code{\link{palms_add_trajectory_field}}
and \code{\link{palms_add_trajectory_location}}.
}
\examples{
data("palms")
palms_remove_tables()

palms_add_field("mvpa", "activityintensity > 1", TRUE)
palmsplus <- palms_build_palmsplus(palms)

# Just with default columns
trajectories <- palms_build_trajectories(palmsplus)

# Adding new fields before building
palms_add_trajectory_field("mot", "first(tripmot)")
palms_add_trajectory_field("mvpa", "sum(mvpa)")

trajectories <- palms_build_trajectories(palmsplus)

}
