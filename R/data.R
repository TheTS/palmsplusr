#' Class timetable
#'
#' A dataset containing
#'
#' @format A data frame with 10 rows and 9 variables:
#'
#' \describe{
#'   \item{school_id}{ID of school}
#'   \item{class_id}{ID of class}
#'   \item{date}{Date}
#'   \item{school_start}{School start time}
#'   \item{school_end}{School end time}
#'   \item{recess1_start}{First recess period start time}
#'   \item{recess1_end}{First recess period end time}
#'   \item{recess2_start}{Second recess period start time}
#'   \item{recess2_end}{Second recess period end time}
#' }
#'
#' @usage data(class_timetable)
#'
"class_timetable"

#' Home points
#'
#' A dataset containing
#'
#' @format A data frame with 2 rows and 3 variables:
#'
#' \describe{
#'   \item{identifier}{Participant identifier}
#'   \item{home_id}{ID of home}
#'   \item{geometry}{Point geometry}
#' }
#'
#' @usage data(home)
#'
"home"

#' PALMS data
#'
#' A dataset containing
#'
#' @format A data frame with 40233 rows and 13 variables:
#'
#' \describe{
#'   \item{identifier}{Participant identifier}
#'   \item{datetime}{Timestamp}
#'   \item{dow}{}
#'   \item{fixtypecode}{}
#'   \item{iov}{}
#'   \item{tripnumber}{}
#'   \item{triptype}{}
#'   \item{tripmot}{}
#'   \item{activity}{}
#'   \item{activityintensity}{}
#'   \item{activityboutnumber}{}
#'   \item{sedentaryboutnumber}{}
#'   \item{geometry}{}
#' }
#'
#' @usage data(palms)
#'
"palms"

#' Participant basis
#'
#' A dataset containing
#'
#' @format A data frame with 1 row and 3 variables:
#'
#' \describe{
#'   \item{identifier}{Participant identifier}
#'   \item{school_id}{ID of school}
#'   \item{class_id}{ID of class}
#' }
#'
#' @usage data(participant_basis)
#'
"participant_basis"


#' School polygons
#'
#' A dataset containing
#'
#' @format A data frame with 1 row and 3 variables:
#'
#' \describe{
#'   \item{school_id}{ID of school}
#'   \item{schoolname}{Name of school}
#'   \item{geometry}{Polygon geometry}
#' }
#'
#' @usage data(school)
#'
"school"
