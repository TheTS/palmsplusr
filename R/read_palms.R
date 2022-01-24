
#' Read PALMS data from a csv file
#'
#' @param file Path to the PALMS csv file.
#'
#' @return A \code{sf data.frame} of PALMS data represented as \code{POINT} geometry.
#' @param verbose Print progress after each step. Default is \code{TRUE}.
#' @param habitus Is this data output from HABITUS (as opposed to PALMS, which has different column names). Default is \code{FALSE}.
#'
#' @details This functions checks for the following columns, and will fail if
#' these are not present:
#' \itemize{
#' \item datetime
#' \item dow
#' \item lon
#' \item lat
#' \item fixtypecode
#' \item iov
#' \item tripnumber
#' \item triptype
#' \item tripmot
#' \item activity
#' \item activityintensity
#' \item activityboutnumber
#' \item sedentaryboutnumber
#' }
#' @examples
#' \dontrun{
#' palms <- read_palms('palms_file.csv')
#' }
#'
#' @import sf
#' @importFrom readr read_csv
#'
#' @export
read_palms <- function(file, verbose = TRUE, habitus = FALSE) {

  palms <- read_csv(file, show_col_types = verbose)
  palms <- setNames(palms, tolower(names(palms)))

  check_names <- c("identifier",
                   "datetime",
                   "dow",
                   "lon",
                   "lat",
                   "fixtypecode",
                   "iov",
                   "tripnumber",
                   "triptype",
                   "tripmot",
                   "activity",
                   "activityintensity",
                   "activityboutnumber",
                   "sedentaryboutnumber")

  # Check column names of output from final version of HABITUS. They are different from PALMS?
  if (habitus)
    check_names <- c("id",
                   "timestamp",
                   "dow",
                   "lon",
                   "lat",
                   "fixtypecode",
                   #"iov",
                   "tripnumber",
                   "triptype",
                   "tripmot",
                   "activity",
                   "activityintensity"
                   #"activityboutnumber",
                   #"sedentaryboutnumber"
                   )

  miss <- setdiff(check_names, names(palms))

  if (length(miss) > 0)
    stop("Your palms input file is missing columns: ", paste(miss, sep = ", "))


  extra <- setdiff(names(palms), check_names)

  if (length(extra) > 0)
    message("Your palms input file has extra columns: ", paste(extra, sep = ", "), "\n")

  if (verbose)
    message("Column name check passed. Converting to sf dataframe...\n")

  if (habitus) {
    palms <- rename(palms, "identifier" = "id", "datetime" = "timestamp")
  }

  palms <- filter(palms, !is.na(lat) & !is.na(lon))

  return(st_as_sf(palms, coords = c("lon", "lat"), crs = 4326))
}
