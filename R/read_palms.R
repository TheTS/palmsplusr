
#' Read PALMS data from a csv file
#'
#' @param file Path to the PALMS csv file.
#'
#' @return A \code{sf data.frame} of PALMS data represented as \code{POINT} geometry.
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
read_palms <- function(file) {
  palms <- read_csv(file)
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

  miss <- setdiff(check_names, names(palms))

  if (length(miss) > 0)
    stop("Your palms input file is missing columns: ", paste(miss, sep = ", "))


  extra <- setdiff(names(palms), check_names)

  if (length(extra) > 0)
    message("Your palms input file has extra columns: ", paste(extra, sep = ", "), "\n")

  message("Column name check passed. Converting to sf dataframe...\n")
  return(st_as_sf(palms, coords = c("lon", "lat"), crs = 4326))
}
