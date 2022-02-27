
#' Add an aggregation domain to the palmsplus_domains table.
#'
#' @param name The name of the domain as a string.
#' @param formula The formula as a string. This should evaluate to a boolean.
#' The formula can contain variables already created with \code{palms_add_field}
#' or other previously specified domains.
#'
#' Make sure to escape quotation marks that are part of the formula.
#'
#' @return If the palmsplus_domains table is not present in the global environment,
#' it will be created. If it already exists, the new domain will be appended.
#'
#' @examples
#' palms_add_domain("home", "at_home")
#'
#' @export
palms_add_domain <- function(name, formula) {
  if (!exists("palmsplus_domains"))
    palmsplus_domains <<- tibble(name = name, formula = formula)
  else if (name %in% palmsplus_domains$name)
    stop(name, " already exists in palmsplus_domains")
  else
    palmsplus_domains <<- rbind(palmsplus_domains, c(name, formula))
}

#' Add an aggregation field to the palmsplus_fields table.
#'
#' @param name The name of the field as a string.
#' @param formula The formula as a string. The formula can contain other fields
#' previously specified.
#' @param domain_field Logical. If \code{TRUE}, this field will be summarised for each
#' \emph{domain} when \code{palms_build_days} is run.
#'
#' @return If the palmsplus_fields table is not present in the global environment,
#' it will be created. If it already exists, the new field will be appended.
#'
#' @examples
#' palms_add_field("pedestrian", "tripmot == 1")
#' palms_add_field("moving", "pedestrian | tripmot == 2") #notice this contains previous field
#'
#' @export
palms_add_field <- function(name, formula, domain_field = FALSE) {
  if (!exists("palmsplus_fields"))
    palmsplus_fields <<- tibble(name = name, formula = formula, domain_field = domain_field)
  else if (name %in% palmsplus_fields$name)
    stop(name, " already exists in palmsplus_fields")
  else
    palmsplus_fields <<- rbind(palmsplus_fields, c(name, formula, domain_field))
}

#' Add a trajectory field to the trajectory_fields table.
#'
#' @param name The name of the field as a string.
#' @param formula The formula as a string. Note that these formulas act on all
#' palmsplus points that are part of the same trip (\code{tripnumber}).
#' @param after_conversion Logical. If \code{TRUE}, this field will be calculated after
#' the trajectory \code{LINESTRING} has been created. For example, a formula that
#' contains \code{st_length()} can only be evaluated with geometry.
#'
#' @return If the trajectory_fields table is not present in the global environment,
#' it will be created. If it already exists, the new field will be appended.
#'
#' @examples
#' palms_add_trajectory_field("mot", "first(tripmot)")
#'
#' @export
palms_add_trajectory_field <- function(name, formula, after_conversion = FALSE) {

  if (!exists("trajectory_fields"))
    trajectory_fields <<- tibble(name = name, formula = formula, after_conversion = after_conversion)
  else if (name %in% trajectory_fields$name)
    stop(name, " already exists in trajectory_fields")
  else
    trajectory_fields <<- rbind(trajectory_fields, c(name, formula, after_conversion))
}

#' Add a trajectory location to the trajectory_locations table.
#'
#' @description This function builds the trajectory_locations table, which is used
#' by \code{\link{palms_build_trajectories}} to identify trajectories that start
#' and end at specific locations. For example, a trip from home to work, or from
#' school to a park. This is done by setting a start and end criteria.
#'
#' @param name The name of the trip as a string.
#' @param start_criteria The start of the trip. This should evaluate to a boolean.
#' For optimal speed, this should be a field pre-calculated in \code{palmsplus}.
#' @param end_criteria The end of the trip. This should evaluate to a boolean.
#' For optimal speed, this should be a field pre-calculated in \code{palmsplus}.
#'
#' @return If the trajectory_locations table is not present in the global environment,
#' it will be created. If it already exists, the new location will be appended.
#'
#' @examples
#' # A full example workflow
#' data(list = c("palms", "home", "school", "participant_basis"))

#' palms_add_field("at_home", "palms_in_polygon(., filter(palms_buffer(home, distance = 100),
#'                                                        identifier == i), identifier)")
#'
#' palms_add_field("at_school", "palms_in_polygon(., filter(school, school_id == participant_basis %>%
#'                                                   filter(identifier == i) %>% pull(school_id)))")
#' palmsplus <- palms_build_palmsplus(palms)
#'
#' palms_add_trajectory_location("home_school", "at_home", "at_school")
#'
#' trajectories <- palms_build_trajectories
#'
#' @export
palms_add_trajectory_location <- function(name, start_criteria, end_criteria) {
  if (!exists("trajectory_locations"))
    trajectory_locations <<- tibble(name = name, start_criteria = start_criteria, end_criteria = end_criteria)
  else if (name %in% trajectory_locations$name)
    stop(name, " already exists in trajectory_locations")
  else
    trajectory_locations <<- rbind(trajectory_locations, c(name, start_criteria, end_criteria))
}

#' A a multimodal field to the multimodal_fields table
#'
#' @param name The name of the field as a string. This field mulst be present in
#' the \code{trajectories} dataset. This also accepts a vector of names.
#' @param func The summary function to use as a string. This is normally either
#' "sum" or "mean".
#'
#' @return If the multimodal_fields table is not present in the global environment,
#' it will be created. If it already exists, the new field will be appended.
#'
#' @examples
#' palms_add_multimodal_field("mvpa", "sum")
#' palms_add_multimodal_field("speed", "mean")
#'
#' # A vector of names using the same summary function
#' palms_add_multimodal_field(c("sedentary", "light", "moderate", "duration"), "sum")
#'
#' @export
palms_add_multimodal_field <- function(name, func) {
  for (i in name) {
    if (!exists("multimodal_fields"))
      multimodal_fields <<- tibble(name = i, func = func)
    else if (i %in% multimodal_fields$name)
      stop(i, " already exists in multimodal_fields")
    else
      multimodal_fields <<- rbind(multimodal_fields, c(i, func))
  }
}

#' Return a buffer (polygon) of the input geometry.
#'
#' @param point The geometry to buffer
#' @param distance The buffer distance in meters. Default \code{100}.
#' @param crs The projection to convert to before buffering. Default \code{2193}.
#' This is necessary as at units for ESPG:4326 (WGS 84) are degrees, not meters.
#'
#' @return A polygon representing a buffer of the input feature. This is always
#' returned in the ESPG:4326 projection.
#'
#' @examples
#' data("home")
#'
#' home.buffer <- palms_buffer(home, 100)
#'
#' @export
palms_buffer <- function(point, distance = 100, crs = 2193) {
  point %>%
    st_transform(crs) %>%
    st_buffer(distance) %>%
    st_transform(4326)
}

#' Check if points fall inside a polygon
#'
#' @description Returns a logical vector the length of the input \code{data}
#' points. This checks if each point falls inside \code{polygon}. Multiple
#' polygons are supported (e.g., multiple homes, or parks). If the \code{polygon}
#' data is empty, \code{NA} will be returned.
#'
#' @param data The data points.
#' @param polygons The polygon feature.
#' @param collapse_var An optional parameter for aggragating polygons by a
#' variable. Essentially this dissolves the polygon layer by \code{collapse_var}.
#'
#' @return A logical vector the length of the input data points. If multiple
#' polygons are present, an additional column will be returned for each.
#'
#' @examples
#' data(list = c("palms", "home"))
#'
#' home.buffer <- palms_buffer(home, distance = 100)
#'
#' at_home <- palms_in_polygon(palms, home.buffer)
#'
#' @importFrom rlang quo_text enquo
#' @importFrom stats aggregate
#'
#' @export
palms_in_polygon <- function(data, polygons, collapse_var = NULL){

  if (nrow(polygons) < 1) {
    message("palms_in_polygon: Polygon data has 0 rows, returning NA")
    return(NA)
  }

  polygons <- st_make_valid(polygons)

  collapse_var <- quo_text(enquo(collapse_var))

   if (!(collapse_var == "NULL"))
    polygons <- aggregate(polygons, list(polygons[[collapse_var]]), function(x) x[1])

  suppressMessages( # Supresses the 'planar coordinates' warning
    st_contains(polygons, data, sparse = FALSE) %>% as.vector(.)
  )
}

#' Check if points fall between a start and end time
#'
#' @description Returns a logical vector the length of the input \code{data} points.
#' This checks if each point falls between a start and end time. This is designed
#' to work with a \code{timetable} and \code{participant basis} file.
#' \itemize{
#' \item \code{timetable} contains a list of start and end times for different periods
#' of the day, for different groups of people (e.g., classrooms).
#' \item \code{participant basis} contains a list of identifiers and group membership.
#' }
#'
#' @param data The data points.
#' @param identifier The participant identifier.
#' @param timetable The timetable table.
#' @param basis The participant basis table.
#' @param start_col The column in \code{timetable} containing the start time.
#' @param end_col The column in \code{timetable} containing the end time.
#'
#' @return A logical vector the length of the input data points.
#'
#' @examples
#' data(list = c("palms", "participant_basis", "class_timetable"))
#'
#' in_school <- palms_in_time(palms, "BC0627", class_timetable,
#'                            participant_basis, school_start, school_end)
#'
#' @export
palms_in_time <- function(data, identifier, timetable, basis, start_col, end_col) {
  start_col <- enquo(start_col)
  end_col <- enquo(end_col)

  s_id <- as.numeric(basis[basis$identifier == identifier, "school_id"])
  c_id <- as.numeric(basis[basis$identifier == identifier, "class_id"])

  dates <- timetable %>%
    filter((school_id == s_id) & (class_id == c_id)) %>%
    select(!!start_col, !!end_col) %>%
    mutate_all(as.POSIXct)

  as.vector(data$datetime %in% unlist(Map(`:`, dates[[1]], dates[[2]])))
}

#' Get the epoch length of the PALMS dataset
#'
#' @param data The PALMS data obtained using \code{\link{read_palms}}. The
#'\code{palmsplus} dataset also works.
#'
#' @return An integer representing the seconds between the first and second
#' data points.
#'
#' @examples
#' data("palms")
#'
#' epoch <- palms_epoch(palms)
#'
#' epoch
#'
#' @export
palms_epoch <- function(data) {
  times <- data[1:2,] %>%
    st_set_geometry(NULL) %>%
    select(datetime) %>%
    as.data.frame()

  return(as.numeric(difftime(times[2,1], times[1,1], units = "secs")))
}

#' Remove all palmsplusr field and domain tables
#'
#' @description Removes all field and domain tables that exist in the global
#' environment. This is useful when rebuilding tables. This function checks for
#' the following tables:
#' \itemize{
#' \item \code{palmsplus_domains}
#' \item \code{palmsplus_fields}
#' \item \code{trajectory_fields}
#' \item \code{trajectory_locations}
#' \item \code{multimodal_fields}
#' }
#'
#' @examples
#' palms_remove_tables()
#'
#' @export
palms_remove_tables <- function() {
  if (exists("palmsplus_domains")) {rm(palmsplus_domains, envir =  globalenv())}
  if (exists("palmsplus_fields")) {rm(palmsplus_fields, envir =  globalenv())}
  if (exists("trajectory_fields")) {rm(trajectory_fields, envir =  globalenv())}
  if (exists("trajectory_locations")) {rm(trajectory_locations, envir =  globalenv())}
  if (exists("multimodal_fields")) {rm(multimodal_fields, envir =  globalenv())}
}

#' Populates field and domain tables with default values
#'
#' @description This will populate the \code{palmsplus_fields},
#' \code{trajectory_fields} and \code{multimodal_fields} tables with simple fields
#' that should be compatable with all PALMS data sets. The formulas only use
#' variables that are present in the PALMS input dataset.
#'
#' @param epoch_length The epoch length of the PALMS data in seconds. This is
#' necessary so the default formulas are created correctly. This can be passed in
#' using \code{\link{palms_epoch}}.
#'
#' @examples
#'
#' data("palms")
#'
#' palms_remove_tables()
#'
#' epoch <- palms_epoch(palms)
#' epoch
#'
#' palms_load_defaults(epoch)
#'
#' @export
palms_load_defaults <- function(epoch_length) {
  epoch_length <- as.character(epoch_length)

  # palmsplus_fields
  palms_add_field("weekday",    "dow < 6")
  palms_add_field("weekend",    "dow > 5")
  palms_add_field("indoors",    "iov == 3")
  palms_add_field("outdoors",   "iov == 1")
  palms_add_field("in_vehicle", "iov == 2")
  palms_add_field("inserted",   "fixtypecode == 6")
  palms_add_field("pedestrian", "tripmot == 1")
  palms_add_field("bicycle",    "tripmot == 2")
  palms_add_field("vehicle",    "tripmot == 3")
  palms_add_field("nonwear",    "activityintensity < 0",  TRUE)
  palms_add_field("wear",       "activityintensity >= 0", TRUE)
  palms_add_field("sedentary",  "activityintensity == 0", TRUE)
  palms_add_field("light",      "activityintensity == 1", TRUE)
  palms_add_field("moderate",   "activityintensity == 2", TRUE)
  palms_add_field("vigorous",   "activityintensity == 3", TRUE)
  palms_add_field("mvpa",       "moderate + vigorous",    TRUE)

  # trajectory_fields
  palms_add_trajectory_field("mot",       "first(tripmot)")
  palms_add_trajectory_field("date",      "first(as.Date(datetime))")
  palms_add_trajectory_field("start",     "datetime[triptype==1]")
  palms_add_trajectory_field("end",       "datetime[triptype==4]")
  palms_add_trajectory_field("duration",  paste0("as.numeric(difftime(end, start, units = \"secs\") + ", epoch_length, ")"))
  palms_add_trajectory_field("nonwear",   paste0("sum(activityintensity < 0) * ", epoch_length))
  palms_add_trajectory_field("wear",      paste0("sum(activityintensity >= 0) * ", epoch_length))
  palms_add_trajectory_field("sedentary", paste0("sum(activityintensity == 0) * ", epoch_length))
  palms_add_trajectory_field("light",     paste0("sum(activityintensity == 1) * ", epoch_length))
  palms_add_trajectory_field("moderate",  paste0("sum(activityintensity == 2) * ", epoch_length))
  palms_add_trajectory_field("vigorous",  paste0("sum(activityintensity == 3) * ", epoch_length))
  palms_add_trajectory_field("mvpa",      "moderate + vigorous")
  palms_add_trajectory_field("length",    "as.numeric(st_length(geometry))",  TRUE)
  palms_add_trajectory_field("speed",     "(length / duration) * 3.6", TRUE)

  # multimodal_fields
  palms_add_multimodal_field(c("duration", "nonwear", "wear", "sedentary", "light",
                               "moderate", "vigorous", "mvpa", "length"), "sum")
  palms_add_multimodal_field("speed", "mean")
}
