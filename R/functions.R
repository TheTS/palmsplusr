#' Adds an aggregation domain to the palmsplus_domains table.
#'
#' @param name The name of the domain as a string.
#' @param formula The formula as a string. The formula can contain variables
#' that are created with \code{palms_add_field}. Make sure to escape quotation
#' marks that are part of the formula.
#'
#' @return If the palmsplus_domains table is not present in the global environment,
#' it will be created. If it already exists, the new domain will be appended.
#'
#' @examples
#' #palms_add_domain("d_home", "at_home")
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

#' Adds an aggregation field to the palmsplus_fields table.
#'
#' @param name The name of the field as a string.
#' @param formula The formula as a string.
#' @param domain_field Logical. If TRUE, this field will be summarised for each
#' domain when \code{palms_calc_days} is run. If this is just an intermediary
#' variable this should be FALSE.
#'
#' @return If the palmsplus_fields table is not present in the global environment,
#' it will be created. If it already exists, the new field will be appended.
#'
#' @examples
#' #palms_add_field("pedestrian", "tripmot == 1")
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

#' Adds a trajectory field to the trajectory_fields table.
#'
#' @param name The name of the field as a string.
#' @param formula The formula as a string.
#' @param after_conversion Logical. If TRUE, this field will be calculated after
#' the trajectory LINESTRING has been created. For example, st_length().
#'
#' @return If the trajectory_fields table is not present in the global environment,
#' it will be created. If it already exists, the new field will be appended.
#'
#' @examples
#' #palms_add_trajectory_field("mot", "first(tripmot)")
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

#' Adds a trip location to the trajectory_locations table.
#'
#' @param name The name of the field as a string.
#' @param start_criteria The start of the trip. This should be a field in
#' palms plus, and evaluate to a logical.
#' @param end_criteria The start of the trip. This should be a field in
#' palms plus, and evaluate to a logical.
#'
#' @return If the trajectory_locations table is not present in the global environment,
#' it will be created. If it already exists, the new location will be appended.
#'
#' @examples
#' #palms_add_trip_location("home_school", "at_home", "at_school")
#'
#' @export
palms_add_trip_location <- function(name, start_criteria, end_criteria) {
  if (!exists("trajectory_locations"))
    trajectory_locations <<- tibble(name = name, start_criteria = start_criteria, end_criteria = end_criteria)
  else if (name %in% trajectory_locations$name)
    stop(name, " already exists in trajectory_locations")
  else
    trajectory_locations <<- rbind(trajectory_locations, c(name, start_criteria, end_criteria))
}

#' Returns a buffer (polygon) of the input geometry.
#'
#' @param point The point to buffer
#' @param distance The buffer distance. Default \code{100}.
#' @param crs The projection to convert to before buffering. Default \code{2193}.
#'
#' @return A polygon representing a buffer of the input feature. This is always
#' returned in the ESPG:4326 projection.
#'
#' @examples
#' #p_buffer_1000 <- palms_buffer(p, 1000)
#'
#' @export
palms_buffer <- function(point, distance = 100, crs = 2193) {
  point %>%
    st_transform(crs) %>%
    st_buffer(distance) %>%
    st_transform(4326)
}

#' Returns a logical vector if input points are inside polygons
#'
#' @param data The data points.
#' @param polygons The polygon feature.
#' @param collapse_var An optional parameter for aggragating polygons by a
#' variable. Essentially this dissolves the polygon layer by \code{collapse_var}.
#'
#' @return A logical vector the length of the input data points. Each polygon
#' will return an additional column
#'
#' @examples
#' #data$is_home <- palms_in_polygon(data, homes, "identifier")
#'
#' @export
palms_in_polygon <- function(data, polygons, collapse_var = NULL){

  collapse_var <- quo_text(enquo(collapse_var))

   if (!(collapse_var == "NULL"))
    polygons <- aggregate(polygons, list(polygons[[collapse_var]]), function(x) x[1])

  suppressMessages( # Supresses the 'planar coordinates' warning
    st_contains(polygons, data, sparse = FALSE) %>% as.vector(.)
  )
}

#' Returns a logical vector if points are within a time period
#'
#' @param data The data points
#' @param pid The participant identifier
#' @param timetable The class timetable table
#' @param basis The participant basis table
#' @param start_col The column in \code{timetable} containing period start times
#' @param end_col The column in \code{timetable} containing period end times
#'
#' @return A logical vector the length of the input data points.
#'
#' @examples
#' #data <- ...
#' #ct <- read.csv("class_timetable.csv")
#' #pb <- read.csv("participant_basis.csv")
#' #data$in_recess <- palms_in_time(data, "BC0627", ct, pb, "recess_start", "recess_end")
#'
#' @export
palms_in_time <- function(data, pid, timetable, basis, start_col, end_col) {
  start_col <- enquo(start_col)
  end_col <- enquo(end_col)

  s_id <- as.numeric(basis[basis$identifier == pid, "school_id"])
  c_id <- as.numeric(basis[basis$identifier == pid, "class_id"])

  dates <- timetable %>%
    filter((school_id == s_id) & (class_id == c_id)) %>%
    select(!!start_col, !!end_col) %>%
    mutate_all(as.POSIXct)

  as.vector(data$datetime %in% unlist(Map(`:`, dates[[1]], dates[[2]])))
}

#' Returns the epoch length of the PALMS dataset
#'
#' @param data The data points
#'
#' @return An integer representing the seconds between the first and second
#' data points.
#'
#' @examples
#' #epoch <- palms_epoch_length(data)
#'
#' @export
palms_epoch_length <- function(data) {
  times <- data[1:2,] %>%
    st_set_geometry(NULL) %>%
    select(datetime) %>%
    as.data.frame()

  return(as.numeric(difftime(times[2,1], times[1,1], units = "secs")))
}

#' Returns the start or end point of a LINESTRING
#'
#' @param line The line geometry
#' @param first_point Logical. If true, the first point will be returned.
#'
#' @return A POINT geometry feature representing the first or last point in the
#' LINESTRING
#'
#' @examples
#' #start <- palms_line_point(line, TRUE)
#' #end <- palms_line_point(line, FALSE)
#'
#' @export
palms_line_point <- function(line, first_point) {
  x <- st_cast(line, "POINT")
  if (first_point)
    return(st_sfc(x[[1]], crs = st_crs(line)))
  else
    return(st_sfc(x[[length(x)]], crs = st_crs(line)))
}

#' Returns true if a LINESTRING starts and ends in specific polygons
#'
#' @param line The line geometry
#' @param start_loc A polygon representing the start location
#' @param end_loc A polygon representing the end location
#' @param collapse_start An optional parameter for aggragating polygons.
#' See \code{\link{palms_in_polygon}}.
#' @param collapse_end An optional parameter for aggragating polygons.
#'
#' @return A logical. True if the LINESTRING starts inside \code{start_loc} and
#' ends inside \code{end_loc}.
#'
#' @examples
#' #school_trip <- palms_trip(line, home, school)
#'
#' @export
palms_trip <- function(line, start_loc, end_loc, collapse_start=NULL, collapse_end=NULL) {
  start_point <- palms_line_point(line, TRUE)
  end_point <- palms_line_point(line, FALSE)

  start <- palms_in_polygon(start_point, start_loc, collapse_start)
  end <- palms_in_polygon(end_point, end_loc, collapse_end)

  return(start & end)
}

#' @export
palms_remove_tables <- function() {
  if (exists("palmsplus_domains")) {rm(palmsplus_domains, envir =  globalenv())}
  if (exists("palmsplus_fields")) {rm(palmsplus_fields, envir =  globalenv())}
  if (exists("trajectory_fields")) {rm(trajectory_fields, envir =  globalenv())}
  if (exists("trajectory_locations")) {rm(trajectory_locations, envir =  globalenv())}
}

