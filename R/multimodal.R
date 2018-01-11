
#' Build multimodal trips from trajectories
#'
#' @description Build multimodal trips from trajectories.
#'
#' @param data The trajectories object built with \code{palms_calc_trajectories}.
#' @param spatial_threshold Spatial threshold in meters
#' @param temporal_threshold Temporal threshold in minutes
#' @param verbose Print progress after each step. Default is \code{TRUE}.
#'
#' @details Several columns are required in the \code{trajectories} dataset. These
#' need to be added as trajectory fields:
#' \itemize{
#' \item identifier
#' \item tripnumber
#' \item mot
#' \item start
#' \item end
#' \item geometry
#' }
#'
#' @return The input trajectories LINESTRING geometry, collapsed into multimodal trips
#'
#' @examples
#' #multimodal <- palms_calc_multimodal(trajectories, 200, 10)
#'
#' @export
palms_build_multimodal <- function(data, spatial_threshold,
                                  temporal_threshold, verbose = TRUE) {

  if (!all(c("identifier", "tripnumber", "start", "end", "geometry", "mot") %in% colnames(data)))
    stop("Your trajectories data does not contain the required column names...")

  if(verbose) cat('Calculating multimodal eligibility...')

  # Determine if a trajectory meets satial and temporal criteria
  data <- data %>%
    arrange(identifier, tripnumber) %>%
    mutate(time_diff = difftime(start, lag(end), units = "mins")) %>%
    group_by(identifier, tripnumber) %>%
    mutate(start_point = st_as_text(st_cast(geometry, "POINT")[1]),
           end_point = st_as_text(st_cast(geometry, "POINT")[length(st_cast(geometry, "POINT"))])) %>%
    ungroup() %>%
    mutate(end_prev = lag(end_point, default = start_point[1])) %>%
    group_by(identifier, tripnumber) %>%
    mutate(distance_diff = distGeo(
      matrix(c(st_as_sfc(end_prev, crs = 4326)[[1]][1],
               st_as_sfc(end_prev, crs = 4326)[[1]][2]), ncol = 2),
      matrix(c(st_as_sfc(start_point, crs = 4326)[[1]][1],
               st_as_sfc(start_point, crs = 4326)[[1]][2]), ncol = 2))) %>%
    ungroup() %>%
    mutate(mmt_criteria = ((distance_diff < spatial_threshold) & (time_diff < temporal_threshold)),
           mmt_number = NA)

  if(verbose) cat('done\nAssigning trip numbers...')

  # Assign correct start times for consecutive mmt segments
  for(i in 1:(nrow(data)-1)) {
    data$mmt_number[i] <- ifelse((!data$mmt_criteria[i]) & data$mmt_criteria[i+1], data$start[i],
                        ifelse(data$mmt_criteria[i], data$mmt_number[i-1], data$start[i]))
  }

  data$mmt_number[nrow(data)] <- ifelse(data$mmt_criteria[nrow(data)], data$mmt_number[nrow(data)-1],
                                        data$start[nrow(data)])

  # Use run-length encoding to assign mmt numbers
  data <- data %>%
    group_by(identifier) %>%
    mutate(mmt_number = rleid(mmt_number)) %>%
    ungroup() %>%
    select(-c(start_point, end_point, end_prev, mmt_criteria, time_diff, distance_diff))

  if(verbose) cat('done\nCalculating fields...')

  if (exists("multimodal_fields")) {

    # Split varables into each mot
    mot_split <- data %>%
      select(c("mot", "mmt_number", "identifier", "geometry", multimodal_fields$name)) %>%
      mutate(mot = paste0("mot_", mot)) %>%
      gather(variable, value, -mmt_number, -mot, -identifier, -geometry) %>%
      unite(col, mot, variable) %>%
      spread(col, value) %>%
      arrange(identifier, mmt_number) %>%
      cbind(data) %>%
      select(-ends_with(".1"))

    # Calculate multimodal_fields
    df_fields <- list()

    for (i in unique(multimodal_fields$func)) {
      df_fields[[i]] <- mot_split %>%
        as.data.frame() %>%
        group_by(identifier, mmt_number) %>%
        summarise_at(vars(matches(
          paste(multimodal_fields$name[multimodal_fields$func == i], collapse = "|"))),
                          i, na.rm = TRUE)
    }

    df_fields <- reduce(df_fields, left_join,
      by = c("identifier" = "identifier", "mmt_number" = "mmt_number"))

    df_fields[is.na(df_fields)] <- NA

  } else
    mot_split <- data

  # Build trajectory_location formulas if they exist
  if (exists("trajectory_locations")) {

    names <- unique(c(trajectory_locations$start_criteria,
      trajectory_locations$end_criteria))

    # Rather than recalculating geometry, just lookup in palmsplus
    stopifnot(exists("palmsplus"))

    lookup <- palmsplus %>%
      filter(tripnumber > 0 & triptype %in% c(1, 4)) %>%
      as.data.frame() %>%
      select(c("identifier", "tripnumber", "triptype", names)) %>%
      as.data.table()

    args_locations <- setNames(
      paste0("lookup[tripnumber==start_trip & triptype==1 & identifier==first(identifier),",
        trajectory_locations[[2]],
        "] & lookup[tripnumber==end_trip & triptype==4  & identifier==first(identifier),",
        trajectory_locations[[3]], "]"),
        trajectory_locations[[1]]) %>%
      lapply(parse_expr)
  } else
    args_locations <- NULL

  # Calculate other fields (+ trajectory_locations)
  df_other <- mot_split %>%
    group_by(identifier, mmt_number) %>%
    summarise(start_trip = first(tripnumber),
              end_trip = last(tripnumber),
              trip_numbers = paste0(tripnumber, collapse = "-"),
              n_segments = n(),
              mot_order = paste0(mot, collapse = "-"),
              start = first(start),
              end = last(end),
              !!!args_locations,
              do_union = FALSE) %>%
    ungroup() %>%
    select(-c(start_trip, end_trip)) %>%
    mutate_if(is.logical, as.integer)

  if (exists("df_fields"))
    df <- reduce(list(df_other, df_fields), left_join,
      by = c("identifier" = "identifier", "mmt_number" = "mmt_number"))
  else
    df <- df_other

  if(verbose) cat('done\n')
  return(df)
}
