
#' Build multimodal trips from trajectories
#'
#' @param data The trajectories object built with \code{palms_calc_trajectories}.
#' @param spatial_threshold Spatial threshold in meters
#' @param temporal_threshold Temporal threshold in minutes
#' @param verbose Print progress after each step. Default is \code{TRUE}.
#'
#' @return The input trajectories LINESTRING geometry, collapsed into multimodal trips
#'
#' @examples
#' #multimodal <- palms_calc_multimodal(trajectories, 200, 10)
#'
#' @export
palms_calc_multimodal <- function(data, spatial_threshold,
                                  temporal_threshold, verbose = TRUE) {

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

  # TODO, make these summary columns dynamic - e.g. from trajectory fields
  cols <- c("mvpa", "speed", "length", "duration", "nonwear", "wear",
            "sedentary", "light", "moderate", "vigorous",
            "mot", "mmt_number", "identifier", "geometry")

  # Split varables into each mot
  mot_split <- data %>%
    select(cols) %>%
    mutate(mot = paste0("mot_", mot)) %>%
    gather(variable, value, -mmt_number, -mot, -identifier, -geometry) %>%
    unite(col, mot, variable) %>%
    spread(col, value) %>%
    arrange(identifier, mmt_number) %>%
    cbind(data) %>%
    select(-ends_with(".1"))

  # Sums
  df_sum <- mot_split %>%
    as.data.frame() %>%
    group_by(identifier, mmt_number) %>%
    summarise_at(vars(duration, sedentary, light, moderate, vigorous, mvpa,
                      starts_with("mot_")), sum, na.rm = TRUE) %>%
    select(-ends_with("_speed"))

  # Means
  df_mean <- mot_split %>%
    as.data.frame() %>%
    group_by(identifier, mmt_number) %>%
    summarise_at(vars(ends_with("_speed")), mean, na.rm = TRUE)

  df_mean[is.na(df_mean)] <- NA

  # Others
  df_summary <- mot_split %>%
    group_by(identifier, mmt_number) %>%
    summarise(trip_numbers = paste0(tripnumber, collapse = "-"),
              n_segments = n(),
              mot_order = paste0(mot, collapse = "-"),
              start = first(start),
              end = last(end),
              do_union = FALSE)

  df <- reduce(list(df_summary, df_sum, df_mean), left_join,
               by = c("identifier" = "identifier", "mmt_number" = "mmt_number"))

  if(verbose) cat('done\nRebuilding geometry...')

  # Rebuild each geometry into one LINESTRING
  geometry <- list()
  for(i in 1:nrow(df)) {
    geometry[[i]] <- st_as_text(st_linestring(do.call(rbind, unlist(df$geometry[i], recursive = FALSE))))
  }

  x <- bind_rows(tibble(geometry))
  st_geometry(df) <- st_as_sfc(geometry, crs = 4326)

  if(verbose) cat('done\n')

  return(df)
}


