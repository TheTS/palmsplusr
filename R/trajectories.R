
#' Build trajectories from trip points
#'
#' @param data The palms data
#'
#' @return A table of individual trips represented as LINESTRING geometry. Each
#' trip has a number of fields specified in \code{palms_add_trajectory_field}.
#'
#' @examples
#' #trajectories <- palms_calc_trajectories(palms)
#'
#' @export
palms_calc_trajectories <- function(data) {

  args <- trajectory_fields %>% filter(after_conversion == FALSE)
  args_after <- trajectory_fields %>% filter(after_conversion == TRUE)

  args <- setNames(args[[2]], args[[1]]) %>% lapply(parse_expr)
  args_after <- setNames(args_after[[2]], args_after[[1]]) %>% lapply(parse_expr)

  if (exists("trajectory_locations")) {
    args_locations <- setNames(paste0("first(", trajectory_locations[[2]],
                                      ") & last(", trajectory_locations[[3]], ")"),
                               trajectory_locations[[1]]) %>% lapply(parse_expr)
    args <- c(args, args_locations)
  }

  epoch <- palms_epoch_length(data)

  x <- data %>%
    filter(tripnumber > 0) %>%
    group_by(identifier, tripnumber) %>%
    summarise(!!!args, do_union = FALSE) %>%
    st_cast("LINESTRING") %>%
    mutate(!!!args_after) %>%
    mutate_if(is.logical, as.integer)
}





