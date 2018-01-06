
#' Build trajectories from the palmsplus dataset
#'
#' @description Build trajectories (trips) from the \code{palmsplus} dataset. This
#' returns a \code{sf data.frame} with \code{LINESTRING} geometry. Three columns
#' are returned by default (\code{identifier}, \code{tripnumber}, and \code{geometry}).
#' Additional columns can be specified with \code{\link{palms_add_trajectory_field}}
#' and \code{\link{palms_add_trajectory_location}}.
#'
#' @param data The palmsplus data obtained from \code{\link{palms_build_palmsplus}}.
#'
#' @return A table of individual trips represented as \code{LINESTRING} geometry.
#'
#' @examples
#' data("palms")
#'
#' palms_add_field("mvpa", "activityintensity > 1", TRUE)
#' palmsplus <- palms_build_palmsplus(palms)
#'
#' # Just with default columns
#' trajectories <- palms_build_trajectories(palmsplus)
#'
#' # Adding new fields before building
#' palms_add_trajectory_field("mot", "first(tripmot)")
#' palms_add_trajectory_field("mvpa", "sum(mvpa)")
#'
#' trajectories <- palms_build_trajectories(palmsplus)
#'
#' @export
palms_build_trajectories <- function(data) {

  if (exists("trajectory_fields")) {
    args <- trajectory_fields %>% filter(after_conversion == FALSE)
    args_after <- trajectory_fields %>% filter(after_conversion == TRUE)

    args <- setNames(args[[2]], args[[1]]) %>% lapply(parse_expr)
    args_after <- setNames(args_after[[2]], args_after[[1]]) %>% lapply(parse_expr)
  } else {
    args <- list()
    args_after <- list()
  }

  if (exists("trajectory_locations")) {
    args_locations <- setNames(paste0("first(", trajectory_locations[[2]],
                                      ") & last(", trajectory_locations[[3]], ")"),
                               trajectory_locations[[1]]) %>% lapply(parse_expr)
    args <- c(args, args_locations)
  }

  data %>%
    filter(tripnumber > 0) %>%
    group_by(identifier, tripnumber) %>%
    summarise(!!!args, do_union = FALSE) %>%
    st_cast("LINESTRING") %>%
    mutate(!!!args_after) %>%
    mutate_if(is.logical, as.integer)
}





