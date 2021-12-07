
#' Build trajectories from the palmsplus dataset
#'
#' @description Build trajectories (trips) from the \code{palmsplus} dataset. This
#' returns a \code{sf data.frame} with \code{LINESTRING} geometry. Three columns
#' are returned by default (\code{identifier}, \code{tripnumber}, and \code{geometry}).
#' Additional columns can be specified with \code{\link{palms_add_trajectory_field}}
#' and \code{\link{palms_add_trajectory_location}}.
#'
#' @param data The palmsplus data obtained from \code{\link{palms_build_palmsplus}}.
#' @param config_file Path to the config file
#'
#' @return A table of individual trips represented as \code{LINESTRING} geometry.
#'
#' @examples
#' data("palms")
#' palms_remove_tables()
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
#' @import dplyr
#' @import sf
#' @importFrom rlang parse_expr
#' @importFrom stats setNames
#'
#' @export
palms_build_trajectories <- function(data, config_file = NULL) {


  # If using field tables
  if (exists("trajectory_fields") & is.null(config_file)) {
    args <- trajectory_fields %>% filter(after_conversion == FALSE)
    args_after <- trajectory_fields %>% filter(after_conversion == TRUE)

    args <- setNames(args[[2]], args[[1]]) %>% lapply(parse_expr)
    args_after <- setNames(args_after[[2]], args_after[[1]]) %>% lapply(parse_expr)
  } else {
    args <- list()
    args_after <- list()
  }

  # If using config file
  if (!is.null(config_file) & !exists("trajectory_fields")) {
    config <- read_config(config_file) %>%
      filter(context == 'trajectory_field')

    if (nrow(config) > 0) {
      args <- config %>% filter(after_conversion == FALSE)
      args_after <- config %>% filter(after_conversion == TRUE)

      args <- setNames(args$formula, args$name) %>% lapply(parse_expr)
      args_after <- setNames(args_after$formula, args_after$name) %>% lapply(parse_expr)
    } else {
      args <- list()
      args_after <- list()
    }
  }


  # If using field tables
  if (exists("trajectory_locations") & is.null(config_file)) {
    args_locations <- setNames(paste0("first(", trajectory_locations[[2]],
                                      ") & last(", trajectory_locations[[3]], ")"),
                               trajectory_locations[[1]]) %>% lapply(parse_expr)
    args <- c(args, args_locations)
  }


  # If using config file
  if (!is.null(config_file) & !exists("trajectory_fields")) {
    config <- read_config(config_file) %>%
      filter(context == 'trajectory_location')

    if (nrow(config) > 0) {
      args_locations <- setNames(paste0("first(", config$start_criteria,
                                        ") & last(", config$end_criteria, ")"),
                                 config$name) %>% lapply(parse_expr)
      args <- c(args, args_locations)
    }

  }


  data %>%
    filter(tripnumber > 0) %>%
    group_by(identifier, tripnumber) %>%
    summarise(!!!args, do_union = FALSE) %>%
    st_cast("LINESTRING") %>%
    mutate(!!!args_after) %>%
    ungroup() %>%
    mutate_if(is.logical, as.integer)
}





