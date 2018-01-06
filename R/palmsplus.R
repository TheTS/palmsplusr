
#' Build the palmsplus dataset
#'
#' @description Build the \code{palmsplus} dataset by adding additional columns to the PALMS dataset.
#' The additional columns are specified using \code{\link{palms_add_field}}, \code{\link{palms_add_domain}},
#' \code{\link{palms_add_trajectory_field}}, and \code{\link{palms_add_trajectory_location}}.
#'
#' @param data The PALMS data obtained using \code{\link{read_palms}}.
#' @param verbose Print progress to console after each iteration. Default is \code{TRUE}.
#'
#' @examples
#' data("palms")
#'
#' palms_add_field("weekend", "dow > 5")
#' palmsplus <- palms_build_palmsplus(palms)
#'
#' @export
palms_build_palmsplus <- function(data, verbose = TRUE) {

  if (!exists("palmsplus_fields")) stop("No palmsplus fields have been added.")

  field_args <- setNames(palmsplus_fields[[2]], palmsplus_fields[[1]]) %>%
    lapply(parse_expr)

  if (exists("palmsplus_domains")) {
    domain_args <- setNames(palmsplus_domains[[2]], palmsplus_domains[[1]]) %>%
      lapply(parse_expr)

    field_args <- c(field_args, domain_args)
  }

  x <- list()
  j <- 1
  len <- length(unique(data$identifier))

  for (i in unique(data$identifier)) {
    x[[i]] <- data %>%
      filter(identifier == i) %>%
      mutate(!!! field_args) %>%
      mutate_if(is.logical, as.integer)

    if (verbose) {
      cat("[", j, "/", len, "] Computed palmsplus for: ", i, "\n", sep = "")
      j <- j + 1
    }
  }

  data <- data.table::rbindlist(x) %>%
    st_set_geometry(data$geometry)
}
