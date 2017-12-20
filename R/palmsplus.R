#' Adds additional columns to PALMS data that represent domains and fields
#'
#' @param data The palms data
#' @param verbose Print progress after each iteration. Default is \code{TRUE}.
#'
#' @return The input data with additional columns specified in
#' \code{palms_add_field} and \code{palms_add_domain}.
#'
#' @examples
#' #palmsplus <- palms_calc_palmsplus(palms)
#'
#' @export
palms_calc_palmsplus <- function(data, verbose = TRUE) {

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

  for (identifier in unique(data$identifier)) {
    x[[identifier]] <- data %>%
      filter(identifier == identifier) %>%
      mutate(!!! field_args) %>%
      mutate_if(is.logical, as.integer)

    if (verbose == TRUE)
      cat("[", j, "/", len, "] Computed palmsplus for: ", identifier, "\n", sep = "")
    j <- j + 1
  }

  data <- data.table::rbindlist(x) %>%
    st_set_geometry(data$geometry)
}
