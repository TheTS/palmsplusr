
#' Build the palmsplus dataset
#'
#' @description Build the \code{palmsplus} dataset by adding additional columns to the PALMS dataset.
#' The additional columns are specified using \code{\link{palms_add_field}}.
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
#' @import dplyr
#' @import sf
#' @importFrom rlang parse_expr
#' @importFrom data.table rbindlist
#'
#'
#' @export
palms_build_palmsplus <- function(data, verbose = TRUE) {

  if (!exists("palmsplus_fields")) stop("No palmsplus fields have been added.")

  field_args <- setNames(palmsplus_fields[[2]], palmsplus_fields[[1]]) %>%
    lapply(parse_expr)

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

  data <- rbindlist(x) %>%
    st_set_geometry(data$geometry)
}
