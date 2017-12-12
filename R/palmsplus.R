#' Adds additional columns to PALMS data that represent domains and fields
#'
#' @param data The palms data
#'
#' @return The input data with additional columns specified in
#' \code{palms_add_field} and \code{palms_add_domain}.
#'
#' @examples
#' #palmsplus <- palms_calc_palmsplus(palms)
#'
#' @export
palms_calc_palmsplus <- function(data) {

  field_args <- setNames(palmsplus_fields[[2]], palmsplus_fields[[1]]) %>%
    lapply(parse_expr)

  domain_args <- setNames(palmsplus_domains[[2]], palmsplus_domains[[1]]) %>%
    lapply(parse_expr)

  args <- c(field_args, domain_args)

  x <- list()
  for (i in unique(data$identifier)) {

    x[[i]] <- data %>%
      filter(identifier == i) %>%
      mutate(!!! args) %>%
      mutate_if(is.logical, as.integer)

    cat("Computed palmsplus for:", i, "\n")
  }

  return(do.call(rbind, x))
}
