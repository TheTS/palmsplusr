
#' Calculate day-level summaries from the palmsplus dataset
#'
#' @description Build a days dataset by summarising \code{palmsplus}
#' by day and person (\code{identifier}). Not all variables in \code{palmsplus}
#' are summarised, only those specified using \code{\link{palms_add_field}} with
#' the argument \code{domain_field = TRUE}. By default, a \code{duration} field
#' is added (e.g., the total minutes per day).
#'
#' All data are summarised by default. However, additional aggragation \emph{domains}
#' can be specified using \code{\link{palms_add_domain}} before building palmsplus.
#' Domains are a subset of data, such as during school time. All \code{domain_field}
#' variables will be summarised for each \emph{domain} seperatly.
#'
#' @param data The palmsplus data obtained from \code{\link{palms_build_palmsplus}}.
#'
#' @return A table summarised by day.
#'
#' @examples
#' data("palms")
#' palms_remove_tables()
#'
#' # Add a field, and make it a domain_field
#' palms_add_field("mvpa", "activityintensity > 1", TRUE)
#'
#' palms_add_domain("walking", "tripmot == 3")
#' palmsplus <- palms_build_palmsplus(palms)
#'
#' # This will have 'walking' and 'total' domains
#' # Each domain will have 'duration' and 'mvpa' fields
#' days <- palms_build_days(palmsplus)
#'
#' @export
palms_build_days <- function(data) {

  domains = "total"
  data$total <- 1

  if (!exists("palmsplus_domains"))
    message("palms_build_days: No domains have been added - using totals only.")
  else
    domains <- c(domains, palmsplus_domains[[1]])

  fields <- palmsplus_fields %>% filter(domain_field == TRUE) %>% pull(name)

  data <- data %>%
    st_set_geometry(NULL) %>%
    select(identifier, datetime, domains, fields) %>%
    mutate(duration = 1) %>%
    mutate_at(vars(-identifier,-datetime), funs(. * palms_epoch(data) / 60)) %>%
    group_by(identifier, date = as.Date(datetime)) %>%
    select(-datetime)

  x <- list()
  for (i in domains) {
    x[[i]] <- data %>%
      filter(UQ(as.name(i)) > 0) %>%
      select(-one_of(domains), duration) %>%
      summarise_all(funs(sum(.))) %>%
      ungroup() %>%
      rename_at(vars(-identifier, -date), funs(paste0(i, "_", .)))
  }

  result <- x %>%
    reduce(left_join, by = c("identifier" = "identifier", "date" = "date"))
  return(result)
}
