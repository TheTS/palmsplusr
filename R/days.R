#' Summarises palmsplus data by aggregation domain
#'
#' @param data The palmsplus data obtained from \code{palms_calc_plusplus}.
#'
#' @return A table summaised by day. Each day has columns for each domain and
#' each field specified in \code{palms_add_domain} and \code{palms_add_field}.
#'
#' @examples
#' #days <- palms_calc_days(palmsplus)
#'
#' @export
palms_calc_days <- function(data) {

  if (!exists("palmsplus_domains"))
    stop("No palmsplus domains have been added. Remember to rebuild palmsplus after adding new domains.")

  domains <- palmsplus_domains[[1]]
  fields <- palmsplus_fields %>% filter(domain_field == TRUE) %>% pull(name)

  epoch <- palms_epoch_length(data)

  data <- data %>%
    st_set_geometry(NULL) %>%
    select(identifier, datetime, domains, fields) %>%
    mutate_at(vars(-identifier,-datetime), funs(. * epoch / 60)) %>%
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
