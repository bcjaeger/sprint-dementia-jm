#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param x
#' @param tar_name
clean_control <- function(x,
                          tar_name = "fit_summary") {

 as_tibble(x) %>%
  mutate(
   control = str_remove(control, pattern = glue("{tar_name}_")),
   control = str_replace(control, "_", "\\.\\.")
  ) %>%
  separate(col = control,
           into = c("outcome", "subgroup"),
           sep = "\\.\\.")

}
