#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nameme1
bind_fits <- function(...) {

 inputs <- list(...)

 outputs <- map(inputs, clean_fit)

 outcome_ptrn <- c('\\_pd\\_|\\_mci\\_|\\_pdmci\\_')

 output_hr <- map_dfr(outputs, 'estimate_hr', .id = 'outcome') %>%
  mutate(outcome = str_extract(outcome, pattern = outcome_ptrn),
         outcome = str_remove_all(outcome, pattern = '\\_'))

 output_sbp <- map_dfr(outputs, 'estimate_sbp', .id = 'outcome') %>%
  mutate(outcome = str_extract(outcome, pattern = outcome_ptrn),
         outcome = str_remove_all(outcome, pattern = '\\_'))

 list(estimate_hr = output_hr,
      estimate_sbp = output_sbp)

}


clean_fit <- function(x){

 long_preds <- distinct(x$estimate_sbp)
 est_hr <- x$estimate_hr

 subset_vars <- c('sex', 'race', 'education', 'age_cat')

 subset_var_index <- which(names(long_preds) %in% subset_vars)

 if(!is_empty(subset_var_index)){
  subset_var <- names(long_preds)[subset_var_index]
  long_preds$subset_variable <- subset_var
  names(long_preds)[subset_var_index] <- 'subset_level'
 } else {
  long_preds$subset_level <- 'Overall'
  est_hr$subset_variable <- 'Overall'
  est_hr$subset_level <- 'Overall'
 }

 list(estimate_hr = est_hr,
      estimate_sbp = long_preds) %>%
  map(relocate, subset_variable, subset_level, .before = 1)

}
