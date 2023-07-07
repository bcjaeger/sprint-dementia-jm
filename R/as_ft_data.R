#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param fits_smry
as_ft_data <- function(fits_smry) {

 rs <- round_spec() %>%
  round_using_magnitude(digits = c(2,1,0), breaks = c(10, 100, Inf))

 ft_data <- fits_smry %>%
  mutate(
   across(c(est, ci_lower, ci_upper),
          ~if_else(term == 'value(sbpavg)', -.x, .x)),
   across(c(est, ci_lower, ci_upper), exp),
   tbl_value = if_else(
    term == 'value(sbpavg)',
    table_glue("{est} ({ci_upper}, {ci_lower})", rspec = rs),
    table_glue("{est} ({ci_lower}, {ci_upper})", rspec = rs)
   )
  ) %>%
  select(-c(est, stdev, ci_lower, ci_upper, Rhat, pvalue)) %>%
  mutate(
   outcome = recode(outcome,
                    pd = 'Probable dementia',
                    mci = 'Mild cognitive impairment',
                    pdmci = 'Composite PD or MCI'),
   control = recode(control,
                    A = "Model 1",
                    B = "Model 2",
                    C = "Model 3")
  ) %>%
  pivot_wider(values_from = tbl_value, names_from = control) %>%
  split(.$outcome) %>%
  map_dfr(
   ~ add_row(.x,
             .before = 2,
             outcome = .x$outcome[1],
             term = "edu_4cat3",
             `Model 1` = '1 (Reference)',
             `Model 2` = '1 (Reference)',
             `Model 3` = '1 (Reference)') %>%
    add_row(.before = 2,
            outcome = .x$outcome[1],
            term = "Education") %>%
    add_row(.before = 7,
            outcome = .x$outcome[1],
            term = "race4BLACK",
            `Model 1` = '1 (Reference)',
            `Model 2` = '1 (Reference)',
            `Model 3` = '1 (Reference)') %>%
    add_row(.before = 7,
            outcome = .x$outcome[1],
            term = "Race")
  ) %>%
  mutate(
   indent = as.numeric(str_detect(term, '^edu_4cat|^race4')),
   term = recode(
    term,
    edu_4cat1 = "Less than HS",
    edu_4cat2 = "HS diploma",
    edu_4cat3 = "Post HS",
    edu_4cat4 = "College degree",
    race4BLACK = "Black",
    race4WHITE = "White",
    race4OTHER = "Other",
    race4HISPANIC = "Hispanic",
    "value(sbpavg)" = "Systolic blood pressure, per 10 mm Hg decrease",
    age = "Age, per year"
   ),
   outcome = factor(outcome, levels = c('Probable dementia',
                                        'Mild cognitive impairment',
                                        'Composite PD or MCI'))
  ) %>%
  arrange(outcome)

}
