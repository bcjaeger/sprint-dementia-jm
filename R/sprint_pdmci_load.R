#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

sprint_pdmci_load <- function(status_variable_name,
                              sbp_divby = 10) {

 fpath <- file.path("Z:","sgaussoin","Williamson BP Decline from Baseline")

 data_paths <- list.files(fpath, pattern = '\\.csv$', full.names = TRUE)

 data_names <- list.files(fpath, pattern = '\\.csv$') %>%
  str_remove("\\.csv$")

 data_in <- map(.x = data_paths,
                .f = read_csv,
                show_col_types = FALSE) %>%
  set_names(data_names) %>%
  map(drop_na, age, race4, edu_4cat, sbpavg)

 data_participants <-
  read_csv('../../npajewski/NDI/Data/longterm_death.csv') %>%
  transmute(
   pid,
   age_cat = factor(sub_senior,
                    levels = c(0, 1),
                    labels = c("lt75_years", "gteq75_years")),
   sex = factor(female,
                levels = c(0,1),
                labels = c("Male","Female")),
   race = factor(is.na(race_black),
                 levels = c(TRUE, FALSE),
                 labels = c("Non_Black", "Black")),
   ckd = factor(eGFR_CKDEPI < 60,
                levels = c(FALSE, TRUE),
                labels = c("No", "Yes")),
   # New creatinine based race-agnostic eGFR equation
   egfr_2021 = ckd_epi_2021_compute(age = age,
                                    sex = sex,
                                    screat = screat),
   ckd_2021 = factor(egfr_2021 < 60,
                     levels = c(FALSE, TRUE),
                     labels = c("No", "Yes")),
   moca = factor(moca_status,
                 levels = c(0, 1),
                 labels = c("gt10th_percentile",
                            "lteq10th_percentile")),
   frail = factor(frail_status,
                  levels = c(0,1,2),
                  labels = c("Fit",
                             "Pre_frail",
                             "Frail"))
  )

 # survival data

 data_surv <- data_in %>%
  getElement(glue("{status_variable_name}_bp_longitudinal_withbasebp")) %>%
  rename(status = !!status_variable_name,
         stop = bp_yrs) %>%
  group_by(pid) %>%
  slice(n()) %>%
  transmute(
   pid,
   stop,
   status,
   intensive,
   age,
   race4,
   education = factor(edu_4cat,
                     levels = 1:4,
                     labels = c("HS or less",
                                "HS or less",
                                "Post HS or college",
                                "Post HS or college")),
   edu_4cat = factor(edu_4cat,
                     levels = 1:4,
                     labels = c("Less than HS",
                                "HS",
                                "Post HS",
                                "College"))
  )

 data_long <- data_in %>%
  getElement(glue("{status_variable_name}_bp_longitudinal_withbasebp")) %>%
  mutate(
   sbpavg = sbpavg / sbp_divby
  ) %>%
  select(pid,
         stop = bp_yrs,
         status = !!status_variable_name,
         sbpavg,
         age,
         intensive) %>%
  left_join(select(data_surv, pid, starts_with("edu"))) %>%
  group_by(pid)

 list(surv = data_surv,
      long = data_long) %>%
  map(left_join, data_participants)

}
