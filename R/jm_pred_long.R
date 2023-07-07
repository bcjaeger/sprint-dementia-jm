#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ...

jm_pred_long <- function(fit,
                         subset_variable = NULL,
                         subset_levels = NULL){

 # pid, education, sbpavg, and race variable values are not relevant.
 # They are included to bypass the jm error catchers.
 # They do not impact longitudinal model estimates
 if(!inherits(fit, 'jm')) return(NULL)

 newdata <- expand.grid(
  pid = 2,
  sbpavg = 15,
  age = 68,
  age_cat = c('lt75_years', 'gteq_75years'),
  stop = c(2.5, 5),
  intensive = c(0, 1),
  # need to include all levels for compatibility with predict.jm,
  # even though these levels are not used in the jm longitudinal mdl
  sex = c("Male", "Female"),
  education = c("HS or less", "Post HS or college"),
  race = c("Non_Black", "Black"),
  stringsAsFactors = TRUE
 ) %>%
  filter(sex == "Male",
         education == 'Post HS or college',
         race == 'Black')

 if(!is.null(subset_variable)){

  subset_data <- data.frame(temp = subset_levels)
  names(subset_data) <- subset_variable

  newdata[[subset_variable]] <- NULL
  newdata <- cross_join(newdata, subset_data)
  newdata[[subset_variable]] <- factor(newdata[[subset_variable]],
                                       levels = subset_levels)

 }

 output <- newdata %>%
  split(.$intensive) %>%
  map(
   ~ predict(fit,
             newdata = .x,
             process = 'longitudinal',
             type = 'mean_subject',
             return_newdata = TRUE)
  ) %>%
  bind_rows(.id = 'intensive') %>%
  mutate(sbpavg = round(pred_sbpavg*10))

 keep_cols <- c(subset_variable, 'stop', 'intensive', 'pred_sbpavg')

 output[, keep_cols]

}
