#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param fit
jm_summarize <- function(fit_joint_list) {

 map(fit_joint_list, .f = jm_summarize_)

}

jm_summarize_ <- function(fit_joint){

 out <- fit_joint

 if(inherits(fit_joint, 'jm')){

  fit_joint_smry <- summary(fit_joint)

  out <- fit_joint_smry[c("Outcome1", "Survival")] %>%
   map(
    ~ as_tibble(.x, rownames = 'term') %>%
     rename(est = Mean,
            stdev = StDev,
            ci_lower = `2.5%`,
            ci_upper = `97.5%`,
            pvalue = P)
   ) %>%
   set_names(c("longitudinal", "survival")) %>%
   bind_rows(.id = 'model')

 }

 out

}
