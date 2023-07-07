#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data
#' @param fix
#' @param cov
jm_fit <- function(data,
                   outcome,
                   fix,
                   cov,
                   subset_variable,
                   return_model = FALSE) {

 data_model <- list(surv = data[[outcome]]$surv,
                    long = data[[outcome]]$long)

 if(subset_variable != 'Overall'){

  subset_levels <- levels(data[[outcome]]$surv[[subset_variable]])

  # fit a model testing interaction between sbp and subset variable
  fix_interaction <- fix %>%
   str_replace(
    pattern = glue("\\~ {subset_variable} \\+"),
    replacement = "\\~"
   ) %>%
   str_replace(
    pattern = glue(" \\+ {subset_variable}"),
    replacement = ""
   ) %>%
   str_replace(pattern = "\\~ (.+)",
               replacement = glue("\\~ \\(\\1\\) \\* {subset_variable}"))

  functional_forms <- list(
   sbpavg = as.formula(
    glue("~ value(sbpavg) * {subset_variable}")
   )
  )

  lme_formula <- as.formula(
   glue("sbpavg ~ age * {subset_variable} +
        poly(stop,2) * intensive * {subset_variable}")
  )

  fit_lme <- lme(
   fixed = lme_formula,
   data = data_model$long,
   random = as.formula(cov),
   control = nlmeControl(msVerbose = TRUE,
                         msMaxIter = 100,
                         opt = 'nlm')
  )

  fit_surv <- coxph(
   formula =  as.formula(fix_interaction),
   data = data[[outcome]]$surv,
   model = TRUE
  )

  # joint model fit, under the (default) Weibull model
  fit_joint <- jm(Surv_object = fit_surv,
                  Mixed_objects = list(fit_lme),
                  functional_forms = functional_forms,
                  time_var = "stop",
                  id_var = 'pid')

  # This assumes simple two way interaction
  estimates <- fit_joint %>%
   getElement("mcmc") %>%
   getElement("alphas") %>%
   map(~ as_tibble(as.matrix(.x))) %>%
   bind_rows() %>%
   set_names(c("fixed", "interaction")) %>%
   summarize(
    main_est = mean(fixed),
    main_lwr = quantile(fixed, probs = 0.025),
    main_upr = quantile(fixed, probs = 0.975),
    interaction_est = mean(fixed + interaction),
    interaction_lwr = quantile(fixed + interaction, probs = 0.025),
    interaction_upr = quantile(fixed + interaction, probs = 0.975)
   ) %>%
   pivot_longer(everything()) %>%
   # using .subset_variable here to avoid name conflict with subset_variable, which
   # currently exists in global environment.
   separate(name, into = c("subset_level", "type")) %>%
   mutate(
    subset_level = factor(subset_level,
                          levels = c('main', 'interaction'),
                          labels = subset_levels)
   ) %>%
   pivot_wider(values_from = value, names_from = type) %>%
   mutate(subset_variable = subset_variable, .before = subset_level)

  pvalue <-
   summary(fit_joint) %>%
   getElement('Survival') %>%
   as_tibble(rownames = 'term') %>%
   filter(str_detect(term, glue("value\\(sbpavg\\)\\:{subset_variable}"))) %>%
   transmute(subset_variable = subset_variable,
             subset_level = NA_character_,
             pvalue = P)

  output <- list(
   estimate_hr = bind_rows(pvalue, estimates),
   estimate_sbp = jm_pred_long(fit_joint,
                               subset_variable = subset_variable,
                               subset_levels = subset_levels)
  )

 } else {

   # linear mixed model fit

   fit_lme <- lme(
    fixed = sbpavg ~ age + poly(stop,2) * intensive,
    data = data_model$long,
    random = as.formula(cov),
    control = nlmeControl(msVerbose = TRUE,
                          msMaxIter = 100,
                          opt = 'nlm')
   )


   fit_surv <- coxph(
    formula =  as.formula(fix),
    data = data_model$surv,
    model = TRUE
   )

   # joint model fit, under the (default) Weibull model
   fit_joint <- jm(Surv_object = fit_surv,
                   Mixed_objects = list(fit_lme),
                   time_var = "stop",
                   id_var = 'pid')

   estimates <- fit_joint %>%
    getElement("mcmc") %>%
    getElement("alphas") %>%
    map(~ as_tibble(as.matrix(.x))) %>%
    bind_rows() %>%
    set_names(c("fixed")) %>%
    summarize(
     est = mean(fixed),
     lwr = quantile(fixed, probs = 0.025),
     upr = quantile(fixed, probs = 0.975)
    )

   output <- list(
    estimate_hr = estimates,
    estimate_sbp = jm_pred_long(fit_joint) %>%
     mutate(subset_variable = 'Overall')
   )

 }

 if(return_model) output$model <- fit_joint


 output

}


# deprecated:
# data_subset <- map(data[[outcome]], ~split(.x, f = .x[[subset]]))
# subgroups <- unique(na.omit(data[[outcome]]$surv[[subset]]))
#
# fix <- fix %>%
#  str_replace(
#   pattern = glue("\\~ {subset} \\+"),
#   replacement = "\\~"
#  ) %>%
#  str_replace(
#   pattern = glue(" \\+ {subset}"),
#   replacement = ""
#  )
#
# output <- named_list(subgroups)
#
# output$interaction_pvalue <- interaction_pvalue
#
# for(i in subgroups){
#
#  message("Fitting model to participants in ", i, " subgroup")
#
#  data_model <- list(surv = data_subset$surv[[i]],
#                     long = data_subset$long[[i]])
#
#  # linear mixed model fit
#
#  fit_lme <- lme(
#   fixed = sbpavg ~ age + poly(stop,2) * intensive,
#   data = data_model$long,
#   random = as.formula(cov),
#   control = nlmeControl(msVerbose = TRUE,
#                         msMaxIter = 100,
#                         opt = 'nlm')
#  )
#
#
#  fit_surv <- coxph(
#   formula =  as.formula(fix),
#   data = data_model$surv,
#   model = TRUE
#  )
#
#  # joint model fit, under the (default) Weibull model
#  fit_joint <- jm(Surv_object = fit_surv,
#                  Mixed_objects = list(fit_lme),
#                  time_var = "stop",
#                  id_var = 'pid')
#
#  output[[i]] <- fit_joint
#
# }
