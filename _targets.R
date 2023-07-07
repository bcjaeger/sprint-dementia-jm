## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
library(future)
library(future.callr)
plan(callr)


model_specs <- expand_grid(
 outcome = c('pd', 'mci', 'pdmci'),
 fix = c("Surv(stop, status) ~ education + sex + race + age_cat"),
 cov = c("~ 1 + poly(stop, 2) | pid"),
 subset = c("Overall",
            "age_cat",
            "sex",
            "race",
            "education")
) %>%
 mutate(label = glue("{outcome}_{subset}"))


tar_plan(

 sprint = list(
  pd = sprint_pdmci_load(status_variable_name = 'pd'),
  mci = sprint_pdmci_load(status_variable_name = 'mci'),
  pdmci = sprint_pdmci_load(status_variable_name = 'pdmci')
 ),

 sprint_recoder = recoder_make(),

 event_stats <- tar_map(
  values = distinct(model_specs, subset),
  tar_target(
   event_stats,
   sprint_summarize_events(sprint, .group_by = subset)
  )
 ),

 tar_combine(event_stats_combn, event_stats),

 fits <- tar_map(
  values = model_specs,
  names = label,
  tar_target(
   fit,
   jm_fit(data = sprint,
          outcome = outcome,
          fix = fix,
          cov = cov,
          subset = subset),
   memory = "transient",
   garbage_collection = TRUE
  )
 ),

 tar_combine(fits_combn,
             fits[[1]],
             command = bind_fits(!!!.x)),

 fig_forest = gg_forest(fits_combn, event_stats_combn, sprint_recoder)

 # fits_sbp_subgroups = as_forestplot_data(fits_smry),

 #
 # fits_ft_data = as_ft_data(fits_smry),
 #
 # tar_render(abstract, "doc/abstract.Rmd")

) |>
 tar_hook_before(
  hook = source("conflicts.R"),
  names = everything()
 )



