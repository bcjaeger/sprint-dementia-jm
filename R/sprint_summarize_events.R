

sprint_summarize_events <- function(sprint, .group_by){

 surv_data <- map(sprint, 'surv')

 if(.group_by != 'Overall'){

  surv_data <- map(surv_data, ~split(.x, .x[[.group_by]]))

 } else {

  surv_data <- list(pd = list(Overall = surv_data$pd),
                    mci = list(Overall = surv_data$mci),
                    pdmci = list(Overall = surv_data$pdmci))
 }

 map_dfr(
  surv_data,
  .f = function(subset_variable){
   map_dfr(
    subset_variable,
    .f = function(subset_level){
     summarize(ungroup(subset_level),
               n_event = sum(status),
               n_participant = n(),
               n_years = sum(stop))
    },
    .id = 'subset_level'
   )
  },
  .id = 'outcome'
 ) %>%
  mutate(subset_variable = .group_by, .before = subset_level)

}

