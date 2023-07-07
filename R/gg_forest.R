#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param fits_surv_combined
#' @param fits_long_combined
gg_forest <- function(fits_combn,
                      event_stats_combn,
                      sprint_recoder) {

 y_col_0 <- exp(-5.5) # label
 y_col_1 <- exp(-3.2) # event stats
 y_col_2 <- exp(-2.2) # BP levels
 y_col_3 <- exp(-1.2) # hazard ratio viz
 y_col_4 <- exp(1.4)  # hazard ratio text
 y_col_5 <- exp(2.5)  # p-value interaction

 rspec <- round_spec() %>%
  round_using_decimal(digits = 2)

 tx_labels <- c('standard', 'intensive')

 bp_levels <- fits_combn$estimate_sbp %>%
  mutate(intensive = factor(intensive, labels = tx_labels),
         pred_sbpavg = 10 * pred_sbpavg) %>%
  pivot_wider(names_from = intensive, values_from = pred_sbpavg) %>%
  pivot_wider(names_from = stop, values_from = c(intensive, standard))

 gg_data <- fits_combn$estimate_hr %>%
  left_join(bp_levels) %>%
  left_join(event_stats_combn) %>%
  mutate(
   across(
    .cols = c(subset_variable, subset_level),
    .fns = ~recode(.x, !!!sprint_recoder)
   )
  ) %>%
  mutate(
   event_text = if_else(
    is.na(n_event),
    NA_character_,
    table_glue("{n_event} ({100 * n_event / n_participant}%)")
   ),
   .before = est
  ) %>%
  mutate(
    # use abs to get around table_pvalue errors for hr estimates
   pvalue = if_else(is.na(pvalue),
                    NA_character_,
                    table_pvalue(as.numeric(pvalue))),
   hr_est = exp(-est),
   hr_lwr = exp(-upr),
   hr_upr = exp(-lwr),
   hr_ci = if_else(
    is.na(hr_est),
    NA_character_,
    table_glue("{hr_est} ({hr_lwr}, {hr_upr})", rspec = rspec)
   ),
   subset_variable = case_when(
    subset_level == 'Overall' ~ subset_variable,
    is.na(subset_level) ~ subset_variable,
    TRUE ~ paste0("  ", subset_level)
   ),
   intensive = table_glue("{intensive_2.5}, {intensive_5}"),
   standard  = table_glue("{standard_2.5}, {standard_5}"),
   across(.cols = c(intensive, standard),
          .fns = ~ if_else(.x == '--, --', NA_character_, .x))
  ) %>%
  select(-est, -lwr, -upr, -subset_level,
         -starts_with("ci_"),
         -ends_with('5'),
         -starts_with("n_")) %>%
  split(.$outcome) %>%
  map(mutate, x = seq(n(), 1), .before = 1)

 gg_outcome <- recode(names(gg_data),
                      mci = 'mild cognitive impairment',
                      pd = 'probable dementia',
                      pdmci = 'MCI or PD')


 map2(
  .x = gg_data,
  .y = gg_outcome,
  .f = function(.gg_data, .outcome){

   xmax <- max(.gg_data$x)

   y_col_hr <- exp((log(y_col_3) + log(y_col_4)) / 2) + 1/3
   y_col_bp <- exp((log(y_col_2) + log(y_col_3)) / 2)

   hr_label <- paste0("Hazard ratio (95% CI)\n for ", .outcome)

   gg_header <- tribble(
    ~ label, ~ x, ~ y , ~ hjust, ~ fontface,
    "Characteristic", xmax + 2, y_col_0 , 0, "bold",
    "N events (%)", xmax + 2, y_col_1 , 0.5, "bold",
    "SBP at 2.5, 5 years", xmax + 2, y_col_bp, 0.5, "bold",
    "Intensive", xmax + 1, y_col_2 , 0.5, "italic",
    "Standard", xmax + 1, y_col_3 , 0.5, "italic",
    hr_label, xmax + 2.5, y_col_hr, 0.5, "bold",
    "per 10 mm Hg lower SBP", xmax + 1, y_col_hr, 0.5, "italic",
    "P value", xmax + 2, y_col_5 , 1, "bold",
    "interaction", xmax + 1, y_col_5, 1, 'italic'
   )

   gg_rect <- tibble(
    xmin = seq(xmax+1) - 1/2,
    xmax = seq(xmax+1) + 1/2,
    ymin = y_col_0,
    ymax = y_col_5
   ) %>%
    filter(seq(n()) %% 2 == 0)

   gg_arrows_bottom <- tibble(
    x = c(0, 0),
    y = c(exp(-1/4), exp(1/4)),
    yend = c(exp(-.9), exp(.9)),
    label = c("Favors\nlower SBP", "Favors\nhigher SBP")
   )

   size_arrow <- 1 / 5
   size_text <- 4

   fig_bottom <- ggplot(gg_arrows_bottom) +
    aes(x=x, xend=x, y=y, yend=yend, label=label) +
    geom_segment(arrow=arrow(length = unit(size_arrow, 'cm'))) +
    geom_text(size = size_text,
              hjust = c(1,0),
              vjust = 1/2) +
    scale_y_log10(limits = c(y_col_0, y_col_5+0.1),
                  breaks = c(0.5, 1, 2),
                  expand = c(0,0)) +
    coord_flip() +
    theme_void()

   fig_main <- ggplot(.gg_data) +
    aes(x = x) +
    theme_bw() +
    labs(y = 'Hazard ratio') +
    theme(panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.title = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.line = element_blank()) +
    geom_rect(data = gg_rect,
              inherit.aes = FALSE,
              aes(xmin = xmin,
                  xmax = xmax,
                  ymin = ymin,
                  ymax = ymax),
              fill = 'grey',
              alpha = 1/5) +
    scale_y_log10(limits = c(y_col_0, y_col_5+0.1), breaks = c(0.5, 1, 2),
                  expand = c(0,0)) +
    scale_x_continuous(limits = c(1, max(gg_header$x))) +
    coord_flip() +
    geom_text(aes(label = subset_variable, y = y_col_0),
              hjust = 0, size = size_text) +
    geom_text(aes(label = event_text, y = y_col_1),
              hjust = 0.5, size = size_text) +
    geom_text(aes(label = intensive, y = y_col_2),
              hjust = 0.5, size = size_text) +
    geom_text(aes(label = standard, y = y_col_3),
              hjust = 0.5, size = size_text) +
    geom_text(aes(label = hr_ci, y = y_col_4),
              hjust = 0.5, size = size_text) +
    geom_text(aes(label = pvalue, y = y_col_5),
              hjust = 1, size = size_text) +
    geom_text(data = gg_header,
              aes(x = x,
                  y = y,
                  label = label,
                  hjust = hjust,
                  fontface = fontface),
              size = size_text) +
    geom_segment(y = 0, yend = 0,
                 x = 0, xend = xmax + 1/3,
                 color = 'grey',
                 alpha = 0.5,
                 linetype = 2) +
    geom_segment(y = log(0.5)/2, yend = log(2)/2,
                 x = 0, xend = 0) +
    geom_point(aes(y = hr_est), shape = 15, size = 3) +
    geom_linerange(aes(ymin = hr_lwr, ymax = hr_upr))

   cowplot::plot_grid(fig_main,
                      fig_bottom,
                      align = 'v',
                      nrow=2, rel_heights = c(14, 1))

  }
 )

}
