





ggplot(drop_na(data_long)) +
 aes(x = time, y = sbpavg, col = factor(intensive)) +
 geom_smooth(method = 'lm',
             formula = y ~ bs(x, df = 4))

data_events <- data_long %>%
 group_by(pid) %>%
 filter(sum(status) == 1)

data_nope <- data_long %>%
 group_by(pid) %>%
 filter(sum(status) == 0)

ggplot(bind_rows(events = data_events,
                 nope = data_nope,
                 .id = 'type')) +
 aes(x = time, y = sbpavg, color = type) +
 geom_smooth() +
 facet_wrap(~intensive)

ggplot(data_long) +
 aes(x = time, y = sbpavg) +
 geom_line(aes(group = pid)) +
 facet_wrap(~intensive) +
 geom_smooth()

# linear mixed model fit
fit_lme <- lme(
 sbpavg ~ age + time * intensive,
 random = ~ 1 + time | pid,
 data = data_long, control = list(opt = 'optim')
)

anova(fit_lme)

# interval censoring
fit_surv <- survreg(
 formula =  Surv(t1, t2, type = "interval2") ~ edu_4cat + race4,
 data = data_surv,
 model = TRUE
)

fit_surv <- coxph(
 formula =  Surv(stop, status) ~ edu_4cat + race4 + age,
 data = data_surv,
 model = TRUE
)

functional_forms <- list(sbpavg = ~ slope(sbpavg) + area(sbpavg))

# joint model fit, under the (default) Weibull model
fit_joint <- jm(Surv_object = fit_surv,
                Mixed_objects = list(fit_lme),
                time_var = "time",
                id_var = 'pid')

summary(fit_joint)

new_data <- data_long %>%
 group_by(pid) %>%
 # filter(sum(status) > 0) %>%
 ungroup() %>%
 filter(pid %in% unique(.$pid)[105]) %>%  # c(10004629)) %>%
 mutate(race4 = factor(race4, levels = unique(data_surv$race4)))

new_data

for(i in seq(nrow(new_data))){


 surv_probs <- survfitJM(object = fit_joint,
                         newdata = new_data[seq(i),],
                         idVar = 'pid')

 plot(
  surv_probs,
  split = c(1,1),
  surv_in_all = TRUE,
  lty_lines_CI = 3,
  col_lines = "blue",
  col_fill_CI = "pink2",
  main = ifelse(any(new_data$status == 1), 'event', 'no event'),
  ylab = c("SBP, mm Hg"),
  col_points = "red",
  pch_points = 16,
  cex_xlab = 0.8,
  cex_ylab = 0.8,
  cex_zlab = 0.8,
  cex_main = 0.8,
  cex_axis = 0.7
 )


}


Formulas <-
 list(
  "sbpavg" = "value",
  "sbpavg" = list(
   fixed = ~ 1,
   random = ~ 1,
   indFixed = 2,
   indRandom = 2,
   name = "area"
  )
 )

fit_joint_functional <- update(fit_joint, Formulas = Formulas)

summary(fit_joint_functional)

