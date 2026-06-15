################################################################################
#                                                                              #
# The following script creates Figure 6 in Wolfe (2026), Monotonic Growth      #
# Models. The script imports the appropriate models and datasets and exports   #
# Figure 6. Note, to estimate age, previous age estimation was completed using #
# Man M1 based on the SVAD_US data and protocols at:                           #
#  https://rpubs.com/elainechu/mcp_vignette.                                   #
#                                                                              #
################################################################################

# Package Load
library(tidyverse)
library(posterior)
library(patchwork)
library(cmdstanr)

# Import Data and Models
us_raw <- read.csv("data/SVAD_US.csv") ## US Data
us_raw[us_raw == -1] <- NA
us_sub<- us_raw %>% select(SVAD_identifier,agey, FDL_L, man_M1_L) %>% na.omit() # subset age and FDL

## MCP Age Estimation Results from https://rpubs.com/elainechu/mcp_vignette
estim <- read.csv("data/man_m1_US_test_predictions.csv")
estim <- estim %>% select(SVAD_identifier, xmean, lower95, upper95)
joined_data <- left_join(us_sub, estim, by = "SVAD_identifier")

### The model requires an estimate of the SE - quantfied here. 
dat_est <- joined_data |>
  mutate(sdx = (upper95 - lower95) / (2 * 1.96))

# Error in variables model accounting for uncertainty on age
mod <- cmdstan_model("errors_in_variables_growth.stan")

## Stan Data
stan_data <- list(
  N     = nrow(dat_est),
  y     = dat_est$FDL_L,
  xmean = dat_est$xmean,
  sdx   = dat_est$sdx
)

## cmdstanr fit
est_fit_age <- mod$sample(
  data = stan_data,
  chains = 4, parallel_chains = 4,
  iter_warmup = 1500, iter_sampling = 1500,
  adapt_delta = 0.99, max_treedepth = 12
)
est_fit_age$save_object("age_error.rds")

# Prep Plot
draws <- as_draws_df(est_fit_age$draws())

age_grid <- seq(min(dat_est$xmean), max(dat_est$xmean), length.out = 200)

# Build a (draw × age) grid of mu and sigma
post <- draws |>
  as_tibble() |>
  select(a, r, b, g0, g1) |>
  mutate(.draw = row_number()) |>
  tidyr::crossing(agey = age_grid) |>
  mutate(
    mu    = a * agey^r + b,
    sigma = exp(g0 + g1 * agey),
    y_rep = rnorm(n(), mu, sigma)   # posterior predictive draw
  )

curve_est <- post |>
  group_by(agey) |>
  summarise(
    mu_mean = mean(mu),
    ci_lo   = quantile(mu,    0.025),
    ci_hi   = quantile(mu,    0.975),
    pi_lo   = quantile(y_rep, 0.025),
    pi_hi   = quantile(y_rep, 0.975),
    .groups = "drop"
  ) |>
  rename(mu = mu_mean) |>
  mutate(model = "Estimated age (errors-in-variables)")

nd_known   <- data.frame(agey = age_grid)
fit_known  <- fitted (us_fit_age, newdata = nd_known, probs = c(0.025, 0.975)) |>
  as.data.frame() |> rename(mu = Estimate, ci_lo = Q2.5, ci_hi = Q97.5)
pred_known <- predict(us_fit_age, newdata = nd_known, probs = c(0.025, 0.975)) |>
  as.data.frame() |> rename(pi_lo = Q2.5, pi_hi = Q97.5)
curve_known <- bind_cols(nd_known,
                         fit_known[, c("mu","ci_lo","ci_hi")],
                         pred_known[, c("pi_lo","pi_hi")]) |>
  mutate(model = "Known age (US reference)")

curves <- bind_rows(curve_known, curve_est)

pal <- c("Known age (US reference)" = "#1b6ca8",
         "Estimated age (errors-in-variables)" = "#c1666b")

fig6b <- ggplot() +
  # Prediction intervals (light) and CI on mean (dark) for both models
  geom_ribbon(data = curves,
              aes(x = agey, ymin = pi_lo, ymax = pi_hi, fill = model),
              alpha = 0.10, color = NA) +
  geom_ribbon(data = curves,
              aes(x = agey, ymin = ci_lo, ymax = ci_hi, fill = model),
              alpha = 0.30, color = NA) +
  geom_line(data = curves,
            aes(x = agey, y = mu, color = model), linewidth = 0.9) +
  # Known-age raw data — just points
  geom_point(data = us_sub,
             aes(x = agey, y = FDL_L),
             color = "#1b6ca8", alpha = 0.35, size = 1.2,
             inherit.aes = FALSE) +
  # Estimated-age raw data — horizontal error bars for age CI + point at xmean
  geom_errorbarh(data = dat_est,
                 aes(xmin = lower95, xmax = upper95, y = FDL_L),
                 color = "#c1666b", alpha = 0.45, height = 0, linewidth = 0.4) +
  geom_point(data = dat_est,
             aes(x = xmean, y = FDL_L),
             color = "#c1666b", alpha = 0.85, size = 1.6) +
  scale_color_manual(values = pal, name = NULL) +
  scale_fill_manual( values = pal, name = NULL) +
  labs(x = "Age (years)",
       y = "FDL (mm)") +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom") + scale_x_continuous(breaks = seq(0,24,1))

fig6a <- ggplot() +
  # Prediction intervals (light) and CI on mean (dark) for both models
  geom_ribbon(data = curves,
              aes(x = agey, ymin = pi_lo, ymax = pi_hi, fill = model),
              alpha = 0.10, color = NA) +
  geom_ribbon(data = curves,
              aes(x = agey, ymin = ci_lo, ymax = ci_hi, fill = model),
              alpha = 0.30, color = NA) +
  geom_line(data = curves,
            aes(x = agey, y = mu, color = model), linewidth = 0.9) +
  scale_color_manual(values = pal, name = NULL) +
  scale_fill_manual( values = pal, name = NULL) +
  labs(x = "Age (years)",
       y = "FDL (mm)") +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom") + scale_x_continuous(breaks = seq(0,18,1))

fig6 <- (fig6a| fig6b) + plot_annotation(tag_levels = 'A') + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("Fig6_V1.pdf", fig6, width = 9, height = 7, device = "pdf", units = "in")

###############################END##############################################
