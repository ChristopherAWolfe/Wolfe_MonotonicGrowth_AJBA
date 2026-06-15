################################################################################
#                                                                              #
# The following script creates Figure 3 in Wolfe (2026), Monotonic Growth      #
# Models. The script imports the appropriate model and datasets and exports    #
# Figure 3.                                                                    #
#                                                                              #
################################################################################

# Package Load
library(tidyverse)
library(brms)
library(posterior)
library(patchwork)

# Import Data and Models
us_raw <- read.csv("data/SVAD_US.csv") ## US Data
us_raw[us_raw == -1] <- NA
us_sub<- us_raw %>% select(SVAD_identifier,agey, FDL_L, man_M1_L) %>% na.omit() # subset age and FDL
us_fit_m1_mo <- readRDS("models/us_mo.rds")

# Grid of ordinal levels
newdat <- data.frame(man_M1_L = sort(unique(us_sub$man_M1_L)))

# Prediction intervals
pred <- predict(us_fit_m1_mo, newdata = newdat, probs = c(0.025, 0.975)) |>
  as.data.frame() |>
  rename(pi_lo = Q2.5, pi_hi = Q97.5)

# Credible intervals
fit <- fitted(us_fit_m1_mo, newdata = newdat, probs = c(0.025, 0.975)) |>
  as.data.frame() |>
  rename(ci_lo = Q2.5, ci_hi = Q97.5, mu = Estimate)

plot_df <- bind_cols(newdat, fit[, c("mu", "ci_lo", "ci_hi")],
                     pred[, c("pi_lo", "pi_hi")])

fig3a <- ggplot(plot_df, aes(x = man_M1_L, y = mu)) +
  # 95% PI as thin outer whisker
  geom_linerange(aes(ymin = pi_lo, ymax = pi_hi),
                 color = "steelblue", linewidth = 0.6) +
  # 95% CI as thicker inner bar
  geom_linerange(aes(ymin = ci_lo, ymax = ci_hi),
                 color = "steelblue4", linewidth = 2) +
  # Mean as point
  geom_point(color = "steelblue4", size = 2.5) +
  # Raw data
  geom_jitter(data = us_sub, aes(x = man_M1_L, y = FDL_L),
              width = 0.08, height = 0, alpha = 0.5,
              size = 0.5, color = "grey25",
              inherit.aes = FALSE) +
  scale_x_continuous(breaks = sort(unique(us_sub$man_M1_L))) +
  labs(x = "Mandibular M1 stage",
       y = "FDL (mm)") +
  theme_classic(base_size = 12)

#### 3B
fig3b <- ggplot(plot_df, aes(x = man_M1_L, y = mu)) +
  # 95% prediction interval — fans out with sigma
  geom_ribbon(aes(ymin = pi_lo, ymax = pi_hi),
              fill = "steelblue", alpha = 0.15) +
  # 95% CI on the mean
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi),
              fill = "steelblue", alpha = 0.35) +
  # Growth trajectory
  geom_line(color = "steelblue4", linewidth = 0.9) +
  geom_point(color = "steelblue4", size = 2.2) +
  # Raw data
  geom_jitter(data = us_sub, aes(x = man_M1_L, y = FDL_L),
              width = 0.12, height = 0, alpha = 0.5,
              size = 1.4, color = "grey25",
              inherit.aes = FALSE) +
  scale_x_continuous(breaks = sort(unique(us_sub$man_M1_L))) +
  labs(x = "Mandibular M1 stage",
       y = "FDL (mm)") +   # adjust units
  theme_classic(base_size = 12)

(fig3a | fig3b) + plot_annotation(tag_levels = 'A') + plot_layout(guides = "collect")

ggsave("publication/submission1/figures/Fig3_V1.pdf", device = "pdf", units = "in",
       width = 9, height = 7)

###############################END##############################################
