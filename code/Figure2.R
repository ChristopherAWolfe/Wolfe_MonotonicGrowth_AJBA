################################################################################
#                                                                              #
# The following script creates Figure 2 in Wolfe (2026), Monotonic Growth      #
# Models. The script imports the chronological age model and the US dataset.   #
# The script exports Figure 2.                                                 #
#                                                                              #
################################################################################

# Package Load
library(tidyverse)
library(brms)
library(posterior)

# Import Data and Model
us_raw <- read.csv("data/SVAD_US.csv") ## US Data
us_raw[us_raw == -1] <- NA
us_sub<- us_raw %>% select(SVAD_identifier,agey, FDL_L, man_M1_L) %>% na.omit() # subset age and FDL
us_fit_age <- readRDS("models/us_age.rds")


# Dense grid across observed age range
newdat <- data.frame(
  agey = seq(min(us_sub$agey, na.rm = TRUE),
             max(us_sub$agey, na.rm = TRUE),
             length.out = 200)
)

# Prediction intervals
pred <- predict(us_fit_age, newdata = newdat, probs = c(0.025, 0.975)) |>
  as.data.frame() |>
  rename(pi_lo = Q2.5, pi_hi = Q97.5)

# CI intervals
fit <- fitted(us_fit_age, newdata = newdat, probs = c(0.025, 0.975)) |>
  as.data.frame() |>
  rename(ci_lo = Q2.5, ci_hi = Q97.5, mu = Estimate)

plot_df <- bind_cols(newdat, fit[, c("mu", "ci_lo", "ci_hi")],
                     pred[, c("pi_lo", "pi_hi")])
# Plot
ggplot(plot_df, aes(x = agey, y = mu)) +
  # 95% prediction interval — fans out with sigma
  geom_ribbon(aes(ymin = pi_lo, ymax = pi_hi),
              fill = "steelblue", alpha = 0.15) +
  # 95% CI on the mean
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi),
              fill = "steelblue", alpha = 0.35) +
  # Growth trajectory
  geom_line(color = "steelblue4", linewidth = 0.9) +
  # Raw data
  geom_point(data = us_sub, aes(x = agey, y = FDL_L),
             alpha = 0.5, size = 1.4, color = "grey25",
             inherit.aes = FALSE) +
  labs(x = "Age (years)",
       y = "FDL (mm)")+
  scale_x_continuous(breaks = seq(0,16,1)) +
  theme_classic(base_size = 12)
ggsave("publication/submission1/figures/Fig2_V1.pdf", device = "pdf", units = "in",
       width = 5, height = 4)

###############################END##############################################
