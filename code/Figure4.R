################################################################################
#                                                                              #
# The following script creates Figure 4 in Wolfe (2026), Monotonic Growth      #
# Models. The script imports the appropriate models and datasets and exports   #
# Figure 4.                                                                    #
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
us_fit_age <- readRDS("models/us_age.rds")
sa_raw <- read.csv("data/SVAD_SA.csv") ## South Africa Data
sa_raw[sa_raw == -1] <- NA
col_raw <- read.csv("data/SVAD_Colombia.csv") ## Colombia Data
col_raw[col_raw == -1] <- NA
sa_sub<- sa_raw %>% select(agey, FDL_L, man_M1_L) %>% na.omit() # subset age and FDL
col_sub<- col_raw %>% select(agey, FDL_L, man_M1_L) %>% na.omit() # subset age and FDL
sa_mod_mo <- readRDS("models/sa_mo.rds")
col_mod_mo <- readRDS("models/col_mo.rds")
col_fit_age <- readRDS("models/col_age.rds")
sa_fit_age <- readRDS("models/sa_age.rds")

# Helper to extract predictions for one model + sample
get_pred_df <- function(model, data, sample_label) {
  newdat <- data.frame(man_M1_L = sort(unique(data$man_M1_L)))
  
  pred <- predict(model, newdata = newdat, probs = c(0.025, 0.975)) |>
    as.data.frame() |>
    rename(pi_lo = Q2.5, pi_hi = Q97.5)
  
  fit <- fitted(model, newdata = newdat, probs = c(0.025, 0.975)) |>
    as.data.frame() |>
    rename(ci_lo = Q2.5, ci_hi = Q97.5, mu = Estimate)
  
  bind_cols(newdat, fit[, c("mu", "ci_lo", "ci_hi")],
            pred[, c("pi_lo", "pi_hi")]) |>
    mutate(sample = sample_label)
}

# Build combined prediction frame
plot_df <- bind_rows(
  get_pred_df(us_fit_m1_mo, us_sub,  "US"),
  get_pred_df(sa_mod_mo,    sa_sub,  "SA"),
  get_pred_df(col_mod_mo,   col_sub, "COL")
)

# Combined raw data
raw_df <- bind_rows(
  us_sub  |> select(man_M1_L, FDL_L) |> mutate(sample = "US"),
  sa_sub  |> select(man_M1_L, FDL_L) |> mutate(sample = "SA"),
  col_sub |> select(man_M1_L, FDL_L) |> mutate(sample = "COL")
)

# Consistent factor ordering for legend
plot_df$sample <- factor(plot_df$sample, levels = c("US", "SA", "COL"))
raw_df$sample  <- factor(raw_df$sample,  levels = c("US", "SA", "COL"))

# Palette — colorblind-safe, decent contrast
pal <- c("US" = "#1b6ca8", "SA" = "#c1666b", "COL" = "#3e8e41")

fig4b <- ggplot(plot_df, aes(x = man_M1_L, y = mu,
                             color = sample, fill = sample, group = sample)) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi),
              alpha = 0.28, color = NA) +
  # Growth trajectory
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  # Raw data
  geom_jitter(data = raw_df, aes(x = man_M1_L, y = FDL_L, color = sample),
              width = 0.10, height = 0, alpha = 0.35, size = 1.2,
              inherit.aes = FALSE) +
  scale_color_manual(values = pal, name = "Sample") +
  scale_fill_manual(values = pal,  name = "Sample") +
  scale_x_continuous(breaks = sort(unique(raw_df$man_M1_L))) +
  labs(x = "Maxillary M1 stage",
       y = "FDL (mm)") +
  theme_classic(base_size = 12)


# Helper to extract predictions for one age model + sample
get_pred_df_age <- function(model, data, sample_label, n_grid = 200) {
  newdat <- data.frame(
    agey = seq(min(data$agey, na.rm = TRUE),
               max(data$agey, na.rm = TRUE),
               length.out = n_grid)
  )
  
  pred <- predict(model, newdata = newdat, probs = c(0.025, 0.975)) |>
    as.data.frame() |>
    rename(pi_lo = Q2.5, pi_hi = Q97.5)
  
  fit <- fitted(model, newdata = newdat, probs = c(0.025, 0.975)) |>
    as.data.frame() |>
    rename(ci_lo = Q2.5, ci_hi = Q97.5, mu = Estimate)
  
  bind_cols(newdat, fit[, c("mu", "ci_lo", "ci_hi")],
            pred[, c("pi_lo", "pi_hi")]) |>
    mutate(sample = sample_label)
}

# Build combined prediction frame
plot_df <- bind_rows(
  get_pred_df_age(us_fit_age,  us_sub,  "US"),
  get_pred_df_age(sa_fit_age,  sa_sub,  "SA"),
  get_pred_df_age(col_fit_age, col_sub, "COL")
)

# Combined raw data
raw_df <- bind_rows(
  us_sub  |> select(agey, FDL_L) |> mutate(sample = "US"),
  sa_sub  |> select(agey, FDL_L) |> mutate(sample = "SA"),
  col_sub |> select(agey, FDL_L) |> mutate(sample = "COL")
)

# Consistent factor ordering for legend
plot_df$sample <- factor(plot_df$sample, levels = c("US", "SA", "COL"))
raw_df$sample  <- factor(raw_df$sample,  levels = c("US", "SA", "COL"))

# Same palette as the M1 plot for consistency
pal <- c("US" = "#1b6ca8", "SA" = "#c1666b", "COL" = "#3e8e41")

fig4_a <- ggplot(plot_df, aes(x = agey, y = mu,
                              color = sample, fill = sample, group = sample)) +
  # 95% CI on the mean
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi),
              alpha = 0.28, color = NA) +
  # Growth trajectory
  geom_line(linewidth = 0.9) +
  # Raw data
  geom_point(data = raw_df, aes(x = agey, y = FDL_L, color = sample),
             alpha = 0.4, size = 1.2,
             inherit.aes = FALSE) +
  scale_color_manual(values = pal, name = "Sample") +
  scale_fill_manual(values = pal,  name = "Sample") +
  labs(x = "Age (years)",
       y = "FDL (mm)") +
  scale_x_continuous(breaks = seq(0,18,1)) +
  theme_classic(base_size = 12) + theme(legend.position = "none")

(fig4_a | fig4b) + plot_annotation(tag_levels = 'A') + plot_layout(guides = "collect")

ggsave("publication/submission1/figures/Fig4_V1.pdf", device = "pdf", units = "in",
       width = 9, height = 7)

###############################END##############################################
