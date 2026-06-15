################################################################################
#                                                                              #
# The following script creates Figure 5 in Wolfe (2026), Monotonic Growth      #
# Models. The script imports the appropriate models and datasets and exports   #
# Figure 5. Note, all associated tables can be found as supporting information.#
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

us  <- us_sub  |> mutate(sample = "US")
sa  <- sa_sub  |> mutate(sample = "SA")
col <- col_sub |> mutate(sample = "COL")

# Stage: Model-based mu and sigma at each stage 
stage_grid <- data.frame(man_M1_L = sort(unique(us$man_M1_L)))

mu_stage    <- fitted(us_fit_m1_mo, newdata = stage_grid)[, "Estimate"]
sigma_stage <- fitted(us_fit_m1_mo, newdata = stage_grid, dpar = "sigma")[, "Estimate"]

ref_stage <- data.frame(
  man_M1_L = stage_grid$man_M1_L,
  mu_stage = mu_stage,
  sigma_stage = sigma_stage
)

# AGE: model-based mu(age) and sigma(age) at each observation 
add_age_ref <- function(df) {
  mu    <- fitted(us_fit_age, newdata = df)[, "Estimate"]
  sigma <- fitted(us_fit_age, newdata = df, dpar = "sigma")[, "Estimate"]
  df |> mutate(mu_age = mu, sigma_age = sigma)
}

# Compute z-scores for SA + COL
score <- function(df) {
  df |>
    left_join(ref_stage, by = "man_M1_L") |>
    add_age_ref() |>
    mutate(z_stage = (FDL_L - mu_stage) / sigma_stage,
           z_age   = (FDL_L - mu_age)   / sigma_age)
}

sa_z  <- score(sa)
col_z <- score(col)
zs    <- bind_rows(sa_z, col_z)

# Reference at integer ages (for the Ref_age table)
ref_age <- add_age_ref(data.frame(agey = 0:16)) |>
  rename(age_yrs = agey, mu_model = mu_age, sigma_model = sigma_age)

SA_COLOR  <- "#c1666b"
COL_COLOR <- "#3e8e41"
GREY_REF  <- "#9aa0a6"
pal       <- c("SA" = SA_COLOR, "COL" = COL_COLOR)

# Panel A: z by stage
p_stage <- ggplot(zs, aes(x = factor(man_M1_L), y = z_stage, color = sample)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -2, ymax = 2,
           fill = GREY_REF, alpha = 0.08) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -1, ymax = 1,
           fill = GREY_REF, alpha = 0.10) +
  geom_hline(yintercept = 0,  color = GREY_REF, linewidth = 0.4) +
  geom_hline(yintercept = c(-2, 2), color = GREY_REF,
             linewidth = 0.3, linetype = "dashed") +
  geom_point(position = position_jitterdodge(jitter.width = 0.15,
                                             dodge.width = 0.45),
             size = 2.4, alpha = 0.85,
             stroke = 0.6, shape = 21,
             aes(fill = sample), color = "white") +
  scale_fill_manual(values = pal, name = "Sample") +
  scale_color_manual(values = pal, name = "Sample") +
  coord_cartesian(ylim = c(-5, 5)) +
  labs(x = "Mandibular M1 stage",
       y = NULL,
       title = "B. z-scores by dentition stage") +
  theme_classic(base_size = 11) +   theme(legend.position = "bottom")

# Panel B: z by continuous age
p_age <- ggplot(zs, aes(x = agey, y = z_age, fill = sample)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -2, ymax = 2,
           fill = GREY_REF, alpha = 0.08) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -1, ymax = 1,
           fill = GREY_REF, alpha = 0.10) +
  geom_hline(yintercept = 0,  color = GREY_REF, linewidth = 0.4) +
  geom_hline(yintercept = c(-2, 2), color = GREY_REF,
             linewidth = 0.3, linetype = "dashed") +
  geom_point(size = 2.4, alpha = 0.85,
             stroke = 0.6, shape = 21, color = "white") +
  scale_fill_manual(values = pal, name = "Sample") +
  coord_cartesian(ylim = c(-5, 5)) +
  labs(x = "Age (years)", y = "FDL z-score (model-based US reference)",
       title = "A. z-scores by chronological age") +
  theme_classic(base_size = 11) +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(0,18,1))

fig <- (p_age | p_stage) + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("Fig5_V1.pdf", fig, width = 9, height = 7, device = "pdf", units = "in")

## SUPPORTING INFORMATION

library(openxlsx)

# Build the same workbook structure
wb <- createWorkbook()

# Empirical comparison table for stage ref
ref_stage_full <- us |>
  group_by(man_M1_L) |>
  summarise(n_US = n(),
            emp_mean = mean(FDL_L),
            emp_sd   = sd(FDL_L), .groups = "drop") |>
  left_join(ref_stage, by = "man_M1_L") |>
  rename(`M1 stage` = man_M1_L,
         `Empirical mean` = emp_mean, `Empirical SD` = emp_sd,
         `Model μ` = mu_stage, `Model σ` = sigma_stage)

# Per-individual table
ind <- zs |>
  select(Sample = sample, `Age (yrs)` = agey, `M1 stage` = man_M1_L,
         FDL = FDL_L,
         `μ_stage` = mu_stage, `σ_stage` = sigma_stage,
         `z (by stage)` = z_stage,
         `μ_age` = mu_age, `σ_age` = sigma_age,
         `z (by age)` = z_age) |>
  arrange(Sample, `M1 stage`, `Age (yrs)`)

addWorksheet(wb, "Ref_stage");    writeData(wb, "Ref_stage",    ref_stage_full)
addWorksheet(wb, "Ref_age");      writeData(wb, "Ref_age",      ref_age)
addWorksheet(wb, "Z_individual"); writeData(wb, "Z_individual", ind)

# Summary z per sample × stage
sum_stage <- zs |>
  group_by(sample, man_M1_L) |>
  summarise(n = n(), mean_z = mean(z_stage), sd_z = sd(z_stage), .groups = "drop")
addWorksheet(wb, "Summary_stage"); writeData(wb, "Summary_stage", sum_stage)

# Summary z per sample × age bin
age_breaks <- c(0, 1, 2, 4, 6, 9, 12, 16)
age_labels <- c("<1","1–2","2–4","4–6","6–9","9–12","12–16")
sum_age <- zs |>
  mutate(age_bin = cut(agey, breaks = age_breaks, labels = age_labels,
                       right = FALSE, include.lowest = TRUE)) |>
  group_by(sample, age_bin) |>
  summarise(n = n(), mean_z = mean(z_age), sd_z = sd(z_age), .groups = "drop")
addWorksheet(wb, "Summary_age"); writeData(wb, "Summary_age", sum_age)

saveWorkbook(wb, "FDL_zscores_model.xlsx", overwrite = TRUE)

###############################END##############################################
