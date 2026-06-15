################################################################################
#                                                                              #
# The following script creates Figure 1 in Wolfe (2026), Monotonic Growth      #
# Models. The longitudinal model was previously fit in Stan based on the file  #
# file longitudinal.stan. The model was fit based on the Harpenden dataset. T  #
# To access the data, please contact Noel Cameron or associated collegues.     #
#                                                                              #
################################################################################

# Package Load
library(tidyverse)
library(brms)
library(haven)
library(posterior)

# Model & Data Import
long_model <- readRDS("fit_models/longitudinal.RDS") # Fitted model
dat <- read_sav("data/harpenden_spss.sav") # Harpenden Data
dat_sub <- dat %>% select(ID, Gender, Age, WT, HT) %>% 
  filter(Age >=5.00 & Age <= 20.999) %>% na.omit()

# Preece-Baines 1 Growth Model
pb1 <- function(x, h0, h1, s0, s1, theta) {
  h1 - (2 * (h1 - h0)) / (exp(s0 * (x - theta)) + exp(s1 * (x - theta)))
}

# Extract posterior draws
draws <- as_draws_df(long_model$draws())

# Posterior means of population-level parameters
pop <- list(
  h0    = mean(draws$h0),
  h1    = mean(draws$h1),
  s0    = mean(draws$`s[1]`),
  s1    = mean(draws$`s[2]`),
  theta = mean(draws$theta)
)

# Posterior means of subject-level random intercepts
a_id_cols <- grep("^a_ID\\[", colnames(draws), value = TRUE)
a_id_df <- tibble(
  ID   = as.integer(sub("a_ID\\[(\\d+)\\]", "\\1", a_id_cols)),
  a_ID = sapply(a_id_cols, function(col) mean(draws[[col]]))
)

# Build prediction grid 
age_grid <- seq(min(dat_sub$Age), max(dat_sub$Age), length.out = 200)

pop_curve <- tibble(
  age    = age_grid,
  mu_pop = pb1(age, pop$h0, pop$h1, pop$s0, pop$s1, pop$theta)
)

# Cross every individual with the age grid → one curve per subject
indiv_curves <- a_id_df |>
  tidyr::crossing(pop_curve) |>
  mutate(y_pred = mu_pop + a_ID)

# Spaghetti plot
ggplot() +
  # Population mean curve on top
  geom_line(data = pop_curve,
            aes(x = age, y = mu_pop),
            color = "tomato", linewidth = 1.3) +
  # Raw observations connected within each subject
  geom_line(data = dat_sub,
            aes(x = Age, y = HT, group = ID),
            color = "grey40", alpha = 0.2, linewidth = 0.3) +
  labs(x = "Age",
       y = "Stature (cm)") +
  scale_y_continuous(breaks = seq(90,200,10))+
  scale_x_continuous(breaks = seq(5,22,1))+
  theme_classic(base_size = 12)
ggsave("figures/Fig1_V1.pdf", device = "pdf", units = "in",
       width = 5, height = 4)

###############################END##############################################
