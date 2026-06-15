################################################################################
#                                                                              #
# The following script fits and saves all Bayesian models associated with the  #
# Wolfe (2026), Monotonic Growth Models. All data is available open access at  #
# the SVAD Zenodo Repository. Models are needed for Figure 2-6 and Table 2-5.  #
#                                                                              #
################################################################################

# Package Load
library(tidyverse)
library(brms)

# Initial Data Import + Prep
## Additional data preparation may be completed below during model fitting
us_raw <- read.csv("data/SVAD_US.csv") ## US Data
us_raw[us_raw == -1] <- NA

sa_raw <- read.csv("data/SVAD_SA.csv") ## South Africa Data
sa_raw[sa_raw == -1] <- NA

col_raw <- read.csv("data/SVAD_Colombia.csv") ## Colombia Data
col_raw[col_raw == -1] <- NA

# Bayesian Growth Modeling using brms

## United States [Necessary for Figures 2 - 6 and Tables 2-5]

### Chronological Age x Length
us_sub<- us_raw %>% select(SVAD_identifier,agey, FDL_L, man_M1_L) %>% na.omit() # subset age and FDL

prior_us_age <- prior(normal(65, 1), nlpar = "a") + 
  prior(normal(0, 1), nlpar = "r") + prior(normal(65, 1), nlpar = "b") # Priors set on offset power law

us_fit_age <- brm(bf(FDL_L ~ a*agey^r + b, sigma ~ agey,a + r + b ~ 1, nl = TRUE),
                  data = us_sub, prior = prior_us_age, backend = "cmdstanr")
saveRDS(us_fit_age, file = "models/us_age.rds")

### Mandibular M1 x Length - Integer
us_fit_m1_int <- brm(bf(FDL_L ~ man_M1_L, sigma ~ man_M1_L), data = us_sub, 
                     backend = "cmdstanr")
saveRDS(us_fit_m1_int, file = "models/us_int.rds")

### Mandibular M1 x Length - Factor (unordered category)
us_sub2 <- us_sub %>% mutate(m1_fact = as.factor(man_M1_L), m1_ordered = as.ordered(man_M1_L))

us_fit_m1_factor <- brm(bf(FDL_L ~ m1_fact, sigma ~ m1_fact), data = us_sub2, 
                        backend = "cmdstanr")
saveRDS(us_fit_m1_factor, file = "models/us_factor.rds")

### Mandibular M1 x Length - Factor (ordered category)
us_fit_m1_order <- brm(bf(FDL_L ~ m1_ordered, sigma ~ m1_ordered), data = us_sub2, 
                       backend = "cmdstanr")
saveRDS(us_fit_m1_order, file = "models/us_order.rds")

### Mandibular M1 x Length - Monotonic Effects
us_fit_m1_mo <- brm(bf(FDL_L ~ mo(man_M1_L), sigma ~ mo(man_M1_L)), data = us_sub, 
                    backend = "cmdstanr")
saveRDS(us_fit_m1_mo, file = "models/us_mo.rds")

## South Africa [Necessary for Figures 4-5]

###  Chronological Age x Length
sa_sub<- sa_raw %>% select(agey, FDL_L, man_M1_L) %>% na.omit() # subset age and FDL
sa_fit_age <- brm(bf(FDL_L ~ a*agey^r + b, sigma ~ agey,a + r + b ~ 1, nl = TRUE),
                  data = sa_sub, prior = prior_us_age, backend = "cmdstanr")
saveRDS(sa_fit_age, file = "models/sa_age.rds")

### Mandibular M1 x Length - Monotonic Effects
sa_mod_mo <- brm(
  bf(FDL_L ~ mo(man_M1_L), sigma ~ mo(man_M1_L)), data = sa_sub, 
  backend = "cmdstanr", adapt_delta = 0.99
)
saveRDS(sa_mod_mo, file = "models/sa_mo.rds")

## Colombia [Necessary for Figures 4-5]

### Chronological Age x Length
col_sub<- col_raw %>% select(agey, FDL_L, man_M1_L) %>% na.omit() # subset age and FDL

col_fit_age <- brm(bf(FDL_L ~ a*agey^r + b, sigma ~ agey,a + r + b ~ 1, nl = TRUE),
                   data = col_sub, prior = prior_us_age, backend = "cmdstanr")
saveRDS(col_fit_age, file = "models/col_age.rds")

### Mandibular M1 x Length - Monotonic Effects
col_mod_mo <- brm(bf(FDL_L ~ mo(man_M1_L), sigma ~ mo(man_M1_L)), data = col_sub, 
                  backend = "cmdstanr", adapt_delta = 0.99, max_treedepth = 12)
saveRDS(col_mod_mo, file = "models/col_mo.rds")

###############################END##############################################
