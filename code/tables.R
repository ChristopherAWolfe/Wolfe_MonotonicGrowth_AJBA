################################################################################
#                                                                              #
# The following script creates all data derived tables (Tables 2 -5) in Wolfe  # 
# (2026), Monotonic Growth. Note, this code provides the necessary results     #
# that were modified in Excel downstream for publication.                      #
#                                                                              #
################################################################################

# Package Load
library(tidyverse)
library(posterior)
library(brms)
library(loo)

# Import Data and Models
us_raw <- read.csv("data/SVAD_US.csv") ## US Data
us_raw[us_raw == -1] <- NA
us_sub<- us_raw %>% select(SVAD_identifier,agey, FDL_L, man_M1_L) %>% na.omit() # subset age and FDL
us_fit_m1_mo <- readRDS("models/us_mo.rds")
us_fit_age <- readRDS("models/us_age.rds")
us_fit_m1_int <- readRDS("models/us_int.rds")
us_fit_m1_factor <- readRDS("models/us_factor.rds")
us_fit_m1_order <- readRDS("models/us_order.rds")

# TABLE 2

## Model Validation and Comparison

loo_age <- loo(us_fit_age)
loo_int <- loo(us_fit_m1_int)
loo_fact <- loo(us_fit_m1_factor)
loo_order <- loo(us_fit_m1_order)
loo_mo <- loo(us_fit_m1_mo)

comp <- loo_compare(loo_age, loo_int, loo_fact, loo_order, loo_mo)
write.csv(comp, file = "publication/submission1/table2_V1.csv")

# TABLE 3
## Extract only the regression coefficients for the publication table.
summary(us_fit_m1_mo)

# TABLE 4 & TABLE 5
## Note - all values are based on the full list of posterior draws. 

model <- us_fit_m1_mo
probs <- c(0.025, 0.975) # change for a different credible level

### Pull all draws
dr    <- as_draws_df(model)
b_int <- dr[["b_Intercept"]]            # FDL at lowest category (mm)
b_m   <- dr[["bsp_moman_M1_L"]]           # avg adjacent-cat diff, FDL (mm)
b_sint<- dr[["b_sigma_Intercept"]]      # log residual SD at lowest category
b_s   <- dr[["bsp_sigma_moman_M1_L"]]     # avg adjacent-cat diff, log-sigma

### simplex draws as draws x D matrices
ord_cols <- function(pattern) {
  cols <- grep(pattern, names(dr), value = TRUE)
  cols[order(as.integer(sub(".*\\[(\\d+)\\]$", "\\1", cols)))]
}
Zm <- as.matrix(dr[, ord_cols("^simo_moman_M1_L1\\[")])        # S x D
Zs <- as.matrix(dr[, ord_cols("^simo_sigma_moman_M1_L1\\[")])  # S x D
D  <- ncol(Zm)

### Reconstruct quantities inside each draw
cum_m  <- t(apply(Zm, 1, cumsum))       # S x D  cumulative shares
cum_s  <- t(apply(Zs, 1, cumsum))
totM   <- b_m * D                        # full-range FDL change per draw
totS   <- b_s * D                        # full-range log-sigma change per draw

### per-category (0..D): cols 1..(D+1) correspond to categories 0..D
FDLcat <- cbind(b_int,  b_int  + totM * cum_m)              # S x (D+1), mm
SDcat  <- exp(cbind(b_sint, b_sint + totS * cum_s))         # S x (D+1), mm

### per-step (1..D)
shareM <- Zm * 100                                           # % of total effect
deltaF <- totM * Zm                                          # Delta FDL (mm)
pctF   <- (FDLcat[, -1, drop = FALSE] - FDLcat[, -(D + 1), drop = FALSE]) /
  FDLcat[, -(D + 1), drop = FALSE] * 100        # % change vs prev cat
facS   <- exp(totS * Zs)                                     # SD multiplier
pctSD  <- (facS - 1) * 100                                   # % change in SD

### Summarize across draws
### returns est (posterior mean) + lower/upper credible bounds per column
qs <- function(M) {
  M  <- as.matrix(M)
  qq <- t(apply(M, 2L, quantile, probs = probs, names = FALSE))
  list(est = colMeans(M), lo = qq[, 1], hi = qq[, 2])
}
pad <- function(v) c(NA, v)              # NA in the category-0 row

s_share <- qs(shareM); s_dF <- qs(deltaF); s_FDL <- qs(FDLcat)
s_pctF  <- qs(pctF)
s_fac   <- qs(facS); s_pSD <- qs(pctSD); s_SD <- qs(SDcat)

mean_tbl <- data.frame(
  Step_i           = 0:D,
  share_pct_est    = pad(s_share$est), share_pct_lo95 = pad(s_share$lo), share_pct_hi95 = pad(s_share$hi),
  dFDL_mm_est      = pad(s_dF$est),    dFDL_mm_lo95   = pad(s_dF$lo),    dFDL_mm_hi95   = pad(s_dF$hi),
  FDL_mm_est       = s_FDL$est,        FDL_mm_lo95    = s_FDL$lo,        FDL_mm_hi95    = s_FDL$hi,
  pct_change_est   = pad(s_pctF$est),  pct_change_lo95= pad(s_pctF$lo),  pct_change_hi95= pad(s_pctF$hi)
)

sigma_tbl <- data.frame(
  Step_i           = 0:D,
  zeta_est         = pad(qs(Zs)$est),  zeta_lo95 = pad(qs(Zs)$lo), zeta_hi95 = pad(qs(Zs)$hi),
  factor_est       = pad(s_fac$est),   factor_lo95 = pad(s_fac$lo), factor_hi95 = pad(s_fac$hi),
  pct_change_SD_est= pad(s_pSD$est),   pct_change_SD_lo95 = pad(s_pSD$lo), pct_change_SD_hi95 = pad(s_pSD$hi),
  SD_mm_est        = s_SD$est,         SD_mm_lo95 = s_SD$lo,        SD_mm_hi95 = s_SD$hi
)

## Table 4
write.csv(mean_tbl,  "mean_simplex_FDL_CI.csv", row.names = FALSE)

## Table 5
write.csv(sigma_tbl, "sigma_simplex_CI.csv",    row.names = FALSE)

###############################END##############################################
