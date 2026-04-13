source(here::here("codes", "00_config.R"))
library(data.table)
library(fixest)
library(modelsummary)

# ── Load ─────────────────────────────────────────────────────────────────────
panel <- fread(file.path(output_data, "county_demand_panel_drought.csv"))
panel <- panel[!is.na(ln_net_head) & !is.na(ln_subsidy) &
               !is.na(cov_level_avg) & !is.na(endors_len_avg)]
panel <- panel[!is.na(year) & year_quarter != "NAQNA"]
panel[, year       := as.integer(year)]
panel[, state_fips := as.integer(fips %/% 1000)]
panel[, iv_post19_cov := post2019 * cov_level_avg]
panel[, iv_post20_cov := post2020 * cov_level_avg]

feeder <- panel[cattle_type == "feeder"]
fed    <- panel[cattle_type == "fed"]

# =============================================================================
# STAGE 1: FE-IV  (same as Model 1)
# Extract residuals after removing subsidy effect + state FE + time FE
# =============================================================================

stage1_feeder <- feols(
  ln_net_head ~ ln_cov_price + endors_len_avg |
    state_fips + year_quarter |
    ln_subsidy ~ iv_post19_cov,
  data = feeder, vcov = ~state_fips
)

stage1_fed <- feols(
  ln_net_head ~ ln_cov_price + endors_len_avg |
    state_fips + year_quarter |
    ln_subsidy ~ iv_post19_cov + iv_post20_cov,
  data = fed, vcov = ~state_fips
)

# Attach residuals back to panel
feeder[, resid_demand := NA_real_]
fed[,    resid_demand := NA_real_]
feeder[as.integer(names(residuals(stage1_feeder))),
       resid_demand := residuals(stage1_feeder)]
fed[as.integer(names(residuals(stage1_fed))),
    resid_demand := residuals(stage1_fed)]

cat("Feeder residuals — mean:", round(mean(feeder$resid_demand, na.rm=TRUE),4),
    "| SD:", round(sd(feeder$resid_demand, na.rm=TRUE),4), "
")
cat("Fed    residuals — mean:", round(mean(fed$resid_demand,    na.rm=TRUE),4),
    "| SD:", round(sd(fed$resid_demand,    na.rm=TRUE),4), "

")

# =============================================================================
# STAGE 2: Regress residuals on drought + county FE + time FE
# County FE here captures time-invariant county characteristics
# (geography, herd size, local market access)
# Time FE captures national drought trends already absorbed in stage 1
# What remains: within-county, within-time drought variation
# =============================================================================

# 2A: drought_severe only, county FE + year_quarter FE
stage2a_feeder <- feols(
  resid_demand ~ drought_severe | fips + year_quarter,
  data = feeder[!is.na(resid_demand)],
  vcov = ~fips
)

stage2a_fed <- feols(
  resid_demand ~ drought_severe | fips + year_quarter,
  data = fed[!is.na(resid_demand)],
  vcov = ~fips
)

# 2B: Add drought x post2020 interaction
stage2b_feeder <- feols(
  resid_demand ~ drought_severe + drought_x_post2020 | fips + year_quarter,
  data = feeder[!is.na(resid_demand)],
  vcov = ~fips
)

stage2b_fed <- feols(
  resid_demand ~ drought_severe + drought_x_post2020 | fips + year_quarter,
  data = fed[!is.na(resid_demand)],
  vcov = ~fips
)

# 2C: Lagged drought (t-1 quarter) — does past drought predict current demand?
setorder(feeder, fips, year_quarter)
setorder(fed,    fips, year_quarter)
feeder[, drought_lag1 := shift(drought_severe, 1), by=fips]
fed[,    drought_lag1 := shift(drought_severe, 1), by=fips]

stage2c_feeder <- feols(
  resid_demand ~ drought_severe + drought_lag1 | fips + year_quarter,
  data = feeder[!is.na(resid_demand) & !is.na(drought_lag1)],
  vcov = ~fips
)

stage2c_fed <- feols(
  resid_demand ~ drought_severe + drought_lag1 | fips + year_quarter,
  data = fed[!is.na(resid_demand) & !is.na(drought_lag1)],
  vcov = ~fips
)

# 2D: drought_extreme (D3+D4 only) — severe tail events
stage2d_feeder <- feols(
  resid_demand ~ drought_extreme | fips + year_quarter,
  data = feeder[!is.na(resid_demand)],
  vcov = ~fips
)

stage2d_fed <- feols(
  resid_demand ~ drought_extreme | fips + year_quarter,
  data = fed[!is.na(resid_demand)],
  vcov = ~fips
)

# =============================================================================
# RESULTS
# =============================================================================

cat("=== STAGE 2 RESULTS: Drought on Demand Residuals ===

")

cat("--- 2A: drought_severe (county + quarter FE) ---
")
cat("Feeder:", round(coef(stage2a_feeder)["drought_severe"],6),
    "| p =", round(summary(stage2a_feeder)$coeftable["drought_severe","Pr(>|t|)"],3),
    "| R2_within:", round(r2(stage2a_feeder,"ar2"),4), "
")
cat("Fed:   ", round(coef(stage2a_fed)["drought_severe"],6),
    "| p =", round(summary(stage2a_fed)$coeftable["drought_severe","Pr(>|t|)"],3),
    "| R2_within:", round(r2(stage2a_fed,"ar2"),4), "

")

cat("--- 2B: + drought x post2020 ---
")
cat("Feeder drought:          ", round(coef(stage2b_feeder)["drought_severe"],6),
    "p =", round(summary(stage2b_feeder)$coeftable["drought_severe","Pr(>|t|)"],3), "
")
cat("Feeder drought x post2020:", round(coef(stage2b_feeder)["drought_x_post2020"],6),
    "p =", round(summary(stage2b_feeder)$coeftable["drought_x_post2020","Pr(>|t|)"],3), "
")
cat("Fed    drought:          ", round(coef(stage2b_fed)["drought_severe"],6),
    "p =", round(summary(stage2b_fed)$coeftable["drought_severe","Pr(>|t|)"],3), "
")
cat("Fed    drought x post2020:", round(coef(stage2b_fed)["drought_x_post2020"],6),
    "p =", round(summary(stage2b_fed)$coeftable["drought_x_post2020","Pr(>|t|)"],3), "

")

cat("--- 2C: + lagged drought ---
")
cat("Feeder lag1:", round(coef(stage2c_feeder)["drought_lag1"],6),
    "p =", round(summary(stage2c_feeder)$coeftable["drought_lag1","Pr(>|t|)"],3), "
")
cat("Fed    lag1:", round(coef(stage2c_fed)["drought_lag1"],6),
    "p =", round(summary(stage2c_fed)$coeftable["drought_lag1","Pr(>|t|)"],3), "

")

cat("--- 2D: drought_extreme (D3+D4) ---
")
cat("Feeder:", round(coef(stage2d_feeder)["drought_extreme"],6),
    "p =", round(summary(stage2d_feeder)$coeftable["drought_extreme","Pr(>|t|)"],3), "
")
cat("Fed:   ", round(coef(stage2d_fed)["drought_extreme"],6),
    "p =", round(summary(stage2d_fed)$coeftable["drought_extreme","Pr(>|t|)"],3), "

")

# =============================================================================
# EXPORT TABLE
# =============================================================================

modelsummary(
  list(
    "Feeder (2A)" = stage2a_feeder,
    "Fed (2A)"    = stage2a_fed,
    "Feeder (2B)" = stage2b_feeder,
    "Fed (2B)"    = stage2b_fed,
    "Feeder (2C)" = stage2c_feeder,
    "Fed (2C)"    = stage2c_fed,
    "Feeder (2D)" = stage2d_feeder,
    "Fed (2D)"    = stage2d_fed
  ),
  coef_rename = c(
    "drought_severe"     = "Drought Severity (D2+D3+D4)",
    "drought_x_post2020" = "Drought x Post2020",
    "drought_lag1"       = "Drought Severity (lag 1 quarter)",
    "drought_extreme"    = "Extreme Drought (D3+D4)"
  ),
  stars   = c("*"=0.1, "**"=0.05, "***"=0.01),
  gof_map = c("nobs","r.squared"),
  add_rows = data.frame(
    term = c("County FE","Year-Quarter FE","SE clustered by","Dep. Var."),
    `Feeder (2A)` = c("Yes","Yes","County","Demand Residual"),
    `Fed (2A)`    = c("Yes","Yes","County","Demand Residual"),
    `Feeder (2B)` = c("Yes","Yes","County","Demand Residual"),
    `Fed (2B)`    = c("Yes","Yes","County","Demand Residual"),
    `Feeder (2C)` = c("Yes","Yes","County","Demand Residual"),
    `Fed (2C)`    = c("Yes","Yes","County","Demand Residual"),
    `Feeder (2D)` = c("Yes","Yes","County","Demand Residual"),
    `Fed (2D)`    = c("Yes","Yes","County","Demand Residual"),
    check.names = FALSE
  ),
  output = file.path(output_tables, "table3_drought_residual.tex"),
  title  = "Stage 2: Effect of Drought on LRP Demand Residuals"
)
cat("Table saved.
")

# Save log
sink(file.path(output_logs, "model3_drought_residual.txt"))
cat("Run:", as.character(Sys.time()), "

")
cat("Stage 1 subsidy elasticity — feeder:", round(coef(stage1_feeder)["fit_ln_subsidy"],4), "
")
cat("Stage 1 subsidy elasticity — fed:   ", round(coef(stage1_fed)["fit_ln_subsidy"],4), "

")
cat("Stage 2 drought_severe — feeder:", round(coef(stage2a_feeder)["drought_severe"],6), "
")
cat("Stage 2 drought_severe — fed:   ", round(coef(stage2a_fed)["drought_severe"],6), "
")
print(summary(stage2a_feeder))
print(summary(stage2a_fed))
print(summary(stage2b_feeder))
print(summary(stage2b_fed))
sink()
cat("Log saved.
")
