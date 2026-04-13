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

# Standardize drought variable (easier interpretation)
panel[, drought_severe_std := scale(drought_severe)]
panel[, drought_x_post2020_std := drought_severe_std * post2020]

feeder <- panel[cattle_type == "feeder"]
fed    <- panel[cattle_type == "fed"]
cat("Drought missing — feeder:", feeder[is.na(drought_severe), .N],
    "| fed:", fed[is.na(drought_severe), .N], "

")

# =============================================================================
# MODEL 2A: Add drought_severe
# =============================================================================

m2a_feeder <- feols(
  ln_net_head ~ ln_cov_price + endors_len_avg +
    drought_severe |
    state_fips + year_quarter |
    ln_subsidy ~ iv_post19_cov,
  data = feeder, vcov = ~state_fips
)

m2a_fed <- feols(
  ln_net_head ~ ln_cov_price + endors_len_avg +
    drought_severe |
    state_fips + year_quarter |
    ln_subsidy ~ iv_post19_cov + iv_post20_cov,
  data = fed, vcov = ~state_fips
)

# =============================================================================
# MODEL 2B: Add drought x post2020 interaction
# (tests whether drought effect changes after subsidy reform)
# =============================================================================

m2b_feeder <- feols(
  ln_net_head ~ ln_cov_price + endors_len_avg +
    drought_severe + drought_x_post2020 |
    state_fips + year_quarter |
    ln_subsidy ~ iv_post19_cov,
  data = feeder, vcov = ~state_fips
)

m2b_fed <- feols(
  ln_net_head ~ ln_cov_price + endors_len_avg +
    drought_severe + drought_x_post2020 |
    state_fips + year_quarter |
    ln_subsidy ~ iv_post19_cov + iv_post20_cov,
  data = fed, vcov = ~state_fips
)

# =============================================================================
# RESULTS
# =============================================================================
cat("=== MODEL 2A: + drought_severe ===
")
cat("Feeder — subsidy:", round(coef(m2a_feeder)["fit_ln_subsidy"],4),
    "| drought:", round(coef(m2a_feeder)["drought_severe"],4), "
")
cat("Fed    — subsidy:", round(coef(m2a_fed)["fit_ln_subsidy"],4),
    "| drought:", round(coef(m2a_fed)["drought_severe"],4), "

")

cat("=== MODEL 2B: + drought x post2020 ===
")
cat("Feeder — subsidy:", round(coef(m2b_feeder)["fit_ln_subsidy"],4),
    "| drought:", round(coef(m2b_feeder)["drought_severe"],4),
    "| drought x post2020:", round(coef(m2b_feeder)["drought_x_post2020"],4), "
")
cat("Fed    — subsidy:", round(coef(m2b_fed)["fit_ln_subsidy"],4),
    "| drought:", round(coef(m2b_fed)["drought_severe"],4),
    "| drought x post2020:", round(coef(m2b_fed)["drought_x_post2020"],4), "

")

print(summary(m2a_feeder))
print(summary(m2a_fed))
print(summary(m2b_feeder))
print(summary(m2b_fed))

# =============================================================================
# COMBINED TABLE: Model 1 + Model 2
# =============================================================================

# Reload Model 1 results
main_feeder <- feols(
  ln_net_head ~ ln_cov_price + endors_len_avg |
    state_fips + year_quarter |
    ln_subsidy ~ iv_post19_cov,
  data = feeder, vcov = ~state_fips
)
main_fed <- feols(
  ln_net_head ~ ln_cov_price + endors_len_avg |
    state_fips + year_quarter |
    ln_subsidy ~ iv_post19_cov + iv_post20_cov,
  data = fed, vcov = ~state_fips
)

modelsummary(
  list(
    "(1) Feeder"       = main_feeder,
    "(2) Fed"          = main_fed,
    "(3) Feeder+Drought" = m2a_feeder,
    "(4) Fed+Drought"    = m2a_fed,
    "(5) Feeder+Interact"= m2b_feeder,
    "(6) Fed+Interact"   = m2b_fed
  ),
  coef_rename = c(
    "fit_ln_subsidy"    = "ln(Subsidy Rate) [IV]",
    "ln_cov_price"      = "ln(Coverage Price)",
    "endors_len_avg"    = "Endorsement Length",
    "drought_severe"    = "Drought Severity (D2+D3+D4)",
    "drought_x_post2020"= "Drought x Post2020"
  ),
  stars   = c("*"=0.1, "**"=0.05, "***"=0.01),
  gof_map = c("nobs","r.squared"),
  add_rows = data.frame(
    term = c("State FE","Year-Quarter FE","SE clustered by"),
    `(1) Feeder`        = c("Yes","Yes","State"),
    `(2) Fed`           = c("Yes","Yes","State"),
    `(3) Feeder+Drought`= c("Yes","Yes","State"),
    `(4) Fed+Drought`   = c("Yes","Yes","State"),
    `(5) Feeder+Interact`=c("Yes","Yes","State"),
    `(6) Fed+Interact`  = c("Yes","Yes","State"),
    check.names = FALSE
  ),
  output = file.path(output_tables, "table2_drought.tex"),
  title  = "FE-IV: Subsidies, Drought, and LRP Demand"
)
cat("Table saved:", file.path(output_tables, "table2_drought.tex"), "
")

sink(file.path(output_logs, "model2_drought.txt"))
cat("Run:", as.character(Sys.time()), "

")
cat("Model 2A feeder — subsidy:", round(coef(m2a_feeder)["fit_ln_subsidy"],4),
    "drought:", round(coef(m2a_feeder)["drought_severe"],4), "
")
cat("Model 2A fed    — subsidy:", round(coef(m2a_fed)["fit_ln_subsidy"],4),
    "drought:", round(coef(m2a_fed)["drought_severe"],4), "
")
cat("Model 2B feeder — drought x post2020:",
    round(coef(m2b_feeder)["drought_x_post2020"],4), "
")
cat("Model 2B fed    — drought x post2020:",
    round(coef(m2b_fed)["drought_x_post2020"],4), "
")
print(summary(m2a_feeder))
print(summary(m2b_feeder))
sink()
cat("Log saved.
")
