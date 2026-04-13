source(here::here("codes", "00_config.R"))
library(data.table)
library(fixest)
library(modelsummary)

# ── Load and clean ────────────────────────────────────────────────────────────
panel <- fread(file.path(output_data, "county_demand_panel_drought.csv"))
panel <- panel[!is.na(ln_net_head) & !is.na(ln_subsidy) &
               !is.na(cov_level_avg) & !is.na(endors_len_avg)]
panel <- panel[!is.na(year) & year_quarter != "NAQNA"]
panel[, year       := as.integer(year)]
panel[, state_fips := as.integer(fips %/% 1000)]

# Instruments
panel[, iv_post19_cov := post2019 * cov_level_avg]
panel[, iv_post20_cov := post2020 * cov_level_avg]

feeder <- panel[cattle_type == "feeder"]
fed    <- panel[cattle_type == "fed"]
cat("Feeder:", nrow(feeder), "| Fed:", nrow(fed), "

")

# =============================================================================
# MODEL 1: MAIN RESULTS
# Feeder: single IV (post19 x cov) — avoids Sargan rejection
# Fed:    two IVs  (post19 x cov + post20 x cov) — Sargan passes (p=0.998)
# FE: state + year_quarter
# SE: clustered by state
# =============================================================================

# Main specification
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

# =============================================================================
# ROBUSTNESS CHECKS
# =============================================================================

# R1: Both IVs for feeder (compare to main)
rob1_feeder <- feols(
  ln_net_head ~ ln_cov_price + endors_len_avg |
    state_fips + year_quarter |
    ln_subsidy ~ iv_post19_cov + iv_post20_cov,
  data = feeder, vcov = ~state_fips
)

# R2: County FE + year FE (stricter FE)
rob2_feeder <- feols(
  ln_net_head ~ ln_cov_price + endors_len_avg |
    fips + year |
    ln_subsidy ~ iv_post19_cov,
  data = feeder, vcov = ~fips
)

rob2_fed <- feols(
  ln_net_head ~ ln_cov_price + endors_len_avg |
    fips + year |
    ln_subsidy ~ iv_post19_cov + iv_post20_cov,
  data = fed, vcov = ~fips
)

# R3: Post-2015 subsample (LRP more established)
rob3_feeder <- feols(
  ln_net_head ~ ln_cov_price + endors_len_avg |
    state_fips + year_quarter |
    ln_subsidy ~ iv_post19_cov,
  data = feeder[year >= 2015], vcov = ~state_fips
)

rob3_fed <- feols(
  ln_net_head ~ ln_cov_price + endors_len_avg |
    state_fips + year_quarter |
    ln_subsidy ~ iv_post19_cov + iv_post20_cov,
  data = fed[year >= 2015], vcov = ~state_fips
)

# =============================================================================
# PRINT RESULTS
# =============================================================================
cat("=== MAIN RESULTS ===
")
cat("Feeder elasticity:", round(coef(main_feeder)["fit_ln_subsidy"],4),
    "| F:", round(fitstat(main_feeder,"ivf")[[1]]$stat,2),
    "| N:", main_feeder$nobs, "
")
cat("Fed    elasticity:", round(coef(main_fed)["fit_ln_subsidy"],4),
    "| F:", round(fitstat(main_fed,"ivf")[[1]]$stat,2),
    "| Sargan p:", round(fitstat(main_fed,"sargan")[[1]]$p,3),
    "| N:", main_fed$nobs, "

")

cat("=== ROBUSTNESS ===
")
cat("R1 Feeder both IVs:   ", round(coef(rob1_feeder)["fit_ln_subsidy"],4),
    "| Sargan p:", round(fitstat(rob1_feeder,"sargan")[[1]]$p,3), "
")
cat("R2 Feeder county+year:", round(coef(rob2_feeder)["fit_ln_subsidy"],4), "
")
cat("R2 Fed    county+year:", round(coef(rob2_fed)["fit_ln_subsidy"],4), "
")
cat("R3 Feeder post-2015:  ", round(coef(rob3_feeder)["fit_ln_subsidy"],4), "
")
cat("R3 Fed    post-2015:  ", round(coef(rob3_fed)["fit_ln_subsidy"],4), "

")

cat("=== FULL SECOND STAGE ===
")
print(summary(main_feeder))
print(summary(main_fed))

# =============================================================================
# EXPORT TABLES
# =============================================================================

# Main table
modelsummary(
  list("Feeder" = main_feeder, "Fed" = main_fed),
  coef_rename = c(
    "fit_ln_subsidy" = "ln(Subsidy Rate) [IV]",
    "ln_cov_price"   = "ln(Coverage Price)",
    "endors_len_avg" = "Endorsement Length"
  ),
  stars    = c("*"=0.1, "**"=0.05, "***"=0.01),
  gof_map  = c("nobs","r.squared"),
  add_rows = data.frame(
    term = c("State FE","Year-Quarter FE","Clustered SE","Instruments"),
    Feeder = c("Yes","Yes","State","Post2019 x Cov"),
    Fed    = c("Yes","Yes","State","Post2019/2020 x Cov")
  ),
  output = file.path(output_tables, "table1_main.tex"),
  title  = "FE-IV: Effect of Premium Subsidies on LRP Demand"
)

# Robustness table
modelsummary(
  list("Feeder (both IV)"   = rob1_feeder,
       "Feeder (county FE)" = rob2_feeder,
       "Fed (county FE)"    = rob2_fed,
       "Feeder (post-2015)" = rob3_feeder,
       "Fed (post-2015)"    = rob3_fed),
  coef_rename = c("fit_ln_subsidy" = "ln(Subsidy Rate) [IV]"),
  stars   = c("*"=0.1, "**"=0.05, "***"=0.01),
  gof_map = c("nobs","r.squared"),
  output  = file.path(output_tables, "table1_robustness.tex"),
  title   = "Robustness Checks: Subsidy Elasticity"
)
cat("Tables saved.
")

# =============================================================================
# SAVE LOG
# =============================================================================
sink(file.path(output_logs, "model1_final.txt"))
cat("Run:", as.character(Sys.time()), "

")
cat("MAIN RESULTS
")
cat("Feeder elasticity:", round(coef(main_feeder)["fit_ln_subsidy"],4),
    "F:", round(fitstat(main_feeder,"ivf")[[1]]$stat,2), "
")
cat("Fed    elasticity:", round(coef(main_fed)["fit_ln_subsidy"],4),
    "F:", round(fitstat(main_fed,"ivf")[[1]]$stat,2),
    "Sargan p:", round(fitstat(main_fed,"sargan")[[1]]$p,3), "

")
cat("ROBUSTNESS
")
cat("R1:", round(coef(rob1_feeder)["fit_ln_subsidy"],4), "
")
cat("R2 feeder:", round(coef(rob2_feeder)["fit_ln_subsidy"],4), "
")
cat("R2 fed:   ", round(coef(rob2_fed)["fit_ln_subsidy"],4), "
")
cat("R3 feeder:", round(coef(rob3_feeder)["fit_ln_subsidy"],4), "
")
cat("R3 fed:   ", round(coef(rob3_fed)["fit_ln_subsidy"],4), "

")
print(summary(main_feeder))
print(summary(main_fed))
sink()
cat("Log saved.
")
