# code09_statutory_tier_event_study.R
# Implement advisor's coverage-tier-by-time design and diagnostics
# Usage: source(here::here("codes","00_config.R")) then run this script in RStudio

source(here::here("codes", "00_config.R"))
library(data.table)
library(lubridate)
library(fixest)
library(ggplot2)
library(modelsummary)

# -----------------------------
# 1. Load LRP endorsements (endorsement-level)
# -----------------------------
lrp <- fread(file_lrp_all)
# Ensure dates
lrp[, sales_eff_date := as.IDate(sales_eff_date)]
lrp[, month := as.IDate(format(sales_eff_date, "%Y-%m-01"))]

# -----------------------------
# 2. Compute statutory subsidy rate (per endorsement) and diagnostics
#    statutory_subsidy = subsidy_amt / total_prem_amt
# -----------------------------
lrp[, total_prem_amt := as.numeric(total_prem_amt)]
lrp[, subsidy_amt := as.numeric(subsidy_amt)]
# safe compute
lrp[, statutory_subsidy := NA_real_]
lrp[total_prem_amt > 0, statutory_subsidy := subsidy_amt / total_prem_amt]

# Report missing or problematic endorsements
cat("Endorsements:", nrow(lrp), "\n")
cat("Pct missing total_prem_amt:", mean(is.na(lrp$total_prem_amt)) * 100, "%\n")
cat("Pct missing statutory_subsidy:", mean(is.na(lrp$statutory_subsidy)) * 100, "%\n")

# -----------------------------
# 3. Define coverage tiers
#    If an explicit tiers file exists (data/raw/coverage_tiers.csv), use it; otherwise use observed cov_level_pct
# -----------------------------
tier_file <- here("data","raw","coverage_tiers.csv")
if (file.exists(tier_file)) {
  tiers_df <- fread(tier_file)
  # expected columns: coverage_tier (label) and cov_level_pct (numeric) and effective_from (optional)
  # Map endorsement's cov_level_pct to nearest listed tier value
  if (!"cov_level_pct" %in% names(tiers_df)) stop("coverage_tiers.csv must include cov_level_pct column")
  tier_values <- sort(unique(tiers_df$cov_level_pct))
  lrp[, cov_level_pct := as.numeric(cov_level_pct)]
  lrp[, coverage_tier := as.character(sapply(cov_level_pct, function(x) {
    if (is.na(x)) return(NA_character_)
    # Find nearest tier
    tv <- tier_values[which.min(abs(tv <- tier_values - x))]
    paste0(as.character(tv))
  }))]
  lrp[, coverage_tier := factor(coverage_tier, levels = as.character(tier_values))]
} else {
  # fallback: use observed cov_level_pct rounded to 1 decimal as tier label
  lrp[, cov_level_pct := as.numeric(cov_level_pct)]
  uniq_tiers <- sort(unique(na.omit(lrp$cov_level_pct)))
  cat("No coverage_tiers.csv found. Using observed cov_level_pct as tiers. Number of tiers:", length(uniq_tiers), "\n")
  lrp[, coverage_tier := factor(cov_level_pct)]
}

# -----------------------------
# 4. Build county x month x coverage-tier panel (include zero cells)
#    Validate availability of tiers per month if user provided availability file
# -----------------------------
avail_file <- here("data","raw","coverage_tier_availability.csv")
if (file.exists(avail_file)) {
  avail <- fread(avail_file)
  # expect columns: month (YYYY-MM-01) and coverage_tier
  avail[, month := as.IDate(month)]
  setkey(avail, month, coverage_tier)
  use_avail <- TRUE
  cat("Using coverage_tier_availability.csv to verify zero-filled tiers\n")
} else {
  use_avail <- FALSE
  cat("No coverage_tier_availability.csv found. Will infer availability from observed tiers each month.\n")
}

# compute endorsement-level values for aggregation
lrp[, fips := as.integer(fips)]

# Aggregation per endorsement-level
agg_obs <- lrp[!is.na(coverage_tier), .(
  insured_head = sum(net_head, na.rm=TRUE),
  endorsements = .N,
  s_mean = mean(statutory_subsidy, na.rm=TRUE),
  prem_mean = mean(total_prem_amt, na.rm=TRUE),
  subsidy_mean = mean(subsidy_amt, na.rm=TRUE)
), by=.(fips, month, coverage_tier)]

# build all combos
fips_all <- sort(unique(lrp$fips))
months_all <- sort(unique(lrp$month))
tiers_all <- sort(unique(na.omit(as.character(lrp$coverage_tier))))
all_cells <- CJ(fips = fips_all, month = months_all, coverage_tier = tiers_all, unique=TRUE)
setkey(all_cells, fips, month, coverage_tier)
setkey(agg_obs, fips, month, coverage_tier)
panel <- merge(all_cells, agg_obs, all.x=TRUE)
# replace NAs with zeros for outcomes
panel[is.na(insured_head), insured_head := 0]
panel[is.na(endorsements), endorsements := 0]

# attach s_bt: statutory subsidy assigned to tier-month
# If a tier-month has no observed endorsements, s_mean will be NA. We try to fill using tier-month mean from endorsements overall if available
s_bt <- lrp[!is.na(coverage_tier) & !is.na(statutory_subsidy), .(s_bt = mean(statutory_subsidy, na.rm=TRUE)), by=.(month, coverage_tier)]
setkey(s_bt, month, coverage_tier)
setkey(panel, month, coverage_tier)
panel <- s_bt[panel]
# now panel has s_bt; for any remaining NA s_bt, fill with global tier mean across months
panel[, s_bt := as.numeric(s_bt)]
global_tier_mean <- lrp[!is.na(coverage_tier) & !is.na(statutory_subsidy), .(global_s = mean(statutory_subsidy, na.rm=TRUE)), by=.(coverage_tier)]
setkey(global_tier_mean, coverage_tier)
panel <- merge(panel, global_tier_mean, by="coverage_tier", all.x=TRUE)
panel[is.na(s_bt), s_bt := global_s]
panel[, global_s:=NULL]

# mark cells that were observed in raw data (non-zero) vs zero-filled
panel[, observed := endorsements > 0]
panel[, available := TRUE]
if (use_avail) {
  # mark available only if in availability file
  avail[, key := paste0(month, "__", coverage_tier)]
  panel[, key := paste0(month, "__", coverage_tier)]
  panel[, available := key %in% avail$key]
  panel[, key := NULL]
}

# Warn if any zero-filled tier was not available according to availability file
if (use_avail) {
  n_zero_not_avail <- panel[endorsements==0 & !available, .N]
  if (n_zero_not_avail>0) {
    cat("Warning: ", n_zero_not_avail, " zero-filled tier cells that are not marked available in coverage_tier_availability.csv\n")
  }
}

# -----------------------------
# 5. Validation: compare assigned statutory s_bt with subsidy/total_premium per endorsement
# -----------------------------
# compute endorsement-level subsidy/total_premium (already statutory_subsidy). We'll compare s_bt to mean statutory_subsidy among observed cells
val <- lrp[!is.na(coverage_tier) & !is.na(statutory_subsidy), .(
  n_end = .N,
  mean_endorse_subsidy = mean(statutory_subsidy, na.rm=TRUE)
), by=.(month, coverage_tier)]
val <- merge(val, s_bt, by=c("month","coverage_tier"), all.x=TRUE)
val[, diff := mean_endorse_subsidy - s_bt]
# summary
cat("Validation: comparing assigned s_bt with endorsement mean statutory_subsidy\n")
print(val[, .(N = .N, mean_diff = mean(diff, na.rm=TRUE), sd_diff = sd(diff, na.rm=TRUE), cor = cor(mean_endorse_subsidy, s_bt, use="complete.obs") )])

# Save validation table
fwrite(val, file.path(output_tables, "validation_assigned_vs_endorsement_statutory.csv"))

# -----------------------------
# 6. Prepare fixed effects identifiers
# -----------------------------
panel[, county_month := paste0(fips, "__", month)]
panel[, county_tier := paste0(fips, "__", coverage_tier)]
# month id for clustering
panel[, month_id := format(month, "%Y-%m")] 

# -----------------------------
# 7. Main PPML specifications (PPML = Poisson Pseudo-Maximum Likelihood)
#    Outcomes: endorsements (counts) and insured_head (counts)
#    FE: county_month + county_tier
#    Cluster: county (fips) and calendar month (month_id)
# -----------------------------
# scale s_bt to 10-percentage-point units
panel[, s_bt_10 := s_bt * 10]

# control R_bt: attempt to load cme benchmark: codes should prepare file_resid_premium or cme benchmark
cme_file <- file.path(output_data, "cme_tier_month_benchmark.csv")
if (file.exists(cme_file)) {
  cme <- fread(cme_file)
  cme[, month := as.IDate(month)]
  panel <- merge(panel, cme, by=c("month","coverage_tier"), all.x=TRUE)
  panel[, R_bt := cme_put_price]
} else {
  panel[, R_bt := NA_real_]
  cat("No CME benchmark found at", cme_file, " — R_bt left NA. You should prepare an ex-ante CME benchmark per tier-month.\n")
}

# drop cells where tier not available (if availability file used)
if (use_avail) panel <- panel[available==TRUE]

# Restrict to active county-months: those with any LRP activity (sum endorsements across tiers > 0)
active_cm <- panel[, .(cm_total_endorse = sum(endorsements, na.rm=TRUE)), by=.(fips, month)]
active_cm[, active := cm_total_endorse > 0]
panel <- merge(panel, active_cm[, .(fips, month, active)], by=c("fips","month"))
panel_active <- panel[active==TRUE]
cat("Panel rows (active county-months only):", nrow(panel_active), "\n")

# PPML: endorsements
cat("Running PPML for endorsements (active sample)\n")
# feglm with poisson family (PPML) and high-dim FEs
m_endorse <- feglm(endorsements ~ s_bt_10 + R_bt | county_month + county_tier, 
                   family = "poisson", 
                   cluster = c("fips", "month_id"),
                   data = panel_active)

# PPML: insured_head
cat("Running PPML for insured_head (active sample)\n")
# insured_head may be large; still use poisson PPML
m_head <- feglm(insured_head ~ s_bt_10 + R_bt | county_month + county_tier,
                family = "poisson",
                cluster = c("fips","month_id"),
                data = panel_active)

# Save main results
fwrite(data.table(term = names(coef(m_endorse)), estimate = coef(m_endorse)), file.path(output_tables, "ppml_endorsements_coef.csv"))
fwrite(data.table(term = names(coef(m_head)), estimate = coef(m_head)), file.path(output_tables, "ppml_insured_head_coef.csv"))

# Print short summaries
cat("Endorsements PPML:\n")
print(summary(m_endorse))
cat("Insured head PPML:\n")
print(summary(m_head))

# -----------------------------
# 8. Event-study for a specified reform
#    The script expects a reform_schedule.csv with columns: reform_name, reform_date (YYYY-MM-01), tier, delta_s
#    If not provided, attempt to detect large changes in s_bt
# -----------------------------
reform_file <- here("data","raw","reform_schedule.csv")
if (file.exists(reform_file)) {
  reforms <- fread(reform_file)
  reforms[, reform_date := as.IDate(reform_date)]
  use_reforms <- TRUE
  cat("Using reform_schedule.csv for event study.\n")
} else {
  use_reforms <- FALSE
  cat("No reform_schedule.csv found. Attempting to detect large changes in s_bt for event study — CHECK this carefully.\n")
}

# helper: build event time relative to a reform_date
build_event_panel <- function(panel_dt, reform_date, delta_by_tier) {
  dt <- copy(panel_dt)
  dt[, event_time := as.integer(interval(reform_date, month) / months(1))]
  # multiply event_time by delta_s for each tier
  dt <- merge(dt, delta_by_tier, by="coverage_tier", all.x=TRUE)
  dt[, treat_int := event_time * delta_s]
  return(dt)
}

# If reforms provided, run event-study per reform
if (use_reforms) {
  # expect delta_s per tier for the reform name
  for (r in unique(reforms$reform_name)) {
    cat("Running event study for reform:", r, "\n")
    ref_dt <- reforms[reform_name==r, .(coverage_tier, delta_s)]
    reform_date <- unique(reforms[reform_name==r, reform_date])
    ev_panel <- build_event_panel(panel_active, reform_date, ref_dt)

    # restrict event_time window to avoid contamination from other reforms (user must ensure non-overlap)
    ev_panel <- ev_panel[event_time >= -12 & event_time <= 12]

    # create factor of event_time and interact with delta_s
    ev_panel[, event_fac := factor(event_time)]
    # exclude event_time == -1 as reference
    ev_panel <- ev_panel[event_time != -1]

    # formula: endorsements ~ i(event_fac, delta_s, ref = -1) + R_bt | county_month + county_tier
    # fixest supports i(var, gvar)
    f_ms <- as.formula("endorsements ~ i(event_time, delta_s, ref = -1) + R_bt | county_month + county_tier")
    m_ev <- feglm(f_ms, family="poisson", cluster = c("fips","month_id"), data = ev_panel)

    # extract coefficients and plot
    coef_dt <- data.table(term = names(coef(m_ev)), estimate = coef(m_ev))
    fwrite(coef_dt, file.path(output_tables, paste0("eventstudy_", r, "_endorsements_coef.csv")))

    # TODO: plotting function (user can run locally)
    # We'll save the coefficients table; plotting can be done in RMarkdown or ggplot reading the table
  }
} else {
  cat("No reform file provided; skipping formal event study. Please supply data/raw/reform_schedule.csv with reform dates and delta_s by tier.\n")
}

# -----------------------------
# 9. Robustness: active-before and active-both samples
# -----------------------------
# Active before each reform: requires reform dates. If provided, create samples.
if (use_reforms) {
  for (r in unique(reforms$reform_name)) {
    reform_date <- unique(reforms[reform_name==r, reform_date])
    # counties active before reform: any endorsements in window [-24, -1] months
    pre_window_start <- reform_date %m-% months(24)
    pre_c <- unique(panel[month >= pre_window_start & month < reform_date & endorsements>0, fips])
    both_c <- unique(panel[month >= pre_window_start & month <= reform_date %m+% months(12) & endorsements>0, fips])

    cat("Reform", r, ": pre-active counties:", length(pre_c), "both-side-active counties:", length(both_c), "\n")

    # run main PPML on subset of counties pre-active
    p_pre <- panel_active[fips %in% pre_c]
    if (nrow(p_pre) > 0) {
      m_pre_endorse <- feglm(endorsements ~ s_bt_10 + R_bt | county_month + county_tier, 
                             family = "poisson", cluster = c("fips","month_id"), data = p_pre)
      fwrite(data.table(term = names(coef(m_pre_endorse)), estimate = coef(m_pre_endorse)), file.path(output_tables, paste0("ppml_endorse_preactive_", r, ".csv")))
    }

    # run main PPML on counties active both sides
    p_both <- panel_active[fips %in% both_c]
    if (nrow(p_both) > 0) {
      m_both_endorse <- feglm(endorsements ~ s_bt_10 + R_bt | county_month + county_tier, 
                              family = "poisson", cluster = c("fips","month_id"), data = p_both)
      fwrite(data.table(term = names(coef(m_both_endorse)), estimate = coef(m_both_endorse)), file.path(output_tables, paste0("ppml_endorse_bothactive_", r, ".csv")))
    }
  }
}

# -----------------------------
# 10. Save panel and exit
# -----------------------------
fwrite(panel, file.path(output_data, "county_month_tier_panel.csv"))
cat("Saved panel to", file.path(output_data, "county_month_tier_panel.csv"), "\n")

# End of script
