# =============================================================================
# county_level / code01_build_lrp_county_panel.R
#
# PURPOSE:
#   Build a comprehensive county-level LRP panel (2005-2026) from annual
#   Summary of Business files.
#
# READS:   data/raw/lrp_annual/lrp_2005.csv ... lrp_2026.csv
# WRITES:  data/processed/lrp_county_panel.csv
# =============================================================================

source(here::here("codes", "00_config.R"))
library(data.table)
library(lubridate)

# ── 0. DIAGNOSTIC: check paths before doing anything ─────────────────────────

cat("Project root:     ", here::here(), "\n")
cat("input_lrp_annual: ", input_lrp_annual, "\n")
cat("Folder exists:    ", dir.exists(input_lrp_annual), "\n")

if (!dir.exists(input_lrp_annual)) {
  cat("\n*** FOLDER NOT FOUND ***\n")
  cat("Expected folder: ", input_lrp_annual, "\n")
  cat("\nFiles currently in data/raw:\n")
  print(list.files(here::here("data", "raw"), recursive = TRUE))
  cat("\nFIX: copy lrp_YYYY.csv files into data/raw/lrp_annual/\n")
  stop("Stopping — please copy data files first.")
}

# ── 1. Stack all annual files ─────────────────────────────────────────────────

years <- 2005:2026
files <- file.path(input_lrp_annual, paste0("lrp_", years, ".csv"))
files <- files[file.exists(files)]

if (length(files) == 0) {
  cat("\n*** NO FILES FOUND in", input_lrp_annual, "***\n")
  cat("Files in that folder:\n")
  print(list.files(input_lrp_annual))
  stop("Stopping — no lrp_YYYY.csv files found.")
}

cat("Reading", length(files), "annual files...\n")
lrp <- rbindlist(lapply(files, fread, colClasses = "character"), fill = TRUE)
cat("Stacked:", nrow(lrp), "rows\n\n")

# ── 2. Column names: lowercase + trim ────────────────────────────────────────

setnames(lrp, trimws(names(lrp)))
setnames(lrp, names(lrp), tolower(names(lrp)))
cat("Columns:", paste(names(lrp), collapse = ", "), "\n\n")

# ── 3. Numeric conversions ────────────────────────────────────────────────────

# Only convert columns that actually exist
num_cols_wanted <- c("reins_year", "comm_year", "loc_state_code",
                     "loc_county_code", "endors_length", "cov_price",
                     "exp_end_value", "cov_level_pct", "rate",
                     "cost_per_cwt", "net_head", "total_weight",
                     "subsidy_amt", "total_prem_amt", "prod_prem_amt",
                     "liab_amt", "indemn_amt")
num_cols <- intersect(num_cols_wanted, names(lrp))
missing_cols <- setdiff(num_cols_wanted, names(lrp))
if (length(missing_cols) > 0)
  cat("Note — columns not found (skipped):", paste(missing_cols, collapse=", "), "\n")

lrp[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]

# ── 4. Filter to cattle only ─────────────────────────────────────────────────

lrp[, comm_name_clean := trimws(toupper(comm_name))]
lrp <- lrp[comm_name_clean %in% c("FEEDER CATTLE", "FED CATTLE")]
lrp[, cattle_type := fifelse(comm_name_clean == "FEEDER CATTLE", "feeder", "fed")]
cat("After cattle filter:", nrow(lrp), "rows\n")

# ── 5. FIPS ───────────────────────────────────────────────────────────────────

lrp[, fips := as.integer(loc_state_code) * 1000L + as.integer(loc_county_code)]
lrp <- lrp[fips < 99000]

# ── 6. Dates ──────────────────────────────────────────────────────────────────

lrp[, sales_eff_date := as.Date(sales_eff_date)]
lrp[, end_date       := as.Date(end_date)]
lrp[, year           := lubridate::year(sales_eff_date)]
lrp[, quarter        := lubridate::quarter(sales_eff_date)]
lrp[, year_quarter   := paste0(year, "Q", quarter)]
lrp[, month          := lubridate::month(sales_eff_date)]
lrp[, year_month     := paste0(year, "-", sprintf("%02d", month))]

# ── 7. Policy reform dummies ──────────────────────────────────────────────────

lrp[, post2019       := as.integer(comm_year >= 2019)]
lrp[, post2020       := as.integer(comm_year >= 2020)]
lrp[, post2020_x_cov := post2020 * cov_level_pct]

# ── 8. Derived variables ──────────────────────────────────────────────────────

lrp[total_weight > 0, subsidy_rate    := subsidy_amt    / total_weight]
lrp[total_weight > 0, premium_per_cwt := total_prem_amt / total_weight]
lrp[total_weight > 0, indemn_per_cwt  := indemn_amt     / total_weight]
lrp[net_head     > 0, subsidy_per_head := subsidy_amt   / net_head]
lrp[, indemnity_triggered := as.integer(indemn_amt > 0)]

lrp[net_head     > 0, ln_net_head  := log(net_head)]
lrp[subsidy_rate > 0, ln_subsidy   := log(subsidy_rate)]
lrp[cov_price    > 0, ln_cov_price := log(cov_price)]
lrp[total_weight > 0, ln_weight    := log(total_weight)]

# ── 9. Labels ─────────────────────────────────────────────────────────────────

lrp[, state_abbr  := trimws(loc_state_abbr)]
lrp[, county_name := trimws(loc_county_name)]
lrp[, county_state := paste0(county_name, "_", state_abbr)]

# ── 10. Save ──────────────────────────────────────────────────────────────────

dir.create(output_data, showWarnings = FALSE, recursive = TRUE)
out_path <- file.path(output_data, "lrp_county_panel.csv")
fwrite(lrp, out_path)

cat("\n=== Done ===\n")
cat("Saved:        ", out_path, "\n")
cat("rows:         ", nrow(lrp), "\n")
cat("unique FIPS:  ", uniqueN(lrp$fips), "\n")
cat("feeder rows:  ", lrp[cattle_type == "feeder", .N], "\n")
cat("fed rows:     ", lrp[cattle_type == "fed",    .N], "\n")
cat("years:        ", min(lrp$year, na.rm=TRUE), "-",
    max(lrp$year, na.rm=TRUE), "\n")

