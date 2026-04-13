source(here::here("codes", "00_config.R"))
library(data.table)
library(lubridate)

# ── 诊断 ─────────────────────────────────────────────────────────────────────
cat("input_lrp_annual:", input_lrp_annual, "\n")
cat("Folder exists:", dir.exists(input_lrp_annual), "\n")
files <- list.files(input_lrp_annual, pattern="lrp_\\d{4}\\.csv", full.names=TRUE)
cat("Files found:", length(files), "\n\n")
if (length(files) == 0) stop("No files found in ", input_lrp_annual,
  "\nCopy lrp_2005.csv ... lrp_2026.csv into data/raw/lrp_annual/")

# ── Stack ────────────────────────────────────────────────────────────────────
lrp <- rbindlist(lapply(files, fread, colClasses="character"), fill=TRUE)
cat("Stacked:", nrow(lrp), "rows\n")

setnames(lrp, trimws(names(lrp)))
setnames(lrp, names(lrp), tolower(names(lrp)))

num_cols_want <- c("reins_year","comm_year","loc_state_code","loc_county_code",
                   "endors_length","cov_price","exp_end_value","cov_level_pct",
                   "rate","cost_per_cwt","net_head","total_weight",
                   "subsidy_amt","total_prem_amt","prod_prem_amt","liab_amt","indemn_amt")
num_cols <- intersect(num_cols_want, names(lrp))
lrp[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]

# ── Filter to cattle ─────────────────────────────────────────────────────────
lrp[, comm_name_clean := trimws(toupper(comm_name))]
lrp <- lrp[comm_name_clean %in% c("FEEDER CATTLE","FED CATTLE")]
lrp[, cattle_type := fifelse(comm_name_clean=="FEEDER CATTLE","feeder","fed")]
cat("After cattle filter:", nrow(lrp), "rows\n")

# ── FIPS ──────────────────────────────────────────────────────────────────────
lrp[, fips := as.integer(loc_state_code)*1000L + as.integer(loc_county_code)]
lrp <- lrp[fips < 99000]

# ── Dates ─────────────────────────────────────────────────────────────────────
lrp[, sales_eff_date := as.Date(sales_eff_date)]
lrp[, end_date       := as.Date(end_date)]
lrp[, year           := year(sales_eff_date)]
lrp[, quarter        := quarter(sales_eff_date)]
lrp[, year_quarter   := paste0(year,"Q",quarter)]
lrp[, month          := month(sales_eff_date)]
lrp[, year_month     := paste0(year,"-",sprintf("%02d",month))]

# ── Policy dummies ────────────────────────────────────────────────────────────
lrp[, post2019       := as.integer(comm_year >= 2019)]
lrp[, post2020       := as.integer(comm_year >= 2020)]
lrp[, post2020_x_cov := post2020 * cov_level_pct]

# ── Derived vars ─────────────────────────────────────────────────────────────
lrp[total_weight > 0, subsidy_rate    := subsidy_amt    / total_weight]
lrp[total_weight > 0, premium_per_cwt := total_prem_amt / total_weight]
lrp[total_weight > 0, indemn_per_cwt  := indemn_amt     / total_weight]
lrp[net_head     > 0, subsidy_per_head := subsidy_amt   / net_head]
lrp[, indemnity_triggered := as.integer(indemn_amt > 0)]
lrp[net_head     > 0, ln_net_head  := log(net_head)]
lrp[subsidy_rate > 0, ln_subsidy   := log(subsidy_rate)]
lrp[cov_price    > 0, ln_cov_price := log(cov_price)]
lrp[total_weight > 0, ln_weight    := log(total_weight)]
lrp[, state_abbr   := trimws(loc_state_abbr)]
lrp[, county_name  := trimws(loc_county_name)]
lrp[, county_state := paste0(county_name,"_",state_abbr)]

# ── Save ──────────────────────────────────────────────────────────────────────
dir.create(output_data, showWarnings=FALSE, recursive=TRUE)
fwrite(lrp, file_lrp_all)
cat("\nSaved:", file_lrp_all, "\n")
cat("rows:", nrow(lrp), "| FIPS:", uniqueN(lrp$fips),
    "| years:", min(lrp$year,na.rm=TRUE),"-",max(lrp$year,na.rm=TRUE),"\n")
