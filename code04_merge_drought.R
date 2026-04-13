source(here::here("codes", "00_config.R"))
library(data.table)
library(lubridate)

# ── 1. Load drought (weekly) ─────────────────────────────────────────────────
drought <- fread(file_drought)
drought[, week_date := as.Date(week_date)]
cat("Drought loaded:", nrow(drought), "rows,", uniqueN(drought$fips), "counties\n")

# ── 2. Aggregate to county x quarter ─────────────────────────────────────────
drought_q <- drought[, .(
  drought_severe   = mean(drought_severe,  na.rm = TRUE),
  drought_extreme  = mean(drought_extreme, na.rm = TRUE),
  drought_any      = mean(drought_any,     na.rm = TRUE),
  D2_avg           = mean(D2, na.rm = TRUE),
  D3_avg           = mean(D3, na.rm = TRUE),
  D4_avg           = mean(D4, na.rm = TRUE)
), by = .(fips, year_quarter)]
cat("Drought quarterly:", nrow(drought_q), "county-quarter rows\n\n")

# ── 3. Load demand panel (from code03) ───────────────────────────────────────
demand_path <- file.path(output_data, "county_demand_panel.csv")
if (!file.exists(demand_path))
  stop("Run code03_aggregate_county_panel.R first\nMissing: ", demand_path)

demand <- fread(demand_path)
cat("Demand panel loaded:", nrow(demand), "rows\n")

# ── 4. Merge ──────────────────────────────────────────────────────────────────
demand_d <- merge(demand, drought_q,
                  by    = c("fips", "year_quarter"),
                  all.x = TRUE)

demand_d[, drought_x_post2020 := drought_severe * post2020]
demand_d[, drought_x_post2019 := drought_severe * post2019]

n_miss <- demand_d[is.na(drought_severe), .N]
cat("Missing drought:", n_miss,
    sprintf("(%.1f%%)\n", 100 * n_miss / nrow(demand_d)))

# ── 5. Load gap panel (from code02) ──────────────────────────────────────────
gap_path <- file.path(output_data, "lrp_gap_merged.csv")
if (file.exists(gap_path)) {
  gap <- fread(gap_path)
  gap[, start_Date := as.Date(start_Date)]
  gap[, week_date  := start_Date - (as.integer(format(start_Date, "%u")) - 2) %% 7]
  gap[, year_quarter := paste0(year(start_Date), "Q", quarter(start_Date))]

  gap_d <- merge(gap,
                 drought[, .(fips, week_date, drought_severe,
                              drought_extreme, drought_any)],
                 by    = c("fips", "week_date"),
                 all.x = TRUE)

  gap_d[, drought_x_post2020 := drought_severe * post2020]

  n_miss_gap <- gap_d[is.na(drought_severe), .N]
  cat("Gap rows missing drought:", n_miss_gap,
      sprintf("(%.1f%%)\n", 100 * n_miss_gap / nrow(gap_d)))

  fwrite(gap_d, file.path(output_data, "lrp_gap_merged_drought.csv"))
  cat("Saved: lrp_gap_merged_drought.csv\n")
} else {
  cat("Note: lrp_gap_merged.csv not found — skipping gap merge\n")
  cat("Run code02_merge_gap_to_lrp.R first if needed\n")
}

# ── 6. Save ───────────────────────────────────────────────────────────────────
fwrite(demand_d, file.path(output_data, "county_demand_panel_drought.csv"))
cat("\nSaved: county_demand_panel_drought.csv\n")
cat("Rows:", nrow(demand_d), "\n")
cat("With drought data:", demand_d[!is.na(drought_severe), .N], "\n")
cat("\nReady for code05_descriptives.R and regression scripts\n")
