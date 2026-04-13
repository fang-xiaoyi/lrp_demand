source(here::here("codes", "00_config.R"))
library(data.table); library(readxl); library(lubridate)

# ── 1. Load raw gap data ──────────────────────────────────────────────────────
zf <- as.data.table(read_xlsx(file_zf_gap))
ok <- as.data.table(read_xlsx(file_ok_gap))
zf[, futures_contract := "GF"]   # feeder futures
ok[, futures_contract := "LE"]   # live cattle futures
gap <- rbind(zf, ok, fill=TRUE)
gap[, start_Date := as.Date(start_Date)]
gap[, end_Date   := as.Date(end_Date)]
gap[, year       := year(start_Date)]
cat("Raw gap rows:", nrow(gap), "(GF:", nrow(zf), "/ LE:", nrow(ok), ")\n")

# ── 2. Save policy-level gap (unmodified) ────────────────────────────────────
# This is the panel for Model 3 (gap regression)
# Unit: one row per endorsement-option pair
gap[, ln_gap       := log(gap + 1)]
gap[, year_quarter := paste0(year, "Q", quarter(start_Date))]
gap[, post2019     := as.integer(year >= 2019)]
gap[, post2020     := as.integer(year >= 2020)]
fwrite(gap, file.path(output_data, "lrp_gap_policy_level.csv"))
cat("Saved policy-level gap:", nrow(gap), "rows\n")

# ── 3. Aggregate gap to fips x date x length ─────────────────────────────────
# For each unique fips+date+length combination, summarize option protection stats
gap_agg <- gap[, .(
  n_options        = .N,
  mean_gap         = mean(gap,          na.rm=TRUE),
  mean_lrp_protect = mean(LRP_protect,  na.rm=TRUE),
  mean_put_protect = mean(Put_protect,  na.rm=TRUE),
  mean_moneyness   = mean(int_moneyness,na.rm=TRUE),
  mean_dist_strike = mean(dist_strike,  na.rm=TRUE),
  pct_triggered    = mean(end==1,       na.rm=TRUE),
  futures_contract = first(futures_contract)
), by = .(fips, start_Date, end_Date, endors_length, lrp_coverage_level)]
cat("Aggregated gap rows:", nrow(gap_agg), "\n")

# ── 4. Load LRP SOB panel ─────────────────────────────────────────────────────
lrp <- fread(file_lrp_all)
lrp[, sales_eff_date := as.Date(sales_eff_date)]
lrp[, end_date       := as.Date(end_date)]
lrp_sub <- lrp[year >= 2020]
cat("LRP 2020+ rows:", nrow(lrp_sub), "\n\n")

# ── 5. Merge aggregated gap onto SOB ─────────────────────────────────────────
# Primary: fips + start + end + length + coverage_level
m1 <- merge(lrp_sub, gap_agg,
            by.x = c("fips","sales_eff_date","end_date",
                     "endors_length","cov_level_pct"),
            by.y = c("fips","start_Date","end_Date",
                     "endors_length","lrp_coverage_level"),
            all.x = TRUE)
n1 <- m1[!is.na(mean_gap), .N]
cat("Primary match (fips+start+end+length+cov):", n1, "/", nrow(lrp_sub),
    sprintf("(%.1f%%)\n", 100*n1/nrow(lrp_sub)))

# Fallback: fips + start + length (drop end_date and coverage)
unmatched <- m1[is.na(mean_gap)]
matched   <- m1[!is.na(mean_gap)]

gap_agg2  <- gap_agg[, .(mean_gap = mean(mean_gap, na.rm=TRUE),
                          mean_lrp_protect = mean(mean_lrp_protect, na.rm=TRUE),
                          mean_put_protect = mean(mean_put_protect, na.rm=TRUE),
                          mean_moneyness   = mean(mean_moneyness,   na.rm=TRUE),
                          pct_triggered    = mean(pct_triggered,    na.rm=TRUE),
                          futures_contract = first(futures_contract)
                         ), by=.(fips, start_Date, endors_length)]

m2 <- merge(unmatched[, !c("mean_gap","mean_lrp_protect","mean_put_protect",
                            "mean_moneyness","mean_dist_strike",
                            "pct_triggered","futures_contract","n_options")],
            gap_agg2,
            by.x = c("fips","sales_eff_date","endors_length"),
            by.y = c("fips","start_Date","endors_length"),
            all.x = TRUE)
n2 <- m2[!is.na(mean_gap), .N]
cat("Fallback match:", n2, "/", nrow(unmatched),
    sprintf("(%.1f%% of unmatched)\n", 100*n2/nrow(unmatched)))

# ── 6. Combine ────────────────────────────────────────────────────────────────
lrp_gap <- rbind(matched, m2, fill=TRUE)
n_with_gap <- lrp_gap[!is.na(mean_gap), .N]
cat("\nFinal merged:", nrow(lrp_gap), "rows\n")
cat("With gap data:", n_with_gap,
    sprintf("(%.1f%%)\n", 100*n_with_gap/nrow(lrp_gap)))
cat("Without gap:  ", lrp_gap[is.na(mean_gap), .N], "\n")

# ── 7. Summary ────────────────────────────────────────────────────────────────
cat("\n=== Gap summary (SOB-matched rows) ===\n")
cat("mean_gap:         ", round(mean(lrp_gap$mean_gap, na.rm=TRUE), 2), "$/cwt\n")
cat("mean pct_triggered:", round(mean(lrp_gap$pct_triggered, na.rm=TRUE)*100, 1), "%\n")
cat("\nBy year:\n")
print(lrp_gap[!is.na(mean_gap), .(mean_gap=round(mean(mean_gap,na.rm=TRUE),2),
              n=.N), by=year][order(year)])

# ── 8. Save ───────────────────────────────────────────────────────────────────
fwrite(lrp_gap, file.path(output_data, "lrp_gap_merged.csv"))
cat("\nSaved: lrp_gap_merged.csv\n")
cat("Columns:", paste(names(lrp_gap), collapse=", "), "\n")
