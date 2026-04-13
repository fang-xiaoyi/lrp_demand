source(here::here("codes", "00_config.R"))
library(data.table)
library(lubridate)

raw_path <- file_drought_raw
if (!file.exists(raw_path)) stop("File not found: ", raw_path)

raw <- fread(raw_path)
cat("Rows loaded:", nrow(raw), "\n")
cat("Columns:", paste(names(raw), collapse = ", "), "\n\n")

if ("StatisticFormatID" %in% names(raw)) {
  ids <- unique(raw$StatisticFormatID)
  cat("StatisticFormatID:", paste(ids, collapse=", "),
      if (all(ids == 2)) "— Categorical confirmed\n" else "— WARNING\n")
}

setnames(raw, "MapDate", "week_date",   skip_absent = TRUE)
setnames(raw, "FIPS",    "fips",        skip_absent = TRUE)
setnames(raw, "None",    "no_drought",  skip_absent = TRUE)
setnames(raw, "County",  "county_name", skip_absent = TRUE)
setnames(raw, "State",   "state_abbr",  skip_absent = TRUE)

raw[, week_date := as.Date(as.character(week_date), format = "%Y%m%d")]
raw[, fips      := as.integer(fips)]

d_cols <- intersect(c("no_drought","D0","D1","D2","D3","D4"), names(raw))
raw[, (d_cols) := lapply(.SD, as.numeric), .SDcols = d_cols]

raw[, drought_severe  := D2 + D3 + D4]
raw[, drought_extreme := D3 + D4]
raw[, drought_any     := D1 + D2 + D3 + D4]
raw[, year         := year(week_date)]
raw[, month        := month(week_date)]
raw[, quarter      := quarter(week_date)]
raw[, year_quarter := paste0(year, "Q", quarter)]
raw[, year_month   := paste0(year, "-", sprintf("%02d", month))]

raw[, row_total := no_drought + D0 + D1 + D2 + D3 + D4]
bad <- raw[abs(row_total - 100) > 1]
if (nrow(bad) > 0)
  cat("Note:", nrow(bad), "rows not summing to 100 (max dev:",
      round(max(abs(bad$row_total - 100)), 2), ")\n")
raw[, row_total := NULL]

keep <- c("fips","week_date","year","month","quarter",
          "year_quarter","year_month","county_name","state_abbr",
          "no_drought","D0","D1","D2","D3","D4",
          "drought_any","drought_severe","drought_extreme")
out <- raw[, ..keep]
setorder(out, fips, week_date)

cat("\n=== Processed drought data ===\n")
cat("Rows:     ", nrow(out), "\n")
cat("Counties: ", uniqueN(out$fips), "\n")
cat("Dates:    ", format(min(out$week_date)), "to", format(max(out$week_date)), "\n")
cat("\nMean drought_severe by year:\n")
print(out[, .(mean_severe = round(mean(drought_severe,na.rm=TRUE),2),
              pct_any = round(mean(drought_any>0,na.rm=TRUE)*100,1)),
          by=year][order(year)])

dir.create(input_drought, showWarnings=FALSE, recursive=TRUE)
fwrite(out, file_drought)
cat("\nSaved:", file_drought, "\n")

