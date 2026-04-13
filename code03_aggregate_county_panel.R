source(here::here("codes", "00_config.R"))
library(data.table)
library(lubridate)

lrp <- fread(file_lrp_all)
lrp[, sales_eff_date := as.Date(sales_eff_date)]
cat("Loaded:", nrow(lrp), "rows\n")

panel <- lrp[, .(
  net_head_total   = sum(net_head,       na.rm=TRUE),
  total_weight_sum = sum(total_weight,   na.rm=TRUE),
  n_policies       = .N,
  subsidy_amt_sum  = sum(subsidy_amt,    na.rm=TRUE),
  prem_amt_sum     = sum(total_prem_amt, na.rm=TRUE),
  indemn_amt_sum   = sum(indemn_amt,     na.rm=TRUE),
  liab_amt_sum     = sum(liab_amt,       na.rm=TRUE),
  cov_level_avg    = mean(cov_level_pct,  na.rm=TRUE),
  cov_price_avg    = mean(cov_price,      na.rm=TRUE),
  endors_len_avg   = mean(endors_length,  na.rm=TRUE),
  post2019         = max(post2019),
  post2020         = max(post2020),
  comm_year        = max(comm_year),
  year             = first(year),
  state_abbr       = first(state_abbr),
  county_name      = first(county_name),
  county_state     = first(county_state)
), by = .(fips, year_quarter, cattle_type)]

panel[total_weight_sum > 0, subsidy_rate_wt  := subsidy_amt_sum  / total_weight_sum]
panel[liab_amt_sum     > 0, indemnity_rate   := indemn_amt_sum   / liab_amt_sum]
panel[net_head_total   > 0, ln_net_head      := log(net_head_total)]
panel[subsidy_rate_wt  > 0, ln_subsidy       := log(subsidy_rate_wt)]
panel[cov_price_avg    > 0, ln_cov_price     := log(cov_price_avg)]
panel[total_weight_sum > 0, ln_weight        := log(total_weight_sum)]
panel[, iv1 := post2019]
panel[, iv2 := post2020]
panel[, iv3 := post2020 * cov_level_avg]
panel[, state_fips := fips %/% 1000L]

cat("Panel rows:", nrow(panel), "\n")
cat("FIPS:", uniqueN(panel$fips), "\n")
cat("Feeder:", panel[cattle_type=="feeder",.N], "| Fed:", panel[cattle_type=="fed",.N], "\n")

fwrite(panel, file.path(output_data, "county_demand_panel.csv"))
cat("Saved: county_demand_panel.csv\n")
