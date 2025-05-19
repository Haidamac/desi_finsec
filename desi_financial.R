# Add the necessary libraries
library(dplyr)
library(tidyr)

# Download data from CSV files
assets_liab_data <- read.csv("Financial/estat_nasa_10_f_bs_filtered_en.csv")
trans_data <- read.csv("Financial/estat_nasa_10_nf_tr_filtered_en.csv")
desi_data <- read.csv("desi_total.csv")
code_data <- read.csv("Groups/desi_dim-Countries.csv")

# Join desi_data with code_data
desi_data <- desi_data %>%
  inner_join(code_data, by = c("country" = "code")) %>%
  mutate(value = as.numeric(value)) %>%
  mutate(period = as.numeric(period)) %>%
  mutate(TIME_PERIOD = period - 1)

# Assets data
assets_data <- assets_liab_data %>%
  filter(finpos == 'Assets',
         na_item == "Total financial assets/liabilities") %>%
  select(geo, TIME_PERIOD, OBS_VALUE) %>%
  rename(assets = OBS_VALUE) %>%
  mutate(assets = as.numeric(assets))

# Current assets data
current_assets_data <- assets_liab_data %>%
  filter(finpos == 'Assets',
         na_item %in% c("Currency and deposits", "Other accounts receivable/payable", 
                        "Short-term - loans", "Short-term debt securities")) %>%
  group_by(geo, TIME_PERIOD) %>%
  summarise(current_assets = sum(as.numeric(OBS_VALUE), na.rm = TRUE)) %>%
  ungroup()

# Cash data
quick_assets_data <- assets_liab_data %>%
  filter(finpos == 'Assets',
         na_item == "Currency and deposits") %>%
  select(geo, TIME_PERIOD, OBS_VALUE) %>%
  rename(quick_assets = OBS_VALUE) %>%
  mutate(quick_assets = as.numeric(quick_assets))

# Receivables data
receivables_data <- assets_liab_data %>%
  filter(finpos == 'Assets',
         na_item == "Other accounts receivable/payable") %>%
  select(geo, TIME_PERIOD, OBS_VALUE) %>%
  rename(receivable = OBS_VALUE) %>%
  mutate(receivable = as.numeric(receivable))
  
# Liabilities data
liabilities_data <- assets_liab_data %>%
  filter(finpos == 'Liabilities',
         na_item == "Total financial assets/liabilities") %>%
    select(geo, TIME_PERIOD, OBS_VALUE) %>%
  rename(liabilities = OBS_VALUE) %>%
  mutate(liabilities = as.numeric(liabilities))

# Current liabilities data
current_liabilities_data <- assets_liab_data %>%
  filter(finpos == 'Liabilities',
    na_item %in% c("Other accounts receivable/payable", "Short-term - loans", "Short-term debt securities")
  ) %>%
  group_by(geo, TIME_PERIOD) %>%
  summarise(current_liabilities = sum(as.numeric(OBS_VALUE), na.rm = TRUE)) %>%
  ungroup()

# Payables data
payables_data <- assets_liab_data %>%
  filter(finpos == 'Liabilities',
         na_item == "Other accounts receivable/payable") %>%
  select(geo, TIME_PERIOD, OBS_VALUE) %>%
  rename(payable = OBS_VALUE) %>%
  mutate(payable = as.numeric(payable))

# Net Profit data
profit_data <- trans_data %>%
  filter(na_item == "Net lending (+)/net borrowing (-)") %>%
  select(geo, TIME_PERIOD, OBS_VALUE) %>%
  rename(profit = OBS_VALUE) %>%
  mutate(profit = as.numeric(profit))

# Gross Operating Surplus data
surplus_data <- trans_data %>%
  filter(na_item == "Operating surplus and mixed income, gross") %>%
  select(geo, TIME_PERIOD, OBS_VALUE) %>%
  rename(surplus = OBS_VALUE) %>%
  mutate(surplus = as.numeric(surplus))

# Saving Gross data
saving_data <- trans_data %>%
  filter(na_item == "Saving, gross") %>%
  select(geo, TIME_PERIOD, OBS_VALUE) %>%
  rename(saving = OBS_VALUE) %>%
  mutate(saving = as.numeric(saving))

# Output data
output_data <- trans_data %>%
  filter(na_item == "Output") %>%
  select(geo, TIME_PERIOD, OBS_VALUE) %>%
  rename(output = OBS_VALUE) %>%
  mutate(output = as.numeric(output))

# Join data
combined_data <- assets_data %>%
  inner_join(current_assets_data, by = c("geo", "TIME_PERIOD")) %>%
  inner_join(quick_assets_data, by = c("geo", "TIME_PERIOD")) %>%
  inner_join(receivables_data, by = c("geo", "TIME_PERIOD")) %>%
  inner_join(liabilities_data, by = c("geo", "TIME_PERIOD")) %>%
  inner_join(current_liabilities_data, by = c("geo", "TIME_PERIOD")) %>%
  inner_join(payables_data, by = c("geo", "TIME_PERIOD")) %>%
  inner_join(output_data, by = c("geo", "TIME_PERIOD")) %>%
  inner_join(profit_data, by = c("geo", "TIME_PERIOD")) %>%
  inner_join(surplus_data, by = c("geo", "TIME_PERIOD")) %>%
  inner_join(saving_data, by = c("geo", "TIME_PERIOD"))

# Ratios
ratio_data <- combined_data %>%
  mutate(equity = assets - liabilities,
         debt_to_equity_ratio = liabilities / equity) %>% # Debt to Equity Ratio
  drop_na(debt_to_equity_ratio) %>%
  mutate(financial_sustainability_ratio = equity / assets) %>% # Financial Sustainability Ratio
  drop_na(financial_sustainability_ratio) %>%
  mutate(working_capital = current_assets - current_liabilities,
          working_capital_ratio = working_capital / current_assets) %>% # Working Capital Ratio
  drop_na(working_capital_ratio) %>%
  mutate(current_ratio = current_assets / current_liabilities) %>% # Current Ratio
  drop_na(current_ratio) %>%
  mutate(quick_ratio = quick_assets / current_liabilities) %>% # Equity Turnover Ratio
  drop_na(quick_ratio) %>%
  mutate(receivable_to_payable_ratio = receivable / payable) %>% # Receivables to Payables Ratio
  drop_na(receivable_to_payable_ratio) %>%
  mutate(roe = profit / equity) %>% # Return on Equity Ratio
  drop_na(roe) %>%
  mutate(roa = profit / assets) %>% # Return on Assets Ratio
  drop_na(roa)  %>%
  mutate(ros = surplus / output) %>% # Return on Sales Ratio
  drop_na(ros)  %>%
  mutate(asset_turnover_ratio = output / assets) %>% # Asset Turnover Ratio
  drop_na(asset_turnover_ratio) %>%
  mutate(equity_turnover_ratio = output / equity) %>% # Equity Turnover Ratio
  drop_na(equity_turnover_ratio) %>%
  mutate(fixed_assets = assets - current_assets,
          fixed_assets_turnover_ratio = output / fixed_assets) %>% # Fixed Asset Turnover Ratio
  drop_na(fixed_assets_turnover_ratio) %>%
  mutate(net_profit_margin = profit / output) %>% # Net Profit Margin
  drop_na(net_profit_margin) %>%
  mutate(profit_retention_ratio = saving / profit) %>% # Profit Retention Ratio
  drop_na(profit_retention_ratio)

# Integral financial security ratio
ratio_data <- ratio_data %>%
  mutate(financial_stability = 1/debt_to_equity_ratio * 0.5 + financial_sustainability_ratio * 0.15 + working_capital_ratio * 0.35,
         liquidity = current_ratio * 0.5 + quick_ratio * 0.2 + receivable_to_payable_ratio * 0.3,
         profitability = roe * 0.35 + roa * 0.45 + ros * 0.2,
         business_activity = asset_turnover_ratio * 0.5 + fixed_assets_turnover_ratio * 0.3 + equity_turnover_ratio * 0.2,
         investment_attractiveness = net_profit_margin * 0.55 + profit_retention_ratio * 0.45,
         financial_security_ratio = financial_stability * 0.26 + liquidity * 0.22 + profitability * 0.24 + business_activity * 0.18 + investment_attractiveness * 0.1)

# Adding data from DESI
desi_ratio_data <- ratio_data %>%
  inner_join(desi_data, by = c("geo" = "label", "TIME_PERIOD")) %>%
  select(geo, TIME_PERIOD,  financial_security_ratio, debt_to_equity_ratio, financial_sustainability_ratio, working_capital_ratio, roa, asset_turnover_ratio, profit_retention_ratio, value)

write.csv(desi_ratio_data, file = "desi_finsec_output.csv", row.names = FALSE)
