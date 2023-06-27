##Cleaning Script

#libraries
library(tidyverse)
library(readxl)
library(janitor)

#data
worldwide_exports <- read_csv("raw_data/worldwide_exports.csv")

worldwide_expenses <- read_csv("raw_data/worldwide_expense.csv")

population_worldwide <- read_csv("raw_data/worldwide_populations.csv")

foreign_direct_investment <- read_csv("raw_data/foreign_investments.csv")

exchange_rates <- read_csv("raw_data/exchange_rates.csv")

oil_revenue <- read_csv("raw_data/oil_rents.csv")

labour_force <- read_csv("raw_data/labour_force.csv")

unemployment_rate <- read_csv("raw_data/unemploment_rate.csv")

poverty_headcount_ratio <- read_csv("raw_data/poverty_headcount_ratio.csv")

gdp_worldwide <- read_csv("raw_data/worldwide_gdp.csv")

gdp_growth_worldwide <- read_csv("raw_data/gdp_growth_worldwide.csv")

gdp_per_capita <- read_csv("raw_data/gdp_per_capita.csv")

productivity_gdp_hour_worked <- read_csv("raw_data/gdp_hour_worked_productivity.csv")

productivity_multifactor <- read_csv("raw_data/multfactor_productivity.csv")


#cleaning function from world bank data
clean_world_bank_data <- function(db){
  
  db %>% 
    filter(row_number() != c(1:3)) %>% 
    row_to_names(row_number = 1) %>% 
    clean_names() %>% 
    select(-indicator_code) %>% 
    mutate(across(c(x1960:x2022), .fns = as.numeric)) %>% 
    mutate(developed = if_else(
      country_name %in% c("Australia", "Austria", "Belgium", "Canada","Denmark", 
                          "Finland", "France", "Germany", "Hong Kong", 
                          "Ireland", "Israel", "Italy", "Japan", "Luxembourg", 
                          "Netherlands", "New Zealand", "Norway", "Poland", "Portugal",
                          "Singapore", "South Korea", "Spain", "Sweden", "Switzerland",
                          "United Kingdom", "United States"), TRUE, FALSE),
      united_kingdom = if_else(country_name %in% "United Kingdom", TRUE, FALSE),
      .after = country_code) %>% 
    pivot_longer(cols = x1960:x2022, names_to = "year", values_to = "kpi_units") %>% 
    mutate(year = str_remove(year, "x"))
  
}

# expenses cleaning
expenses_clean <- clean_world_bank_data(worldwide_expenses) %>% 
  rename(expense_perc_gdp = kpi_units) %>% 
  select(-indicator_name)

# exports cleaning
exports_clean <- clean_world_bank_data(worldwide_exports) %>% 
  rename(exports_usd = kpi_units) %>% 
  select(-indicator_name)

# oil revenue cleaning
oil_revenue_clean <- clean_world_bank_data(oil_revenue) %>% 
  rename(oil_rents_perc_gdp = kpi_units) %>% 
  select(-indicator_name)

# population cleaning
population_clean <- clean_world_bank_data(population_worldwide) %>% 
  rename(total_population = kpi_units) %>% 
  select(-indicator_name)

# fdi cleanining
foreign_investments_clean <- clean_world_bank_data(foreign_direct_investment) %>% 
  rename(foreign_investment_usd = kpi_units) %>% 
  select(-indicator_name)

# exchange rates cleaning
exchange_rates_clean <- clean_world_bank_data(exchange_rates) %>% 
  rename(exchange_rate_lcu_usd = kpi_units) %>% 
  select(-indicator_name)

# labour force cleaning 
labour_force_clean <- clean_world_bank_data(labour_force) %>% 
  rename(labour_force_total = kpi_units) %>% 
  select(-indicator_name)

# poverty_headcount_ratio cleaning
poverty_headcount_ratio_clean <- clean_world_bank_data(poverty_headcount_ratio) %>% 
  rename(poverty_headcount_ratio_perc_pop = kpi_units) %>% 
  select(-indicator_name)

# unemployment rate cleaning
unemployment_rate_clean <- clean_world_bank_data(unemployment_rate) %>% 
  rename(unemployment_perc_labour_force_total = kpi_units) %>% 
  select(-indicator_name)

# gdp world wide cleaning
gdp_clean <- clean_world_bank_data(gdp_worldwide) %>% 
  rename(gdp_usd = kpi_units) %>% 
  select(-indicator_name)

# gdp growth worldwide clean 
gdp_growth_clean <- clean_world_bank_data(gdp_growth_worldwide) %>% 
  rename(annual_gdp_growth = kpi_units) %>% 
  select(-indicator_name)

# gdp per capita clean
gdp_per_capita_clean <- clean_world_bank_data(gdp_per_capita) %>% 
  rename(gdp_per_capita_usd = kpi_units) %>% 
  select(-indicator_name)

# productivity_gdp_hour_worked cleaning
productivity_gdp_hour_worked_clean <- productivity_gdp_hour_worked %>% 
  rename(country_code = LOCATION,
         gdp_hr_wrkd_usd = Value,
         year = TIME) %>% 
  clean_names() %>% 
  filter(measure == "USD") %>%
  select(country_code, year, gdp_hr_wrkd_usd) %>% 
  mutate(year = as.character(year))

# multifactor productivity cleaning
productivity_multifactor_clean <- productivity_multifactor %>% 
  rename(country_code = LOCATION,
         mfp_annual_growth_rate_res = Value,
         year = TIME) %>% 
  clean_names() %>% 
  filter(measure == "AGRWTH") %>% 
  select(country_code, year, mfp_annual_growth_rate_res) %>% 
  mutate(year = as.character(year))

  

#joining all world bank data together
all_kpis_worldwide_1 <- full_join(expenses_clean, exports_clean) 
all_kpis_worldwide_2 <- full_join(all_kpis_worldwide_1, oil_revenue_clean)
all_kpis_worldwide_3 <- full_join(all_kpis_worldwide_2, population_clean)
all_kpis_worldwide_4 <- full_join(all_kpis_worldwide_3, foreign_investments_clean)
all_kpis_worldwide_5 <- full_join(all_kpis_worldwide_4, exchange_rates_clean)
all_kpis_worldwide_6 <- full_join(all_kpis_worldwide_5, labour_force_clean)
all_kpis_worldwide_7 <- full_join(all_kpis_worldwide_6, poverty_headcount_ratio_clean)
all_kpis_worldwide_8 <- full_join(all_kpis_worldwide_7, unemployment_rate_clean)
all_kpis_worldwide_9 <- full_join(all_kpis_worldwide_8, gdp_clean)
all_kpis_worldwide_10 <- full_join(all_kpis_worldwide_9, gdp_per_capita_clean)
all_kpis_worldwide_11 <- full_join(all_kpis_worldwide_10, gdp_growth_clean) 
productivity_kpis_1 <- left_join(all_kpis_worldwide_11, productivity_gdp_hour_worked_clean, by = join_by(country_code, year))
all_kpis_worldwide_final <- left_join(productivity_kpis_1, productivity_multifactor_clean, by = join_by(country_code, year)) %>% 
  relocate(c(gdp_usd, annual_gdp_growth, total_population), .after = year) %>% 
  mutate(year = as.numeric(year),
         date = make_datetime(year), .after = year)


# writing csv files
write_csv(all_kpis_worldwide_final, "clean_data/deloitte_gdp_kpis.csv")


