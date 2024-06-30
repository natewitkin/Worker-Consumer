## Effort Specialization Analysis

## load programs
pacman::p_load(rio, tidyverse, stringr, haven, reshape2, grid, gridExtra, dplyr, ggplot2, epiDisplay, readxl, multcomp, plm, utils, openxlsx)

# ---- Load and Clean Data ----

# impart SUSB data from the Census Bureau
# these files are web pages with the data, saved as text files
# See https://www.census.gov/programs-surveys/susb/data/datasets.All.List_1268938149.html#list-tab-List_1268938149

states_1997 <- read.delim("Data/NAICS_data/naics_1997.txt", header = TRUE, sep = ",", dec = ".")
states_1998 <- read.delim("Data/NAICS_data/naics_1998.txt", header = TRUE, sep = ",", dec = ".")
states_1999 <- read.delim("Data/NAICS_data/naics_1999.txt", header = TRUE, sep = ",", dec = ".")
states_2000 <- read.delim("Data/NAICS_data/naics_2000.txt", header = TRUE, sep = ",", dec = ".")
states_2001 <- read.delim("Data/NAICS_data/naics_2001.txt", header = TRUE, sep = ",", dec = ".")
states_2002 <- read.delim("Data/NAICS_data/naics_2002.txt", header = TRUE, sep = ",", dec = ".")
states_2003 <- read.delim("Data/NAICS_data/naics_2003.txt", header = TRUE, sep = ",", dec = ".")
states_2004 <- read.delim("Data/NAICS_data/naics_2004.txt", header = TRUE, sep = ",", dec = ".")
states_2005 <- read.delim("Data/NAICS_data/naics_2005.txt", header = TRUE, sep = ",", dec = ".")
states_2006 <- read.delim("Data/NAICS_data/naics_2006.txt", header = TRUE, sep = ",", dec = ".")
states_2007 <- read.delim("Data/NAICS_data/naics_2007.txt", header = TRUE, sep = ",", dec = ".")
states_2008 <- read.delim("Data/NAICS_data/naics_2008.txt", header = TRUE, sep = ",", dec = ".")
states_2009 <- read.delim("Data/NAICS_data/naics_2009.txt", header = TRUE, sep = ",", dec = ".")
states_2010 <- read.delim("Data/NAICS_data/naics_2010.txt", header = TRUE, sep = ",", dec = ".")
states_2011 <- read.delim("Data/NAICS_data/naics_2011.txt", header = TRUE, sep = ",", dec = ".")
states_2012 <- read.delim("Data/NAICS_data/naics_2012.txt", header = TRUE, sep = ",", dec = ".")
states_2013 <- read.delim("Data/NAICS_data/naics_2013.txt", header = TRUE, sep = ",", dec = ".")
states_2014 <- read.delim("Data/NAICS_data/naics_2014.txt", header = TRUE, sep = ",", dec = ".")
states_2015 <- read.delim("Data/NAICS_data/naics_2015.txt", header = TRUE, sep = ",", dec = ".")
states_2016 <- read.delim("Data/NAICS_data/naics_2016.txt", header = TRUE, sep = ",", dec = ".")
states_2017 <- read.delim("Data/NAICS_data/naics_2017.txt", header = TRUE, sep = ",", dec = ".")
states_2018 <- read.delim("Data/NAICS_data/naics_2018.txt", header = TRUE, sep = ",", dec = ".")
states_2019 <- read.delim("Data/NAICS_data/naics_2019.txt", header = TRUE, sep = ",", dec = ".")
states_2020 <- read.delim("Data/NAICS_data/naics_2020.txt", header = TRUE, sep = ",", dec = ".")
states_2021 <- read.delim("Data/NAICS_data/naics_2021.txt", header = TRUE, sep = ",", dec = ".")


# import business size amounts per state
# loops are divided into groups of years based on how groups of business sizes are labeled
# this allows the size variables to be identified and then labeled with consistent names
# the output of the loops are dataframes with the number of businesses of size categories for each state

only_1997 <- c("states_1997")

thru01_years <- c("states_1998", "states_1999", "states_2000",
                 "states_2001")

thru04_years <- c("states_2002", "states_2003", "states_2004")

thru06_years <- c("states_2005", "states_2006")

thru12_years <- c("states_2007", "states_2008", 
                "states_2009", "states_2010", "states_2011", "states_2012")

thru16_years <- c("states_2013", "states_2014", "states_2015", "states_2016")

thru18_years <- c("states_2017", "states_2018")

thru18_years <- c("states_2017", "states_2018")

thru21_years <- c("states_2019", "states_2020", "states_2021")

for(i in only_1997){
  dat <- get(i)
  
  a <- dat %>%
    filter(ENTRSIZE == 1 | ENTRSIZE == 2 | ENTRSIZE == 3 | ENTRSIZE == 4 | 
             ENTRSIZE == 5 | ENTRSIZE == 9 | ENTRSIZE == 26 | ENTRSIZE == 28) %>%
    filter(SICDSCR != "TOTAL OVER ALL INDUSTRIES") %>%
    mutate(ENTRSIZEDSCR = ifelse(ENTRSIZEDSCR == "0 employees", "1-4 employees", as.character(ENTRSIZEDSCR)),
           ENTRSIZE = ifelse(ENTRSIZE == 2, 3, ENTRSIZE),
           ENTRSIZEDSCR = factor(ENTRSIZEDSCR,
                                 levels = c("Total", "1-4 employees", "5-9 employees",
                                            "10-19 employees", "20-99 employees",
                                            "100-499 employees", "500 + employees"),
                                 labels = c("Total", "<5 employees", "5-9 employees",
                                            "10-19 employees", "20-99 employees",
                                            "100-499 employees", "500+ employees")))
  
  assign(i, a)
  
}

for(i in thru01_years){
  dat <- get(i)
  
  a <- dat %>%
    filter(ENTRSIZE == 1 | ENTRSIZE == 2 | ENTRSIZE == 3 | ENTRSIZE == 4 | 
             ENTRSIZE == 5 | ENTRSIZE == 9 | ENTRSIZE == 26 | ENTRSIZE == 28) %>%
    filter(NAICSDSCR != "Total") %>%
    mutate(ENTRSIZEDSCR = ifelse(ENTRSIZEDSCR == "0 employees", "1-4 employees", as.character(ENTRSIZEDSCR)),
           ENTRSIZE = ifelse(ENTRSIZE == 2, 3, ENTRSIZE),
           ENTRSIZEDSCR = factor(ENTRSIZEDSCR,
                                 levels = c("Total", "1-4 employees", "5-9 employees",
                                            "10-19 employees", "20-99 employees",
                                            "100-499 employees", "500 + employees"),
                                 labels = c("Total", "<5 employees", "5-9 employees",
                                            "10-19 employees", "20-99 employees",
                                            "100-499 employees", "500+ employees")))
  
  assign(i, a)
  
}

for(i in thru04_years){
  dat <- get(i)
  
  a <- dat %>%
    filter(ENTRSIZE == 1 | ENTRSIZE == 2 | ENTRSIZE == 3 | ENTRSIZE == 4 | ENTRSIZE == 5 | 
             ENTRSIZE == 7 | ENTRSIZE == 8 | ENTRSIZE == 10) %>%
    filter(NAICSDSCR != "Total") %>%
    mutate(ENTRSIZEDSCR = ifelse(ENTRSIZEDSCR == "0 employees", "1-4 employees", as.character(ENTRSIZEDSCR)),
           ENTRSIZE = ifelse(ENTRSIZE == 2, 3, ENTRSIZE),
           ENTRSIZEDSCR = factor(ENTRSIZEDSCR,
                                 levels = c("Total", "1-4 employees", "5-9 employees",
                                            "10-19 employees", "20-99 employees",
                                            "100-499 employees", "500 + employees"),
                                 labels = c("Total", "<5 employees", "5-9 employees",
                                            "10-19 employees", "20-99 employees",
                                            "100-499 employees", "500+ employees")))
  
  assign(i, a)
  
}

for(i in thru06_years){
  dat <- get(i)
  
  a <- dat %>%
    filter(ENTRSIZE == 1 | ENTRSIZE == 2 | ENTRSIZE == 3 | ENTRSIZE == 4 | 
             ENTRSIZE == 6 | ENTRSIZE == 7 | ENTRSIZE == 9) %>%
    filter(NAICSDSCR != "Total") %>%
    mutate(ENTRSIZEDSCR = factor(ENTRSIZEDSCR,
                                 levels = c("Total", "0-4 employees", "5-9 employees",
                                            "10-19 employees", "20-99 employees",
                                            "100-499 employees", "500 + employees"),
                                 labels = c("Total", "<5 employees", "5-9 employees",
                                            "10-19 employees", "20-99 employees",
                                            "100-499 employees", "500+ employees")))
  
  assign(i, a)
  
}

for(i in thru12_years){
  dat <- get(i)
  
  a <- dat %>%
    filter(ENTRSIZE == 1 | ENTRSIZE == 2 | ENTRSIZE == 3 | ENTRSIZE == 4 | 
             ENTRSIZE == 6 | ENTRSIZE == 7 | ENTRSIZE == 9) %>%
    filter(NAICSDSCR != "Total") %>%
    mutate(ENTRSIZEDSCR = factor(ENTRSIZEDSCR,
                                 levels = c("Total", "0-4", "5-9",
                                            "10-19", "20-99",
                                            "100-499", "500+"),
                                 labels = c("Total", "<5 employees", "5-9 employees",
                                            "10-19 employees", "20-99 employees",
                                            "100-499 employees", "500+ employees")))
  
  assign(i, a)
  
}

for(i in thru16_years){
  dat <- get(i)
  
  a <- dat %>%
    filter(ENTRSIZE == 1 | ENTRSIZE == 2 | ENTRSIZE == 3 | ENTRSIZE == 4 | 
             ENTRSIZE == 6 | ENTRSIZE == 7 | ENTRSIZE == 9) %>%
    filter(NAICSDSCR != "Total") %>%
    mutate(ENTRSIZEDSCR = factor(ENTRSIZEDSCR,
                                 levels = c("01:  Total", "02:  0-4", "03:  5-9",
                                            "04:  10-19", "06:  20-99",
                                            "07:  100-499", "09:  500+"),
                                 labels = c("Total", "<5 employees", "5-9 employees",
                                            "10-19 employees", "20-99 employees",
                                            "100-499 employees", "500+ employees")))
  
  assign(i, a)
  
}

for(i in thru18_years){
  dat <- get(i)
  
  a <- dat %>%
    filter(ENTRSIZE == 1 | ENTRSIZE == 2 | ENTRSIZE == 3 | ENTRSIZE == 4 | 
             ENTRSIZE == 6 | ENTRSIZE == 7 | ENTRSIZE == 9) %>%
    filter(NAICSDSCR != "Total") %>%
    mutate(ENTRSIZEDSCR = factor(ENTRSIZEDSCR,
                                 levels = c("01: Total", "02: <5", "03: 5-9",
                                            "04: 10-19", "06: 20-99",
                                            "07: 100-499", "09: 500+"),
                                 labels = c("Total", "<5 employees", "5-9 employees",
                                            "10-19 employees", "20-99 employees",
                                            "100-499 employees", "500+ employees")))
  
  assign(i, a)
  
}

for(i in thru21_years){
  dat <- get(i)
  
  a <- dat %>%
    filter(ENTRSIZE == 1 | ENTRSIZE == 2 | ENTRSIZE == 3 | ENTRSIZE == 26 | 
             ENTRSIZE == 34 | ENTRSIZE == 35 | ENTRSIZE == 36) %>%
    filter(NAICSDSCR != "Total") %>%
    mutate(ENTRSIZEDSCR = factor(ENTRSIZEDSCR,
                                 levels = c("01: Total", "02: <5", "03: 5-9",
                                            "04: 10-19", "06: 20-99",
                                            "07: 100-499", "09: 500+"),
                                 labels = c("Total", "<5 employees", "5-9 employees",
                                            "10-19 employees", "20-99 employees",
                                            "100-499 employees", "500+ employees")))
  
  assign(i, a)
  
}


# The following loop then takes the state/year dataframes, groups by state and firm size,
# counts the number of firms in each size category, and then calculates the percentage of
# businesses in each size category in each state

firm_data <- data.frame(
  STATEDSCR = NA,
  ENTRSIZEDSCR = NA,
  firms = NA,
  firm_totals = NA,
  firm_perc = NA,
  year = NA
)

state_years <- c("states_1997", "states_1998", "states_1999", "states_2000",
                 "states_2001", "states_2002", "states_2003", "states_2004",
                 "states_2005", "states_2006", "states_2007", "states_2008",
                 "states_2009", "states_2010", "states_2011", "states_2012",
                 "states_2013", "states_2014", "states_2015", "states_2016",
                 "states_2017", "states_2018", "states_2019", "states_2020",
                 "states_2021")

for(i in state_years){
  dat <- get(i)
  years <- as.numeric(substr(i, 8, 11))
  
  biz_data1 <- dat %>%
    filter(STATE != 0) %>%
    filter(ENTRSIZEDSCR != "Total") %>%
    group_by(STATEDSCR, ENTRSIZEDSCR) %>%
    summarize(firms = sum(FIRM)) %>%
    ungroup()
  
  biz_data2 <- dat %>%
    filter(STATE != 0) %>%
    filter(ENTRSIZEDSCR != "Total") %>%
    group_by(STATEDSCR) %>%
    summarize(firm_totals = sum(FIRM)) %>%
    ungroup()

  biz_data3 <- merge(biz_data1, biz_data2, by="STATEDSCR") %>%
    mutate(firm_perc = firms/firm_totals*100,
           year = years) %>%
    filter(ENTRSIZEDSCR != "Total")

  firm_data <- rbind.data.frame(firm_data, biz_data3)
  
}

firm_data <- firm_data %>% filter(!is.na(STATEDSCR))

#View(firm_data)

firm_data2 <- firm_data %>%
  dplyr::select(STATEDSCR, ENTRSIZEDSCR, firm_perc, year) %>%
  mutate(ENTRSIZEDSCR = factor(ENTRSIZEDSCR,
                               levels = c("Total", "<5 employees", "5-9 employees",
                                          "10-19 employees", "20-99 employees",
                                          "100-499 employees", "500+ employees"),
                               labels = c("Total", "<5 employees", "5-9 employees",
                                          "10-19 employees", "20-99 employees",
                                          "100-499 employees", "500+ employees"))) %>%
  spread(ENTRSIZEDSCR, firm_perc)


# import GDP data downloaded from FRED
# These are the "NGSP" series, where the first two letters of each csv are the 
# state abbreviation and the last four letters are "NGSP"



state_files <- c("Data/State_GDP/AKNGSP.csv", "Data/State_GDP/ALNGSP.csv", 
                 "Data/State_GDP/ARNGSP.csv", "Data/State_GDP/AZNGSP.csv",
                 "Data/State_GDP/CANGSP.csv", "Data/State_GDP/CONGSP.csv", 
                 "Data/State_GDP/CTNGSP.csv", "Data/State_GDP/DCNGSP.csv",
                 "Data/State_GDP/DENGSP.csv", "Data/State_GDP/FLNGSP.csv", 
                 "Data/State_GDP/GANGSP.csv", "Data/State_GDP/HINGSP.csv",
                 "Data/State_GDP/IANGSP.csv", "Data/State_GDP/IDNGSP.csv", 
                 "Data/State_GDP/ILNGSP.csv", "Data/State_GDP/INNGSP.csv",
                 "Data/State_GDP/KSNGSP.csv", "Data/State_GDP/KYNGSP.csv", 
                 "Data/State_GDP/LANGSP.csv", "Data/State_GDP/MANGSP.csv",
                 "Data/State_GDP/MDNGSP.csv", "Data/State_GDP/MENGSP.csv", 
                 "Data/State_GDP/MINGSP.csv", "Data/State_GDP/MNNGSP.csv",
                 "Data/State_GDP/MONGSP.csv", "Data/State_GDP/MSNGSP.csv", 
                 "Data/State_GDP/MTNGSP.csv", "Data/State_GDP/NCNGSP.csv",
                 "Data/State_GDP/NDNGSP.csv", "Data/State_GDP/NENGSP.csv", 
                 "Data/State_GDP/NHNGSP.csv", "Data/State_GDP/NJNGSP.csv",
                 "Data/State_GDP/NMNGSP.csv", "Data/State_GDP/NVNGSP.csv", 
                 "Data/State_GDP/NYNGSP.csv", "Data/State_GDP/OHNGSP.csv",
                 "Data/State_GDP/OKNGSP.csv", "Data/State_GDP/ORNGSP.csv", 
                 "Data/State_GDP/PANGSP.csv", "Data/State_GDP/RINGSP.csv",
                 "Data/State_GDP/SCNGSP.csv", "Data/State_GDP/SDNGSP.csv", 
                 "Data/State_GDP/TNNGSP.csv", "Data/State_GDP/TXNGSP.csv",
                 "Data/State_GDP/UTNGSP.csv", "Data/State_GDP/VANGSP.csv", 
                 "Data/State_GDP/VTNGSP.csv", "Data/State_GDP/WANGSP.csv",
                 "Data/State_GDP/WINGSP.csv", "Data/State_GDP/WVNGSP.csv", 
                 "Data/State_GDP/WYNGSP.csv")

state_names <- c("Alaska", "Alabama", "Arkansas", "Arizona",
                 "California", "Colorado", "Connecticut", "District of Columbia",
                 "Delaware", "Florida", "Georgia", "Hawaii",
                 "Iowa", "Idaho", "Illinois", "Indiana",
                 "Kansas", "Kentucky", "Louisiana", "Massachusetts",
                 "Maryland", "Maine", "Michigan", "Minnesota",
                 "Missouri", "Mississippi", "Montana", "North Carolina",
                 "North Dakota", "Nebraska", "New Hampshire", "New Jersey",
                 "New Mexico", "Nevada", "New York", "Ohio",
                 "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island",
                 "South Carolina", "South Dakota", "Tennessee", "Texas",
                 "Utah", "Virginia", "Vermont", "Washington",
                 "Wisconsin", "West Virginia", "Wyoming")


gdp_data_1 <- data.frame(
  year = NA,
  GDP = NA,
  STATEDSCR = NA,
  year_diff = NA,
  perc_diff_prime = NA,
  perc_diff = NA)

for(i in 1:length(state_files)) {
  dat <- 
         import(state_files[i]) %>%
           rename("GDP" = substr(state_files[i], 16, 21),
                  "year" = "DATE") %>%
           mutate(STATEDSCR = state_names[i],
                  year = substr(year, 1, 4),
                  year_diff = ifelse(year == 2023, NA, diff(GDP, lag = 1)),
                  perc_diff_prime = ifelse(year == 2023, NA, (year_diff/GDP)*100),
                  perc_diff = dplyr::lag(perc_diff_prime))
  
  gdp_data_1 <- rbind.data.frame(gdp_data_1, dat)
  
}

# adjustments for inflation

# Using CPI data downloaded from the Federal Reserve Economic Data
# Averaging CPI for each year in comparing CPI across years

inflation_data <- read.csv("Data/CPIAUCSL.csv")

inflation <- inflation_data %>%
  mutate(year = as.numeric(substr(inflation_data$DATE, 1, 4))) %>%
  group_by(year) %>%
  summarize(CPI_year = mean(CPIAUCSL)) %>%
  ungroup()

inflation_2019 <- inflation %>%
  filter(year < 2020) %>%
  mutate(maxyear = max(CPI_year),
         infl_2019 = maxyear/CPI_year)

inflation_2021 <- inflation %>%
  filter(year < 2022) %>%
  mutate(maxyear = max(CPI_year),
         infl_2019 = maxyear/CPI_year)

gdp_data_to2019 <- merge(gdp_data_1, inflation_2019, by = "year") %>%
  arrange(STATEDSCR) %>%
  mutate(realGDP = GDP * infl_2019,
         year_realdiff = ifelse(year == 2019, NA, diff(realGDP, lag = 1)),
         perc_realdiff_prime = ifelse(year == 2019, NA, (year_realdiff/realGDP)*100),
         perc_realdiff = dplyr::lag(perc_realdiff_prime))

gdp_data_to2021 <- merge(gdp_data_1, inflation_2021, by = "year") %>%
  arrange(STATEDSCR) %>%
  mutate(realGDP = GDP * infl_2019,
         year_realdiff = ifelse(year == 2021, NA, diff(realGDP, lag = 1)),
         perc_realdiff_prime = ifelse(year == 2021, NA, (year_realdiff/realGDP)*100),
         perc_realdiff = dplyr::lag(perc_realdiff_prime))


# create effort_data dataframe by merging firm data and GDP data

effort_data_to2019 <- merge(firm_data2, gdp_data_to2019, by = c("STATEDSCR", "year")) %>%
  rename("less_than5" = "<5 employees",
         "five_to9" = "5-9 employees",
         "ten_to19" = "10-19 employees",
         "twenty_to99" = "20-99 employees",
         "hundred_to499" = "100-499 employees",
         "more_than500" = "500+ employees") %>%
  group_by(STATEDSCR) %>%
  mutate(year_fact = factor(year, ordered = T)) %>%
  ungroup()

effort_data_to2021 <- merge(firm_data2, gdp_data_to2021, by = c("STATEDSCR", "year")) %>%
  rename("less_than5" = "<5 employees",
         "five_to9" = "5-9 employees",
         "ten_to19" = "10-19 employees",
         "twenty_to99" = "20-99 employees",
         "hundred_to499" = "100-499 employees",
         "more_than500" = "500+ employees") %>%
  group_by(STATEDSCR) %>%
  mutate(year_fact = factor(year, ordered = T)) %>%
  ungroup()



# ---- Main Analysis to 2019 no inflation ----

options(scipen = 999)

less_than5 <- plm(perc_diff ~ less_than5,
                  data = effort_data_to2019, model = "within", index = c("STATEDSCR", "year"))

five_to9 <- plm(perc_diff ~ five_to9,
                data = effort_data_to2019, model = "within", index = c("STATEDSCR", "year"))

ten_to19 <- plm(perc_diff ~ ten_to19,
                data = effort_data_to2019, model = "within", index = c("STATEDSCR", "year"))

twenty_to99 <- plm(perc_diff ~ twenty_to99,
                   data = effort_data_to2019, model = "within", index = c("STATEDSCR", "year"))

hundred_to499 <- plm(perc_diff ~ hundred_to499,
                     data = effort_data_to2019, model = "within", index = c("STATEDSCR", "year"))

more_than500 <- plm(perc_diff ~ more_than500,
                    data = effort_data_to2019, model = "within", index = c("STATEDSCR", "year"))


fe_results <- c("less_than5", "five_to9", "ten_to19", "twenty_to99", "hundred_to499", "more_than500")

for(i in fe_results){
  
  assign(i, as.data.frame(coef(summary(get(i)))) %>%
           mutate(variable = rownames(.)) %>%
           dplyr::select(variable, "Estimate", "Std. Error", "Pr(>|t|)") %>%
           rename(p_est = "Pr(>|t|)") %>%
           mutate(rsq = round(r.squared(get(i)), digits = 4)) %>%
           pivot_longer(cols = c("Estimate", "Std. Error", "p_est", "rsq"), 
                        names_to = "statistic", values_to = "number") %>%
           mutate(number = as.numeric(number),
                  number = ifelse(statistic == "Estimate", round(number, digits = 2), number),
                  number = ifelse(statistic == "Std. Error", paste0("(", round(number, digits = 2), ") "), number),
           ) %>%
           pivot_wider(names_from = statistic, values_from = number) %>% 
           mutate(p_est = as.numeric(p_est),
                  p_value = case_when(p_est < 0.001 ~ "***",
                                      p_est < 0.01 & p_est >= 0.001 ~ "**",
                                      p_est < 0.05 & p_est >= 0.01 ~ "*",
                                      p_est < 0.1 & p_est >= 0.05 ~ "`",
                                      p_est > 0.1 ~ ""),
                  Estimate = paste0(Estimate, p_value)) %>%
           pivot_longer(cols = c("Estimate", "Std. Error", "rsq"), 
                        names_to = "statistic", values_to = "number") %>%
           add_row(variable = NA, number = i, .before = 1) %>%
           dplyr::select(variable, number))
  
  
  
}


table <- cbind.data.frame(less_than5, five_to9, ten_to19, twenty_to99, hundred_to499, more_than500)[,c(2, 4, 6, 8, 10, 12)]
table_header <- table
colnames(table_header) <- table[1,]
table_numbers <- table_header[-1,]

variables <- c("Percentage Change in GDP", NA, "R-squared")

table1 <- cbind(variables, table_numbers)

write.xlsx(table1, "FE_to2019.xlsx")


# ---- Main Analysis to 2021 no inflation ----

options(scipen = 999)

less_than5 <- plm(perc_diff ~ less_than5,
                  data = effort_data_to2021, model = "within", index = c("STATEDSCR", "year"))

five_to9 <- plm(perc_diff ~ five_to9,
                data = effort_data_to2021, model = "within", index = c("STATEDSCR", "year"))

ten_to19 <- plm(perc_diff ~ ten_to19,
                data = effort_data_to2021, model = "within", index = c("STATEDSCR", "year"))

twenty_to99 <- plm(perc_diff ~ twenty_to99,
                   data = effort_data_to2021, model = "within", index = c("STATEDSCR", "year"))

hundred_to499 <- plm(perc_diff ~ hundred_to499,
                     data = effort_data_to2021, model = "within", index = c("STATEDSCR", "year"))

more_than500 <- plm(perc_diff ~ more_than500,
                    data = effort_data_to2021, model = "within", index = c("STATEDSCR", "year"))


fe_results <- c("less_than5", "five_to9", "ten_to19", "twenty_to99", "hundred_to499", "more_than500")

for(i in fe_results){
  
  assign(i, as.data.frame(coef(summary(get(i)))) %>%
           mutate(variable = rownames(.)) %>%
           dplyr::select(variable, "Estimate", "Std. Error", "Pr(>|t|)") %>%
           rename(p_est = "Pr(>|t|)") %>%
           mutate(rsq = round(r.squared(get(i)), digits = 4)) %>%
           pivot_longer(cols = c("Estimate", "Std. Error", "p_est", "rsq"), 
                        names_to = "statistic", values_to = "number") %>%
           mutate(number = as.numeric(number),
                  number = ifelse(statistic == "Estimate", round(number, digits = 2), number),
                  number = ifelse(statistic == "Std. Error", paste0("(", round(number, digits = 2), ") "), number),
           ) %>%
           pivot_wider(names_from = statistic, values_from = number) %>% 
           mutate(p_est = as.numeric(p_est),
                  p_value = case_when(p_est < 0.001 ~ "***",
                                      p_est < 0.01 & p_est >= 0.001 ~ "**",
                                      p_est < 0.05 & p_est >= 0.01 ~ "*",
                                      p_est < 0.1 & p_est >= 0.05 ~ "`",
                                      p_est > 0.1 ~ ""),
                  Estimate = paste0(Estimate, p_value)) %>%
           pivot_longer(cols = c("Estimate", "Std. Error", "rsq"), 
                        names_to = "statistic", values_to = "number") %>%
           add_row(variable = NA, number = i, .before = 1) %>%
           dplyr::select(variable, number))
  
  
  
}


table <- cbind.data.frame(less_than5, five_to9, ten_to19, twenty_to99, hundred_to499, more_than500)[,c(2, 4, 6, 8, 10, 12)]
table_header <- table
colnames(table_header) <- table[1,]
table_numbers <- table_header[-1,]

variables <- c("Percentage Change in GDP", NA, "R-squared")

table2 <- cbind(variables, table_numbers)

write.xlsx(table2, "FE_to2021.xlsx")

# ---- Main Analysis to 2019, inflation-adjusted ----

options(scipen = 999)

less_than5 <- plm(perc_realdiff ~ less_than5,
                  data = effort_data_to2019, model = "within", index = c("STATEDSCR", "year"))

five_to9 <- plm(perc_realdiff ~ five_to9,
                data = effort_data_to2019, model = "within", index = c("STATEDSCR", "year"))

ten_to19 <- plm(perc_realdiff ~ ten_to19,
                data = effort_data_to2019, model = "within", index = c("STATEDSCR", "year"))

twenty_to99 <- plm(perc_realdiff ~ twenty_to99,
                   data = effort_data_to2019, model = "within", index = c("STATEDSCR", "year"))

hundred_to499 <- plm(perc_realdiff ~ hundred_to499,
                     data = effort_data_to2019, model = "within", index = c("STATEDSCR", "year"))

more_than500 <- plm(perc_realdiff ~ more_than500,
                    data = effort_data_to2019, model = "within", index = c("STATEDSCR", "year"))


fe_results <- c("less_than5", "five_to9", "ten_to19", "twenty_to99", "hundred_to499", "more_than500")

for(i in fe_results){
  
  assign(i, as.data.frame(coef(summary(get(i)))) %>%
           mutate(variable = rownames(.)) %>%
           dplyr::select(variable, "Estimate", "Std. Error", "Pr(>|t|)") %>%
           rename(p_est = "Pr(>|t|)") %>%
           mutate(rsq = round(r.squared(get(i)), digits = 4)) %>%
           pivot_longer(cols = c("Estimate", "Std. Error", "p_est", "rsq"), 
                        names_to = "statistic", values_to = "number") %>%
           mutate(number = as.numeric(number),
                  number = ifelse(statistic == "Estimate", round(number, digits = 2), number),
                  number = ifelse(statistic == "Std. Error", paste0("(", round(number, digits = 2), ") "), number),
           ) %>%
           pivot_wider(names_from = statistic, values_from = number) %>% 
           mutate(p_est = as.numeric(p_est),
                  p_value = case_when(p_est < 0.001 ~ "***",
                                      p_est < 0.01 & p_est >= 0.001 ~ "**",
                                      p_est < 0.05 & p_est >= 0.01 ~ "*",
                                      p_est < 0.1 & p_est >= 0.05 ~ "`",
                                      p_est > 0.1 ~ ""),
                  Estimate = paste0(Estimate, p_value)) %>%
           pivot_longer(cols = c("Estimate", "Std. Error", "rsq"), 
                        names_to = "statistic", values_to = "number") %>%
           add_row(variable = NA, number = i, .before = 1) %>%
           dplyr::select(variable, number))
  
  
  
}


table <- cbind.data.frame(less_than5, five_to9, ten_to19, twenty_to99, hundred_to499, more_than500)[,c(2, 4, 6, 8, 10, 12)]
table_header <- table
colnames(table_header) <- table[1,]
table_numbers <- table_header[-1,]

variables <- c("Percentage Change in GDP", NA, "R-squared")

table3 <- cbind(variables, table_numbers)

write.xlsx(table3, "FE_to2019_inflation.xlsx")

# ---- Main Analysis to 2021, inflation-adjusted ----

options(scipen = 999)

less_than5 <- plm(perc_realdiff ~ less_than5,
                  data = effort_data_to2021, model = "within", index = c("STATEDSCR", "year"))

five_to9 <- plm(perc_realdiff ~ five_to9,
                data = effort_data_to2021, model = "within", index = c("STATEDSCR", "year"))

ten_to19 <- plm(perc_realdiff ~ ten_to19,
                data = effort_data_to2021, model = "within", index = c("STATEDSCR", "year"))

twenty_to99 <- plm(perc_realdiff ~ twenty_to99,
                   data = effort_data_to2021, model = "within", index = c("STATEDSCR", "year"))

hundred_to499 <- plm(perc_realdiff ~ hundred_to499,
                     data = effort_data_to2021, model = "within", index = c("STATEDSCR", "year"))

more_than500 <- plm(perc_realdiff ~ more_than500,
                    data = effort_data_to2021, model = "within", index = c("STATEDSCR", "year"))


fe_results <- c("less_than5", "five_to9", "ten_to19", "twenty_to99", "hundred_to499", "more_than500")

for(i in fe_results){
  
  assign(i, as.data.frame(coef(summary(get(i)))) %>%
           mutate(variable = rownames(.)) %>%
           dplyr::select(variable, "Estimate", "Std. Error", "Pr(>|t|)") %>%
           rename(p_est = "Pr(>|t|)") %>%
           mutate(rsq = round(r.squared(get(i)), digits = 4)) %>%
           pivot_longer(cols = c("Estimate", "Std. Error", "p_est", "rsq"), 
                        names_to = "statistic", values_to = "number") %>%
           mutate(number = as.numeric(number),
                  number = ifelse(statistic == "Estimate", round(number, digits = 2), number),
                  number = ifelse(statistic == "Std. Error", paste0("(", round(number, digits = 2), ") "), number),
           ) %>%
           pivot_wider(names_from = statistic, values_from = number) %>% 
           mutate(p_est = as.numeric(p_est),
                  p_value = case_when(p_est < 0.001 ~ "***",
                                      p_est < 0.01 & p_est >= 0.001 ~ "**",
                                      p_est < 0.05 & p_est >= 0.01 ~ "*",
                                      p_est < 0.1 & p_est >= 0.05 ~ "`",
                                      p_est > 0.1 ~ ""),
                  Estimate = paste0(Estimate, p_value)) %>%
           pivot_longer(cols = c("Estimate", "Std. Error", "rsq"), 
                        names_to = "statistic", values_to = "number") %>%
           add_row(variable = NA, number = i, .before = 1) %>%
           dplyr::select(variable, number))
  
  
  
}


table <- cbind.data.frame(less_than5, five_to9, ten_to19, twenty_to99, hundred_to499, more_than500)[,c(2, 4, 6, 8, 10, 12)]
table_header <- table
colnames(table_header) <- table[1,]
table_numbers <- table_header[-1,]

variables <- c("Percentage Change in GDP", NA, "R-squared")

table4 <- cbind(variables, table_numbers)

write.xlsx(table4, "FE_to2021_inflation.xlsx")

# ---- Clean Data for Data Visualizations ----

effort_avgs <- effort_data %>%
  filter(year != 1997) %>%
  group_by(STATEDSCR) %>%
  summarize(less5_avg = mean(less_than5),
            five9_avg = mean(five_to9),
            ten19_avg = mean(ten_to19),
            twenty99_avg = mean(twenty_to99),
            hundred499_avg = mean(hundred_to499),
            more500_avg = mean(more_than500),
            GDP = mean(GDP),
            year_diff = mean(year_diff2),
            perc_diff = mean(perc_diff)) %>%
  mutate(quart5 = ifelse(less5_avg < quantile(less5_avg, 0.25), 1,
                         ifelse(less5_avg >= quantile(less5_avg, 0.25)
                                & less5_avg < quantile(less5_avg, 0.5), 2,
                                ifelse(less5_avg >= quantile(less5_avg, 0.5)
                                       & less5_avg < quantile(less5_avg, 0.75), 3,
                                       ifelse(less5_avg >= quantile(less5_avg, 0.75), 4, NA)))),
         quart10 = ifelse(five9_avg < quantile(five9_avg, 0.25), 1,
                         ifelse(five9_avg >= quantile(five9_avg, 0.25)
                                & five9_avg < quantile(five9_avg, 0.5), 2,
                                ifelse(five9_avg >= quantile(five9_avg, 0.5)
                                       & five9_avg < quantile(five9_avg, 0.75), 3,
                                       ifelse(five9_avg >= quantile(five9_avg, 0.75), 4, NA)))),
         quart20 = ifelse(ten19_avg < quantile(ten19_avg, 0.25), 1,
                         ifelse(ten19_avg >= quantile(ten19_avg, 0.25)
                                & ten19_avg < quantile(ten19_avg, 0.5), 2,
                                ifelse(ten19_avg >= quantile(ten19_avg, 0.5)
                                       & ten19_avg < quantile(ten19_avg, 0.75), 3,
                                       ifelse(ten19_avg >= quantile(ten19_avg, 0.75), 4, NA)))),
         quart100 = ifelse(twenty99_avg < quantile(twenty99_avg, 0.25), 1,
                         ifelse(twenty99_avg >= quantile(twenty99_avg, 0.25)
                                & twenty99_avg < quantile(twenty99_avg, 0.5), 2,
                                ifelse(twenty99_avg >= quantile(twenty99_avg, 0.5)
                                       & twenty99_avg < quantile(twenty99_avg, 0.75), 3,
                                       ifelse(twenty99_avg >= quantile(twenty99_avg, 0.75), 4, NA)))),
         quart500 = ifelse(hundred499_avg < quantile(hundred499_avg, 0.25), 1,
                           ifelse(hundred499_avg >= quantile(hundred499_avg, 0.25)
                                  & hundred499_avg < quantile(hundred499_avg, 0.5), 2,
                                  ifelse(hundred499_avg >= quantile(hundred499_avg, 0.5)
                                         & hundred499_avg < quantile(hundred499_avg, 0.75), 3,
                                         ifelse(hundred499_avg >= quantile(hundred499_avg, 0.75), 4, NA)))),
         quart500plus = ifelse(more500_avg < quantile(more500_avg, 0.25), 1,
                               ifelse(more500_avg >= quantile(more500_avg, 0.25)
                                      & more500_avg < quantile(more500_avg, 0.5), 2,
                                      ifelse(more500_avg >= quantile(more500_avg, 0.5)
                                             & more500_avg < quantile(more500_avg, 0.75), 3,
                                             ifelse(more500_avg >= quantile(more500_avg, 0.75), 4, NA)))),
         quart_yeardiff = ifelse(year_diff < quantile(year_diff, 0.25), 1,
                               ifelse(year_diff >= quantile(year_diff, 0.25)
                                      & year_diff < quantile(year_diff, 0.5), 2,
                                      ifelse(year_diff >= quantile(year_diff, 0.5)
                                             & year_diff < quantile(year_diff, 0.75), 3,
                                             ifelse(year_diff >= quantile(year_diff, 0.75), 4, NA)))),
         quart5_fact = factor(quart5,
                              levels = c(1, 2, 3, 4),
                              labels = c("Fewest", "Fewer", "More", "Most"),
                              ordered = T),
         quart10_fact = factor(quart10,
                               levels = c(1, 2, 3, 4),
                               labels = c("Fewest", "Fewer", "More", "Most"),
                              ordered = T),
         quart20_fact = factor(quart20,
                               levels = c(1, 2, 3, 4),
                               labels = c("Fewest", "Fewer", "More", "Most"),
                              ordered = T),
         quart100_fact = factor(quart100,
                                levels = c(1, 2, 3, 4),
                                labels = c("Fewest", "Fewer", "More", "Most"),
                              ordered = T),
         quart500_fact = factor(quart500,
                                levels = c(1, 2, 3, 4),
                                labels = c("Fewest", "Fewer", "More", "Most"),
                              ordered = T),
         quart500plus_fact = factor(quart500plus,
                                    levels = c(1, 2, 3, 4),
                                    labels = c("Fewest", "Fewer", "More", "Most"),
                              ordered = T),
         quartyear_fact = factor(quart_yeardiff,
                                    levels = c(1, 2, 3, 4),
                                    labels = c("Lowest", "Lower", "Higher", "Highest"),
                                    ordered = T))

abbrevs <- import("State abbrevs.csv")

effort_vizdata <- cbind(effort_avgs, abbrevs)

theme <- theme(legend.title = element_blank(),
               panel.background = element_blank(),
               strip.background = element_blank(),
               panel.border = element_rect(fill = NA, color = "black", size = 0.5),
               panel.grid.minor.x = element_blank(),
               panel.grid.minor.y = element_blank(),
               panel.grid.major.x = element_line(color = "#adadad", linetype = 3, size = .5),
               panel.grid.major.y = element_blank(),
               axis.line.y = element_line(color = "#adadad", linetype = 1, linewidth = .5),
               axis.ticks.y = element_line(color = "black", size = 0.3),
               axis.ticks.x = element_line(color = "black", size = 0.3),
               legend.position = "none",
               legend.background = element_rect(fill = NA, color = "black"),
               legend.text = element_text(size = 7),
               axis.title.y = element_text(angle=0))

theme2 <- theme(legend.title = element_blank(),
                panel.background = element_blank(),
                strip.background = element_blank(),
                panel.border = element_rect(fill = NA, color = "black", size = 0.5),
                panel.grid.minor.x = element_blank(),
                panel.grid.minor.y = element_blank(),
                panel.grid.major.y = element_line(color = "#adadad", linetype = 3, size = .5),
                panel.grid.major.x = element_blank(),
                axis.line.y = element_line(color = "#adadad", linetype = 1, linewidth = .5),
                axis.ticks.y = element_line(color = "black", size = 0.3),
                axis.ticks.x = element_line(color = "black", size = 0.3),
                legend.position = "none",
                legend.background = element_rect(fill = NA, color = "black"),
                legend.text = element_text(size = 7),
                axis.title.y = element_text(angle=0))

# Bar charts 

effort_quart5 <- effort_vizdata %>%
  group_by(quart5_fact) %>%
  summarize(GDPdiff_mean = mean(year_diff),
            GDPdiff_sd = sd(year_diff),
            GDPdiff_med = median(year_diff))

effort_quart10 <- effort_vizdata %>%
  group_by(quart10_fact) %>%
  summarize(GDPdiff_mean = mean(year_diff),
            GDPdiff_sd = sd(year_diff),
            GDPdiff_med = median(year_diff))

effort_quart20 <- effort_vizdata %>%
  group_by(quart20_fact) %>%
  summarize(GDPdiff_mean = mean(year_diff),
            GDPdiff_sd = sd(year_diff),
            GDPdiff_med = median(year_diff))

effort_quart100 <- effort_vizdata %>%
  group_by(quart100_fact) %>%
  summarize(GDPdiff_mean = mean(year_diff),
            GDPdiff_sd = sd(year_diff),
            GDPdiff_med = median(year_diff))

effort_quart500 <- effort_vizdata %>%
  group_by(quart500_fact) %>%
  summarize(GDPdiff_mean = mean(year_diff),
            GDPdiff_sd = sd(year_diff),
            GDPdiff_med = median(year_diff))

effort_quart500plus <- effort_vizdata %>%
  group_by(quart500plus_fact) %>%
  summarize(GDPdiff_mean = mean(year_diff),
            GDPdiff_sd = sd(year_diff),
            GDPdiff_med = median(year_diff))

# Year Diff Bars

diff_quart5 <- effort_vizdata %>%
  group_by(quartyear_fact) %>%
  summarize(GDPdiff_mean = mean(less5_avg),
            GDPdiff_sd = sd(less5_avg),
            GDPdiff_med = median(less5_avg))

diff_quart10 <- effort_vizdata %>%
  group_by(quartyear_fact) %>%
  summarize(GDPdiff_mean = mean(five9_avg),
            GDPdiff_sd = sd(five9_avg),
            GDPdiff_med = median(five9_avg))

diff_quart20 <- effort_vizdata %>%
  group_by(quartyear_fact) %>%
  summarize(GDPdiff_mean = mean(ten19_avg),
            GDPdiff_sd = sd(ten19_avg),
            GDPdiff_med = median(ten19_avg))

diff_quart100 <- effort_vizdata %>%
  group_by(quartyear_fact) %>%
  summarize(GDPdiff_mean = mean(twenty99_avg),
            GDPdiff_sd = sd(twenty99_avg),
            GDPdiff_med = median(twenty99_avg))

diff_quart500 <- effort_vizdata %>%
  group_by(quartyear_fact) %>%
  summarize(GDPdiff_mean = mean(hundred499_avg),
            GDPdiff_sd = sd(hundred499_avg),
            GDPdiff_med = median(hundred499_avg))

diff_quart500plus <- effort_vizdata %>%
  group_by(quartyear_fact) %>%
  summarize(GDPdiff_mean = mean(more500_avg),
            GDPdiff_sd = sd(more500_avg),
            GDPdiff_med = median(more500_avg))




# ---- Data Visualizations ----

# ---- amount diff viz ----

# <5

effort_diff5 <- ggplot(data = effort_vizdata, aes(x = less5_avg, y = year_diff)) +
  geom_hline(aes(yintercept = 0), color = "#adadad", size = .25) +
  geom_smooth(method = lm, color = "#fc5c3c", fill = "#9881aa") +
  geom_point(size = .75) +
  geom_text(aes(label = abbrev), size = 2, nudge_y = 2000) +
  scale_y_continuous(breaks = seq(-25000, 75000, by = 25000),
                     labels = c("-$25Bn", "+$0", "+$25Bn", "+$50Bn", "+$75Bn")) +
  scale_x_continuous(breaks = seq(50, 65, by = 5),
                     labels = c("50%", "55%", "60%", "65%")) +
  labs(title = "Businesses with Fewer Than 5 Employees",
       x = "Percentage of Businesses in Each State", y = element_blank()) +
  theme

# <10

effort_diff10 <- ggplot(data = effort_vizdata, aes(x = five9_avg, y = year_diff)) +
  geom_hline(aes(yintercept = 0), color = "#adadad", size = .25) +
  geom_smooth(method = lm, color = "#fc5c3c", fill = "#9881aa") +
  geom_point(size = .75) +
  geom_text(aes(label = abbrev), size = 2, nudge_y = 4000) +
  scale_y_continuous(breaks = seq(-25000, 75000, by = 25000),
                     labels = c("-$25Bn", "+$0", "+$25Bn", "+$50Bn", "+$75Bn")) +
  scale_x_continuous(breaks = seq(15, 18, by = 1),
                     labels = c("15%", "16%", "17%", "18%")) +
  labs(title = "5-9 Employees",
       x = element_blank(), y = element_blank()) +
  theme

# <20

effort_diff20 <- ggplot(data = effort_vizdata, aes(x = ten19_avg, y = year_diff)) +
  geom_hline(aes(yintercept = 0), color = "#adadad", size = .25) +
  geom_smooth(method = lm, color = "#fc5c3c", fill = "#9881aa") +
  geom_point(size = .75) +
  geom_text(aes(label = abbrev), size = 2, nudge_y = 4000) +
  scale_y_continuous(breaks = seq(-25000, 75000, by = 25000),
                     labels = c("-$25Bn", "+$0", "+$25Bn", "+$50Bn", "+$75Bn")) +
  scale_x_continuous(breaks = seq(9, 11, by = 1),
                     labels = c("9%", "10%", "11%")) +
  labs(title = "10-19 Employees",
       x = element_blank(), y = element_blank()) +
  theme

# <100

effort_diff100 <- ggplot(data = effort_vizdata, aes(x = twenty99_avg, y = year_diff)) +
  geom_hline(aes(yintercept = 0), color = "#adadad", size = .25) +
  geom_smooth(method = lm, color = "#fc5c3c", fill = "#9881aa") +
  geom_point(size = .75) +
  geom_text(aes(label = abbrev), size = 2, nudge_y = 4000) +
  scale_y_continuous(breaks = seq(-25000, 75000, by = 25000),
                     labels = c("-$25Bn", "+$0", "+$25Bn", "+$50Bn", "+$75Bn")) +
  scale_x_continuous(breaks = seq(8, 12, by = 2),
                     labels = c("8%", "10%", "12%")) +
  labs(title = "20-99 Employees",
       x = "Percentage of Businesses", y = element_blank()) +
  theme

# <500

effort_diff500 <- ggplot(data = effort_vizdata, aes(x = hundred499_avg, y = year_diff)) +
  geom_hline(aes(yintercept = 0), color = "#adadad", size = .25) +
  geom_smooth(method = lm, color = "#fc5c3c", fill = "#9881aa") +
  geom_point(size = .75) +
  geom_text(aes(label = abbrev), size = 2, nudge_y = 4000) +
  scale_y_continuous(breaks = seq(-25000, 75000, by = 25000),
                     labels = c("-$25Bn", "+$0", "+$25Bn", "+$50Bn", "+$75Bn")) +
  scale_x_continuous(breaks = seq(2, 5, by = 1),
                     labels = c("2%", "3%", "4%", "5%")) +
  labs(title = "100-499 Employees",
       x = "Percentage of Businesses", y = element_blank()) +
  theme

# 500+

effort_diff500plus <- ggplot(data = effort_vizdata, aes(x = more500_avg, y = year_diff)) +
  geom_hline(aes(yintercept = 0), color = "#adadad", size = .25) +
  geom_smooth(method = lm, color = "#fc5c3c", fill = "#9881aa") +
  geom_point(size = .75) +
  geom_text(aes(label = abbrev), size = 2, nudge_y = 2000) +
  scale_y_continuous(breaks = seq(-25000, 75000, by = 25000),
                     labels = c("-$25Bn", "+$0", "+$25Bn", "+$50Bn", "+$75Bn")) +
  scale_x_continuous(breaks = seq(2, 8, by = 2),
                     labels = c("2%", "4%", "6%", "8%")) +
  labs(title = "Businesses with More Than 500 Employees",
       x = "Percentage of Businesses in Each State", y = element_blank()) +
  theme

# arranging

mid_biz <- grid.arrange(effort_diff10, effort_diff20, effort_diff100, effort_diff500, ncol = 2, nrow = 2)

arranged_scatter <- grid.arrange(grid.text(label = "Average\nAnnual\nChange\nin GDP",
                                           gp = gpar()),
                                 effort_diff5, mid_biz, effort_diff500plus, 
                                 ncol = 4,
                                 widths = unit(c(0.2, 1, 1, 1), "null"),
                                 top = grid.text(label = "Association Between Average Percentage of Business Sizes and Average Annual Change in GDP Across U.S. States (1997-2019)\n",
                                                 # just = "right",
                                                 hjust = 0.5,
                                                 gp = gpar(fontface = "bold",
                                                           fontsize = 16)),
                                 bottom = grid.text(label = "\nNathan Witkin | Outside Innovation Institute",
                                                 # just = "left",
                                                 hjust = -0.95,
                                                 gp = gpar(fontsize = 10)))

png(filename = "scatter_combo.png",
    units = "in", width=14.5, height=5, res=400)
grid.newpage()
grid.draw(arranged_scatter)
dev.off()


# Bar graphs

effort_bars5 <- ggplot(data = effort_quart5) +
  geom_hline(aes(yintercept = 0), color = "#adadad", size = .5) +
  geom_bar(aes(x = quart5_fact, y = GDPdiff_mean), stat = "identity", fill = "#7838a8") +
  geom_errorbar(aes(ymin = GDPdiff_mean - GDPdiff_sd, ymax = GDPdiff_mean + GDPdiff_sd,
                    x = quart5_fact), stat = "identity", width = 0.2, color = "#fc5c3c",
                position = position_dodge(width = .9),
                show.legend = F) +
  scale_y_continuous(breaks = seq(0, 50000, by = 25000),
                     labels = c("+$0", "+$25Bn", "+$50Bn")) +
  labs(title = "Businesses with Fewer Than 5 Employees",
       x = element_blank(), y = element_blank()) +
  theme2

effort_bars10 <- ggplot(data = effort_quart10) +
  geom_hline(aes(yintercept = 0), color = "#adadad", size = .5) +
  geom_bar(aes(x = quart10_fact, y = GDPdiff_mean), stat = "identity", fill = "#7838a8") +
  geom_errorbar(aes(ymin = GDPdiff_mean - GDPdiff_sd, ymax = GDPdiff_mean + GDPdiff_sd,
                    x = quart10_fact), stat = "identity", width = 0.2, color = "#fc5c3c",
                position = position_dodge(width = .9),
                show.legend = F) +
  scale_y_continuous(breaks = seq(0, 50000, by = 25000),
                     labels = c("+$0", "+$25Bn", "+$50Bn")) +
  labs(title = "5-9 Employees",
       x = element_blank(), y = element_blank()) +
  theme2

effort_bars20 <- ggplot(data = effort_quart20) +
  geom_hline(aes(yintercept = 0), color = "#adadad", size = .5) +
  geom_bar(aes(x = quart20_fact, y = GDPdiff_mean), stat = "identity", fill = "#7838a8") +
  geom_errorbar(aes(ymin = GDPdiff_mean - GDPdiff_sd, ymax = GDPdiff_mean + GDPdiff_sd,
                    x = quart20_fact), stat = "identity", width = 0.2, color = "#fc5c3c",
                position = position_dodge(width = .9),
                show.legend = F) +
  scale_y_continuous(breaks = seq(0, 50000, by = 25000),
                     labels = c("+$0", "+$25Bn", "+$50Bn")) +
  labs(title = "10-19 Employees",
       x = element_blank(), y = element_blank()) +
  theme2

effort_bars100 <- ggplot(data = effort_quart100) +
  geom_hline(aes(yintercept = 0), color = "#adadad", size = .5) +
  geom_bar(aes(x = quart100_fact, y = GDPdiff_mean), stat = "identity", fill = "#7838a8") +
  geom_errorbar(aes(ymin = GDPdiff_mean - GDPdiff_sd, ymax = GDPdiff_mean + GDPdiff_sd,
                    x = quart100_fact), stat = "identity", width = 0.2, color = "#fc5c3c",
                position = position_dodge(width = .9),
                show.legend = F) +
  scale_y_continuous(breaks = seq(0, 50000, by = 25000),
                     labels = c("+$0", "+$25Bn", "+$50Bn")) +
  labs(title = "20-99 Employees",
       x = element_blank(), y = element_blank()) +
  theme2

effort_bars500 <- ggplot(data = effort_quart500) +
  geom_hline(aes(yintercept = 0), color = "#adadad", size = .5) +
  geom_bar(aes(x = quart500_fact, y = GDPdiff_mean), stat = "identity", fill = "#7838a8") +
  geom_errorbar(aes(ymin = GDPdiff_mean - GDPdiff_sd, ymax = GDPdiff_mean + GDPdiff_sd,
                    x = quart500_fact), stat = "identity", width = 0.2, color = "#fc5c3c",
                position = position_dodge(width = .9),
                show.legend = F) +
  scale_y_continuous(breaks = seq(0, 50000, by = 25000),
                     labels = c("+$0", "+$25Bn", "+$50Bn")) +
  labs(title = "100-499 Employees",
       x = element_blank(), y = element_blank()) +
  theme2

effort_bars500plus <- ggplot(data = effort_quart500plus) +
  geom_hline(aes(yintercept = 0), color = "#adadad", size = .5) +
  geom_bar(aes(x = quart500plus_fact, y = GDPdiff_mean), stat = "identity", fill = "#7838a8") +
  geom_errorbar(aes(ymin = GDPdiff_mean - GDPdiff_sd, ymax = GDPdiff_mean + GDPdiff_sd,
                    x = quart500plus_fact), stat = "identity", width = 0.2, color = "#fc5c3c",
                position = position_dodge(width = .9),
                show.legend = F) +
  scale_y_continuous(breaks = seq(0, 50000, by = 25000),
                     labels = c("+$0", "+$25Bn", "+$50Bn")) +
  labs(title = "Businesses with More Than 500 Employees",
       x = element_blank(), y = element_blank()) +
  theme2

# arranging

mid_bars <- grid.arrange(effort_bars10, effort_bars20, effort_bars100, effort_bars500, ncol = 2, nrow = 2)

arranged_bars <- grid.arrange(grid.text(label = "Average\nAnnual\nChange\nin GDP",
                                           gp = gpar()),
                              effort_bars5, mid_bars, effort_bars500plus, 
                                 ncol = 4,
                                 widths = unit(c(0.2, 1, 1, 1), "null"),
                                 top = grid.text(label = "Association Between Quartiles of Different Business Sizes Within U.S. States and Average Annual Increase in State GDP (1997-2019)\n",
                                                 # just = "right",
                                                 hjust = 0.5,
                                                 gp = gpar(fontface = "bold",
                                                           fontsize = 16)),
                                 bottom = grid.text(label = "\nNathan Witkin | Outside Innovation Institute",
                                                    # just = "left",
                                                    hjust = -0.95,
                                                    gp = gpar(fontsize = 10)))

png(filename = "bars_combo.png",
    units = "in", width=14.5, height=5, res=400)
grid.newpage()
grid.draw(arranged_bars)
dev.off()


# ---- percent diff viz ----

# <5

perc_diff5 <- ggplot(data = effort_vizdata, aes(x = less5_avg, y = perc_diff)) +
  geom_hline(aes(yintercept = 0), color = "#adadad", size = .25) +
  geom_smooth(method = lm, color = "#fc5c3c", fill = "#9881aa") +
  geom_point(size = .75) +
  geom_text(aes(label = abbrev), size = 2, nudge_y = 0.1) +
  # scale_y_continuous(breaks = seq(-25000, 75000, by = 25000),
  #                    labels = c("-$25Bn", "+$0", "+$25Bn", "+$50Bn", "+$75Bn")) +
  # scale_x_continuous(breaks = seq(50, 65, by = 5),
  #                    labels = c("50%", "55%", "60%", "65%")) +
  labs(title = "Businesses with Fewer Than 5 Employees",
       x = "Percentage of Businesses in Each State", y = element_blank()) +
  theme

png(filename = "perc_diff5.png",
    units = "in", width=8, height=10, res=400)
plot(perc_diff5)
dev.off()

# <10

perc_diff10 <- ggplot(data = effort_vizdata, aes(x = five9_avg, y = perc_diff)) +
  geom_hline(aes(yintercept = 0), color = "#adadad", size = .25) +
  geom_smooth(method = lm, color = "#fc5c3c", fill = "#9881aa") +
  geom_point(size = .75) +
  geom_text(aes(label = abbrev), size = 2, nudge_y = 0.1) +
  # scale_y_continuous(breaks = seq(-25000, 75000, by = 25000),
  #                    labels = c("-$25Bn", "+$0", "+$25Bn", "+$50Bn", "+$75Bn")) +
  # scale_x_continuous(breaks = seq(15, 18, by = 1),
  #                    labels = c("15%", "16%", "17%", "18%")) +
  labs(title = "5-9 Employees",
       x = element_blank(), y = element_blank()) +
  theme

# <20

perc_diff20 <- ggplot(data = effort_vizdata, aes(x = ten19_avg, y = perc_diff)) +
  geom_hline(aes(yintercept = 0), color = "#adadad", size = .25) +
  geom_smooth(method = lm, color = "#fc5c3c", fill = "#9881aa") +
  geom_point(size = .75) +
  geom_text(aes(label = abbrev), size = 2, nudge_y = 0.1) +
  # scale_y_continuous(breaks = seq(-25000, 75000, by = 25000),
  #                    labels = c("-$25Bn", "+$0", "+$25Bn", "+$50Bn", "+$75Bn")) +
  # scale_x_continuous(breaks = seq(9, 11, by = 1),
  #                    labels = c("9%", "10%", "11%")) +
  labs(title = "10-19 Employees",
       x = element_blank(), y = element_blank()) +
  theme

# <100

perc_diff100 <- ggplot(data = effort_vizdata, aes(x = twenty99_avg, y = perc_diff)) +
  geom_hline(aes(yintercept = 0), color = "#adadad", size = .25) +
  geom_smooth(method = lm, color = "#fc5c3c", fill = "#9881aa") +
  geom_point(size = .75) +
  geom_text(aes(label = abbrev), size = 2, nudge_y = 0.1) +
  # scale_y_continuous(breaks = seq(-25000, 75000, by = 25000),
  #                    labels = c("-$25Bn", "+$0", "+$25Bn", "+$50Bn", "+$75Bn")) +
  # scale_x_continuous(breaks = seq(8, 12, by = 2),
  #                    labels = c("8%", "10%", "12%")) +
  labs(title = "20-99 Employees",
       x = "Percentage of Businesses", y = element_blank()) +
  theme

# <500

perc_diff500 <- ggplot(data = effort_vizdata, aes(x = hundred499_avg, y = perc_diff)) +
  geom_hline(aes(yintercept = 0), color = "#adadad", size = .25) +
  geom_smooth(method = lm, color = "#fc5c3c", fill = "#9881aa") +
  geom_point(size = .75) +
  geom_text(aes(label = abbrev), size = 2, nudge_y = 0.1) +
  # scale_y_continuous(breaks = seq(-25000, 75000, by = 25000),
  #                    labels = c("-$25Bn", "+$0", "+$25Bn", "+$50Bn", "+$75Bn")) +
  # scale_x_continuous(breaks = seq(2, 5, by = 1),
  #                    labels = c("2%", "3%", "4%", "5%")) +
  labs(title = "100-499 Employees",
       x = "Percentage of Businesses", y = element_blank()) +
  theme

# 500+

perc_diff500plus <- ggplot(data = effort_vizdata, aes(x = more500_avg, y = perc_diff)) +
  geom_hline(aes(yintercept = 0), color = "#adadad", size = .25) +
  geom_smooth(method = lm, color = "#fc5c3c", fill = "#9881aa") +
  geom_point(size = .75) +
  geom_text(aes(label = abbrev), size = 2, nudge_y = 0.1) +
  # scale_y_continuous(breaks = seq(-25000, 75000, by = 25000),
  #                    labels = c("-$25Bn", "+$0", "+$25Bn", "+$50Bn", "+$75Bn")) +
  # scale_x_continuous(breaks = seq(2, 8, by = 2),
  #                    labels = c("2%", "4%", "6%", "8%")) +
  labs(title = "Businesses with More Than 500 Employees",
       x = "Percentage of Businesses in Each State", y = element_blank()) +
  theme

# arranging

mid_biz_perc <- grid.arrange(perc_diff10, perc_diff20, perc_diff100, perc_diff500, ncol = 2, nrow = 2)

arranged_scatter_perc <- grid.arrange(grid.text(label = "Average\nAnnual\nPercentage\nChange\nin GDP",
                                           gp = gpar()),
                                 perc_diff5, mid_biz_perc, perc_diff500plus, 
                                 ncol = 4,
                                 widths = unit(c(0.2, 1, 1, 1), "null"),
                                 top = grid.text(label = "Association Between Average Percentage of Business Sizes and Average Annual Percentage Change in GDP Across U.S. States (1997-2019)\n",
                                                 # just = "right",
                                                 hjust = 0.5,
                                                 gp = gpar(fontface = "bold",
                                                           fontsize = 16)),
                                 bottom = grid.text(label = "\nNathan Witkin | Outside Innovation Institute",
                                                    # just = "left",
                                                    hjust = -0.95,
                                                    gp = gpar(fontsize = 10)))

png(filename = "scatter_combo_perc.png",
    units = "in", width=14.5, height=5, res=400)
grid.newpage()
grid.draw(arranged_scatter_perc)
dev.off()

