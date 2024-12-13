
# MICROSIMULATION CHILD POVERTY SCRIPT
# Code author: Ronan McCabe, SPHSU University of Glasgow, <ronan.mccabe@glasgow.ac.uk>
# Contributors: Roxana Pollack &  Eirk Iglestrom
  
# note: Hartlepool infant deaths in 2020 = 1

# libraries # 

library(data.table) # parallel data manipulation
library(proto)
library(gsubfn) # string templates
library(magrittr)
library(tibble)
library(dplyr)
library(tidyverse)
library(R.utils)
library(readr)
library(LaF)
library(openxlsx)
library(readxl)
library(dtplyr)
library(writexl)
library(tictoc)
library(lme4)
library(Matrix)
library(arm)



# Analysis Data

cp_data <- read_csv("cp_data_complete_analysis_200324.csv")
cp_data <- cp_data %>%
  mutate(country = "England")

## correct errors in area for Middlesbrough and Rochdale

cp_data <-  cp_data %>%
  mutate(area = if_else(area_name == 'Middlesbrough', 'North East', 
                        if_else(area_name == 'Rochdale', 'North West', area)))


#### Descriptive Stats ####

cp_data_2023 <- cp_data %>%
  filter(year == 2023)

cp_data_national <- cp_data_2023 %>%
  group_by(country) %>%
  mutate(cp_mean = weighted.mean(cp_baseline, pop_U16),
         cp_med = median(cp_baseline),
         cp_iqr = IQR(cp_baseline),
         mort = sum(mortality)/sum(live_births)*100000,
         cla = sum(CLA_started_U16)/sum(pop_U16)*100000,
         nutri = sum(events_34)/sum(pop_34)*100000,
         admis = sum(events_03)/sum(pop_03)*100000) %>%
  dplyr::select(country, mort, cla, nutri, admis, cp_med, cp_iqr,cp_mean) %>%
  distinct()
  

cp_data_2023_county <- cp_data_2023 %>%
  group_by(area) %>%
  mutate(cp_mean = weighted.mean(cp_baseline, pop_U16),
         cp_med = median(cp_baseline),
         cp_iqr = IQR(cp_baseline),
         mort = sum(mortality)/sum(live_births)*100000,
         nutri = sum(events_34)/sum(pop_34)*100000,
         admis = sum(events_03)/sum(pop_03)*100000, 
         cla = sum(CLA_started_U16)/sum(pop_U16)*100000,) %>%
  dplyr::select(area, cp_med, cp_iqr, mort, nutri, admis, cla, cp_mean) %>%
  distinct()

cp_data_2023_imd <- cp_data_2023 %>%
  group_by(IMD_tertile) %>%
  mutate(cp_mean = weighted.mean(cp_baseline, pop_U16),
         cp_med = median(cp_baseline),
         cp_iqr = IQR(cp_baseline),
         mort = sum(mortality)/sum(live_births)*100000,
         nutri = sum(events_34)/sum(pop_34)*100000,
         admis = sum(events_03)/sum(pop_03)*100000, 
         cla = sum(CLA_started_U16)/sum(pop_U16)*100000) %>%
  dplyr::select(IMD_tertile, cp_med, cp_iqr, mort, nutri, admis, cla, cp_mean) %>%
  distinct()


#### Plot of outcome trends before intervention period ####

cp_data_trends <- cp_data %>%
  filter(year < 2021)

cp_national_trend <- cp_data_trends %>%
  group_by(country, year) %>%
  mutate(cp_mean = weighted.mean(cp_baseline, pop_U16),
         mort = sum(mortality)/sum(live_births)*100000,
         cla = sum(CLA_started_U16)/sum(pop_U16)*100000,
         nutri = sum(events_34)/sum(pop_34)*100000,
         admis = sum(events_03)/sum(pop_03)*100000) %>%
  dplyr::select(country, year, mort, cla, nutri, admis, cp_med) %>%
  distinct() 


cp_national_trend <- cp_data_trends %>%
  group_by(country, year) %>%
  summarise(cp_mean = weighted.mean(cp_baseline, pop_U16))


cp_national_trend <-  cp_national_trend %>%
  mutate(nutri = if_else(year == 2020, NA_real_, nutri),
         admis = if_else(year == 2020, NA_real_, admis))

plot_data <- read_csv("N:/Work/ChildPoverty/Intermediate_data/plot_trend_data.csv")



ggplot(plot_data, aes(year,cases)) +
  geom_line() +
  facet_grid(rows = vars(outcome), scales = "free") +
  xlab("Year") +
  theme(strip.background = element_blank(),
        strip.placement = "outside")


#### Effect Estimates ####

# First derive estimates for anaemias and emergency admissions between 2015 and 2019

#cp_data_est <- cp_data %>%
#  filter(year <2020) %>%
 # dplyr::select(-mortality, -live_births, -pop_U16, CLA_started_U16, -IMD_rank_avg_rank, -IMD_tertile,
  #       -cp_nochange, cp_low, cp_med, cp_high) %>%
#  mutate(rate_34 = events_34/pop_34*100000,
 #        rate_03 = events_03/pop_03*100000)

# anaemias (est = 0.5304, SE = 0.06921)
#est_34 <- lmer(rate_34 ~ cp_advtr + (1|area_code), data = cp_data_est)
#display(est_34)
# emergency admissions (est = 37.74, SE = 16.97)
#est_03 <- lmer(rate_03 ~ cp_advtr + (1|area_code), data = cp_data_est)
#display(est_03) 



#### Simulation ####


# Specify standard errors for each estimate
# estimate for infant mortality 5.8 (95% CI 2.4 to 9.2); and children looked after 5.21 (95% CI 2·2–8·3)

SE_C = (8.3 - 2.2) / 3.92 
SE_M = (9.2 - 2.4) / 3.92 
SE_nutri = 0.06921
SE_admis = 16.97


set.seed(123) 

tictoc::tic()
output <- tibble() #create frame for output
#running this 1000 times (2024-2033)
for (i in 1:1000) {
  mort_effect <- rnorm(1, 5.8, SE_M)
  cla_effect <- rnorm(1, 5.2, SE_C)
  nutri_effect <- rnorm(1, 0.5304, SE_nutri)
  admis_effect <- rnorm(1, 37.74, SE_admis)
  
  result <- cp_data %>%
    lazy_dt() %>%
    pivot_longer(starts_with("cp_"), names_to = "scenario", values_to = "pov") %>% 
    group_by(area_name, scenario, IMD_tertile, IMD_rank, area_code, country) %>% #
    mutate(
      
      pov_change = pov-lag(pov),
      
      mort_change = ((pov_change*mort_effect)/100000)*live_births,
      mort_total = mort_change + mortality,
      mort_event = mortality,
      mort_pop = live_births,
      
      cla_change = ((pov_change*cla_effect)/100000)*pop_U16,
      cla_total = cla_change + CLA_started_U16,
      cla_event = CLA_started_U16,
      cla_pop = pop_U16,
      
      nutri_change = ((pov_change*nutri_effect)/100000)*pop_34, 
      nutri_total = nutri_change + events_34,
      nutri_event = events_34,
      nutri_pop = pop_34,
      
      admis_change = ((pov_change*admis_effect)/100000)*pop_03, 
      admis_total = admis_change + events_03,
      admis_event = events_03,
      admis_pop = pop_03
      
  ) %>%
    mutate(run = i) %>%
    as_tibble()
  
  output <- bind_rows(output, result)
}
tictoc::toc()

View(output) 

##  Adding adverse scenario outcomes as a column in output to use as counterfactual

output_adv <- output %>%
  filter(scenario == "cp_baseline") %>%
  dplyr::select(area_code, area_name, year, run, mort_total, cla_total, nutri_total, admis_total, mort_change, cla_change, nutri_change, admis_change) %>%
  rename(mort_total_adv = mort_total, cla_total_adv = cla_total, nutri_total_adv = nutri_total, admis_total_adv = admis_total,
         mort_change_adv = mort_change, cla_change_adv = cla_change, nutri_change_adv = nutri_change, admis_change_adv = admis_change)

#output_test <- output_adv %>%
 # filter(area_name == "Cornwall")



output_2 <- output %>%
  filter(scenario != "cp_baseline") %>%
  left_join(., output_adv, by =c("area_code", "area_name", "year", "run")) 


####
# for results:
# - change in cases associated with each scenario
# - total cases under each scenario (i.e., change + background events)
# - rate per 100 000 (i.e., total cases/pop*100000) for each scenario
# - rate per 100 000 for adverse scenario
# - rate difference
# - rate ratio

#### Base run (i.e., local authority level) ####

base_run_result <- output_2 %>%
  group_by(area_code, area_name, scenario, run) %>%
  filter(year > 2023) %>% #filter for years above 2023 
  summarise(mort_change = sum(mort_change),
            mort_total = sum(mort_total),
            mort_rate = sum((mort_total)/sum(mort_pop))*100000,
            mort_change_adv = sum(mort_change_adv),
            mort_rate_adv = (sum(mort_total_adv)/sum(mort_pop))*100000,
            mort_diff = sum(mort_change)-sum(mort_change_adv),
            mort_rr = (sum(mort_total)/sum(mort_pop))/(sum(mort_total_adv)/sum(mort_pop)),
            mort_rd = ((sum(mort_total)/sum(mort_pop))*100000)-((sum(mort_total_adv)/sum(mort_pop))*100000),
            
            cla_change = sum(cla_change),
            cla_total = sum(cla_total),
            cla_rate = (sum(cla_total)/sum(cla_pop))*100000,
            cla_change_adv = sum(cla_change_adv),
            cla_rate_adv = (sum(cla_total_adv)/sum(cla_pop))*100000,
            cla_diff = sum(cla_change)-sum(cla_change_adv),
            cla_rr = (sum(cla_total)/sum(cla_pop))/(sum(cla_total_adv)/sum(cla_pop)),
            cla_rd = ((sum(cla_total)/sum(cla_pop))*100000)-((sum(cla_total_adv)/sum(cla_pop))*100000),
            
            nutri_change = sum(nutri_change),
            nutri_total = sum(nutri_total),
            nutri_rate = (sum(nutri_total)/sum(nutri_pop))*100000,
            nutri_change_adv = sum(nutri_change_adv),
            nutri_rate_adv = (sum(nutri_total_adv)/sum(nutri_pop))*100000,
            nutri_diff = sum(nutri_change)-sum(nutri_change_adv),
            nutri_rr = (sum(nutri_total)/sum(nutri_pop))/(sum(nutri_total_adv)/sum(nutri_pop)),
            nutri_rd = ((sum(nutri_total)/sum(nutri_pop))*100000)-((sum(nutri_total_adv)/sum(nutri_pop))*100000),
            
            admis_change = sum(admis_change),
            admis_total = sum(admis_total),
            admis_rate = (sum(admis_total)/sum(admis_pop))*100000,
            admis_change_adv = sum(admis_change_adv),
            admis_rate_adv = (sum(admis_total_adv)/sum(admis_pop))*100000,
            admis_diff = sum(admis_change)-sum(admis_change_adv),
            admis_rr = (sum(admis_total)/sum(admis_pop))/(sum(admis_total_adv)/sum(admis_pop)),
            admis_rd = ((sum(admis_total)/sum(admis_pop))*100000)-((sum(admis_total_adv)/sum(admis_pop))*100000),
            .groups = "drop") %>% 
  # Generate the summary data (of 1000 runs)
  group_by(area_code, area_name, scenario) %>%
  summarise(
    mort_change_median = median(mort_change, na.rm = TRUE),
    mort_change_pct025 = quantile(mort_change, 0.025, na.rm = TRUE),
    mort_change_pct975 = quantile(mort_change, 0.975, na.rm = TRUE),
    
    mort_total_median = median(mort_total, na.rm = TRUE),
    mort_total_pct025 = quantile(mort_total, 0.025, na.rm = TRUE),
    mort_total_pct975 = quantile(mort_total, 0.975, na.rm = TRUE),
    
    mort_rate_median = median(mort_rate, na.rm = TRUE),
    mort_rate_pct025 = quantile(mort_rate, 0.025, na.rm = TRUE),
    mort_rate_pct975 = quantile(mort_rate, 0.975, na.rm = TRUE),
    
    mort_change_adv_median = median(mort_change_adv, na.rm = TRUE),
    mort_change_adv_pct025 = quantile(mort_change_adv, 0.025, na.rm = TRUE),
    mort_change_adv_pct975 = quantile(mort_change_adv, 0.975, na.rm = TRUE),
    
    mort_rate_adv_median = median(mort_rate_adv, na.rm = TRUE),
    mort_rate_adv_pct025 = quantile(mort_rate_adv, 0.025, na.rm = TRUE),
    mort_rate_adv_pct975 = quantile(mort_rate_adv, 0.975, na.rm = TRUE),
    
    mort_diff_median = median(mort_diff, na.rm = TRUE),
    mort_diff_pct025 = quantile(mort_diff, 0.025, na.rm = TRUE),
    mort_diff_pct975 = quantile(mort_diff, 0.975, na.rm = TRUE),
    
    mort_rr_median = median(mort_rr, na.rm = TRUE),
    mort_rr_pct025 = quantile(mort_rr, 0.025, na.rm = TRUE),
    mort_rr_pct975 = quantile(mort_rr, 0.975, na.rm = TRUE),
    
    mort_rd_median = median(mort_rd, na.rm = TRUE),
    mort_rd_pct025 = quantile(mort_rd, 0.025, na.rm = TRUE),
    mort_rd_pct975 = quantile(mort_rd, 0.975, na.rm = TRUE),
    
    cla_change_median = median(cla_change, na.rm = TRUE),
    cla_change_pct025 = quantile(cla_change, 0.025, na.rm = TRUE),
    cla_change_pct975 = quantile(cla_change, 0.975, na.rm = TRUE),
    
    cla_total_median = median(cla_total, na.rm = TRUE),
    cla_total_pct025 = quantile(cla_total, 0.025, na.rm = TRUE),
    cla_total_pct975 = quantile(cla_total, 0.975, na.rm = TRUE),
    
    cla_rate_median = median(cla_rate, na.rm = TRUE),
    cla_rate_pct025 = quantile(cla_rate, 0.025, na.rm = TRUE),
    cla_rate_pct975 = quantile(cla_rate, 0.975, na.rm = TRUE),
    
    cla_change_adv_median = median(cla_change_adv, na.rm = TRUE),
    cla_change_adv_pct025 = quantile(cla_change_adv, 0.025, na.rm = TRUE),
    cla_change_adv_pct975 = quantile(cla_change_adv, 0.975, na.rm = TRUE),
    
    cla_rate_adv_median = median(cla_rate_adv, na.rm = TRUE),
    cla_rate_adv_pct025 = quantile(cla_rate_adv, 0.025, na.rm = TRUE),
    cla_rate_adv_pct975 = quantile(cla_rate_adv, 0.975, na.rm = TRUE),
    
    cla_diff_median = median(cla_diff, na.rm = TRUE),
    cla_diff_pct025 = quantile(cla_diff, 0.025, na.rm = TRUE),
    cla_diff_pct975 = quantile(cla_diff, 0.975, na.rm = TRUE),
    
    cla_rr_median = median(cla_rr, na.rm = TRUE),
    cla_rr_pct025 = quantile(cla_rr, 0.025, na.rm = TRUE),
    cla_rr_pct975 = quantile(cla_rr, 0.975, na.rm = TRUE),
    
    cla_rd_median = median(cla_rd, na.rm = TRUE),
    cla_rd_pct025 = quantile(cla_rd, 0.025, na.rm = TRUE),
    cla_rd_pct975 = quantile(cla_rd, 0.975, na.rm = TRUE),
    
    nutri_change_median = median(nutri_change, na.rm = TRUE),
    nutri_change_pct025 = quantile(nutri_change, 0.025, na.rm = TRUE),
    nutri_change_pct975 = quantile(nutri_change, 0.975, na.rm = TRUE),
    
    nutri_total_median = median(nutri_total, na.rm = TRUE),
    nutri_total_pct025 = quantile(nutri_total, 0.025, na.rm = TRUE),
    nutri_total_pct975 = quantile(nutri_total, 0.975, na.rm = TRUE),
    
    nutri_rate_median = median(nutri_rate, na.rm = TRUE),
    nutri_rate_pct025 = quantile(nutri_rate, 0.025, na.rm = TRUE),
    nutri_rate_pct975 = quantile(nutri_rate, 0.975, na.rm = TRUE),
    
    nutri_diff_median = median(nutri_diff, na.rm = TRUE),
    nutri_diff_pct025 = quantile(nutri_diff, 0.025, na.rm = TRUE),
    nutri_diff_pct975 = quantile(nutri_diff, 0.975, na.rm = TRUE),
    
    nutri_change_adv_median = median(nutri_change_adv, na.rm = TRUE),
    nutri_change_adv_pct025 = quantile(nutri_change_adv, 0.025, na.rm = TRUE),
    nutri_change_adv_pct975 = quantile(nutri_change_adv, 0.975, na.rm = TRUE),
  
    nutri_rate_adv_median = median(nutri_rate_adv, na.rm = TRUE),
    nutri_rate_adv_pct025 = quantile(nutri_rate_adv, 0.025, na.rm = TRUE),
    nutri_rate_adv_pct975 = quantile(nutri_rate_adv, 0.975, na.rm = TRUE),
    
    nutri_rr_median = median(nutri_rr, na.rm = TRUE),
    nutri_rr_pct025 = quantile(nutri_rr, 0.025, na.rm = TRUE),
    nutri_rr_pct975 = quantile(nutri_rr, 0.975, na.rm = TRUE),
    
    nutri_rd_median = median(nutri_rd, na.rm = TRUE),
    nutri_rd_pct025 = quantile(nutri_rd, 0.025, na.rm = TRUE),
    nutri_rd_pct975 = quantile(nutri_rd, 0.975, na.rm = TRUE),
    
    admis_change_median = median(admis_change, na.rm = TRUE),
    admis_change_pct025 = quantile(admis_change, 0.025, na.rm = TRUE),
    admis_change_pct975 = quantile(admis_change, 0.975, na.rm = TRUE),
    
    admis_total_median = median(admis_total, na.rm = TRUE),
    admis_total_pct025 = quantile(admis_total, 0.025, na.rm = TRUE),
    admis_total_pct975 = quantile(admis_total, 0.975, na.rm = TRUE),
    
    admis_rate_median = median(admis_rate, na.rm = TRUE),
    admis_rate_pct025 = quantile(admis_rate, 0.025, na.rm = TRUE),
    admis_rate_pct975 = quantile(admis_rate, 0.975, na.rm = TRUE),
    
    admis_change_adv_median = median(admis_change_adv, na.rm = TRUE),
    admis_change_adv_pct025 = quantile(admis_change_adv, 0.025, na.rm = TRUE),
    admis_change_adv_pct975 = quantile(admis_change_adv, 0.975, na.rm = TRUE),
    
    admis_rate_adv_median = median(admis_rate_adv, na.rm = TRUE),
    admis_rate_adv_pct025 = quantile(admis_rate_adv, 0.025, na.rm = TRUE),
    admis_rate_adv_pct975 = quantile(admis_rate_adv, 0.975, na.rm = TRUE),
    
    admis_diff_median = median(admis_diff, na.rm = TRUE),
    admis_diff_pct025 = quantile(admis_diff, 0.025, na.rm = TRUE),
    admis_diff_pct975 = quantile(admis_diff, 0.975, na.rm = TRUE),
    
    admis_rr_median = median(admis_rr, na.rm = TRUE),
    admis_rr_pct025 = quantile(admis_rr, 0.025, na.rm = TRUE),
    admis_rr_pct975 = quantile(admis_rr, 0.975, na.rm = TRUE),
    
    admis_rd_median = median(admis_rd, na.rm = TRUE),
    admis_rd_pct025 = quantile(admis_rd, 0.025, na.rm = TRUE),
    admis_rd_pct975 = quantile(admis_rd, 0.975, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    mort_change_ci = sprintf("%.0f (%.0f, %.0f)", mort_change_median, mort_change_pct025, mort_change_pct975),
    mort_total_ci = sprintf("%.0f (%.0f, %.0f)", mort_total_median, mort_total_pct025, mort_total_pct975),
    mort_rate_ci = sprintf("%.0f (%.0f, %.0f)", mort_rate_median, mort_rate_pct025, mort_rate_pct975),
    mort_change_adv_ci = sprintf("%.0f (%.0f, %.0f)", mort_change_adv_median, mort_change_adv_pct025, mort_change_adv_pct975),
    mort_rate_adv_ci = sprintf("%.3f (%.3f, %.3f)", mort_rate_adv_median, mort_rate_adv_pct025, mort_rate_adv_pct975),
    mort_diff_ci = sprintf("%.0f (%.0f, %.0f)", mort_diff_median, mort_diff_pct025, mort_diff_pct975),
    mort_rr_ci = sprintf("%.3f (%.3f, %.3f)", mort_rr_median, mort_rr_pct025, mort_rr_pct975),
    mort_rd_ci = sprintf("%.3f (%.3f, %.3f)", mort_rd_median, mort_rd_pct025, mort_rd_pct975),
    
    cla_change_ci = sprintf("%.0f (%.0f, %.0f)", cla_change_median, cla_change_pct025, cla_change_pct975),
    cla_total_ci = sprintf("%.0f (%.0f, %.0f)", cla_total_median, cla_total_pct025, cla_total_pct975),
    cla_rate_ci = sprintf("%.0f (%.0f, %.0f)", cla_rate_median, cla_rate_pct025, cla_rate_pct975),
    cla_change_adv_ci = sprintf("%.0f (%.0f, %.0f)", cla_change_adv_median, cla_change_adv_pct025, cla_change_adv_pct975),
    cla_rate_adv_ci = sprintf("%.3f (%.3f, %.3f)", cla_rate_adv_median, cla_rate_adv_pct025, cla_rate_adv_pct975),
    cla_diff_ci = sprintf("%.0f (%.0f, %.0f)", cla_diff_median, cla_diff_pct025, cla_diff_pct975),
    cla_rr_ci = sprintf("%.3f (%.3f, %.3f)", cla_rr_median, cla_rr_pct025, cla_rr_pct975),
    cla_rd_ci = sprintf("%.3f (%.3f, %.3f)", cla_rd_median, cla_rd_pct025, cla_rd_pct975),
    
    nutri_change_ci = sprintf("%.0f (%.0f, %.0f)", nutri_change_median, nutri_change_pct025, nutri_change_pct975),
    nutri_total_ci = sprintf("%.0f (%.0f, %.0f)", nutri_total_median, nutri_total_pct025, nutri_total_pct975),
    nutri_rate_ci = sprintf("%.0f (%.0f, %.0f)", nutri_rate_median, nutri_rate_pct025, nutri_rate_pct975),
    nutri_change_adv_ci = sprintf("%.0f (%.0f, %.0f)", nutri_change_adv_median, nutri_change_adv_pct025, nutri_change_adv_pct975),
    nutri_rate_adv_ci = sprintf("%.3f (%.3f, %.3f)", nutri_rate_adv_median, nutri_rate_adv_pct025, nutri_rate_adv_pct975),
    nutri_diff_ci = sprintf("%.0f (%.0f, %.0f)", nutri_diff_median, nutri_diff_pct025, nutri_diff_pct975),
    nutri_rr_ci = sprintf("%.3f (%.3f, %.3f)", nutri_rr_median, nutri_rr_pct025, nutri_rr_pct975),
    nutri_rd_ci = sprintf("%.3f (%.3f, %.3f)", nutri_rd_median, nutri_rd_pct025, nutri_rd_pct975),
    
    admis_change_ci = sprintf("%.0f (%.0f, %.0f)", admis_change_median, admis_change_pct025, admis_change_pct975),
    admis_total_ci = sprintf("%.0f (%.0f, %.0f)", admis_total_median, admis_total_pct025, admis_total_pct975),
    admis_rate_ci = sprintf("%.0f (%.0f, %.0f)", admis_rate_median, admis_rate_pct025, admis_rate_pct975),
    admis_change_adv_ci = sprintf("%.0f (%.0f, %.0f)", admis_change_adv_median, admis_change_adv_pct025, admis_change_adv_pct975),
    admis_rate_adv_ci = sprintf("%.3f (%.3f, %.3f)", admis_rate_adv_median, admis_rate_adv_pct025, admis_rate_adv_pct975),
    admis_diff_ci = sprintf("%.0f (%.0f, %.0f)", admis_diff_median, admis_diff_pct025, admis_diff_pct975),
    admis_rr_ci = sprintf("%.3f (%.3f, %.3f)", admis_rr_median, admis_rr_pct025, admis_rr_pct975),
    admis_rd_ci = sprintf("%.3f (%.3f, %.3f)", admis_rd_median, admis_rd_pct025, admis_rd_pct975)
    
  ) 
view(base_run_result)


base_run_result <- base_run_result %>%
  group_by(area_code) %>%
  arrange(factor(scenario, levels = c("cp_low", "cp_med", "cp_high")))



##### IMD run  ####
IMD_run_result <- output_2 %>% 
  group_by(area_code, area_name, scenario, IMD_tertile, run) %>%
  filter(year > 2023) %>% #filter for years above 2023 
  summarise(mort_change = sum(mort_change),
            mort_total = sum(mort_total),
            mort_change_adv = sum(mort_change_adv),
            mort_total_adv = sum(mort_total_adv),
            mort_pop = sum(mort_pop), 
            
            cla_change =  sum(cla_change),
            cla_total = sum(cla_total),
            cla_change_adv = sum(cla_change_adv),
            cla_total_adv = sum(cla_total_adv),
            cla_pop= sum(cla_pop),
            
            nutri_change = sum(nutri_change),  
            nutri_total = sum(nutri_total),
            nutri_change_adv = sum(nutri_change_adv),
            nutri_total_adv = sum(nutri_total_adv),
            nutri_pop= sum(nutri_pop),
            
            admis_change = sum(admis_change),
            admis_total = sum(admis_total),
            admis_change_adv = sum(admis_change_adv),
            admis_total_adv = sum(admis_total_adv),
            admis_pop= sum(admis_pop),
            
            .groups = "drop") %>% 
  # Generate the summary data (of 1000 runs)
  group_by(IMD_tertile, scenario, run) %>%
  summarise(mort_change = sum(mort_change),
            mort_total = sum(mort_total),
            mort_rate = sum((mort_total)/sum(mort_pop))*100000,
            mort_change_adv = sum(mort_change_adv),
            mort_rate_adv = (sum(mort_total_adv)/sum(mort_pop))*100000,
            mort_diff = sum(mort_change)-sum(mort_change_adv),
            mort_rr = (sum(mort_total)/sum(mort_pop))/(sum(mort_total_adv)/sum(mort_pop)),
            mort_rd = ((sum(mort_total)/sum(mort_pop))*100000)-((sum(mort_total_adv)/sum(mort_pop))*100000),
            
            cla_change = sum(cla_change),
            cla_total = sum(cla_total),
            cla_rate = (sum(cla_total)/sum(cla_pop))*100000,
            cla_change_adv = sum(cla_change_adv),
            cla_rate_adv = (sum(cla_total_adv)/sum(cla_pop))*100000,
            cla_diff = sum(cla_change)-sum(cla_change_adv),
            cla_rr = (sum(cla_total)/sum(cla_pop))/(sum(cla_total_adv)/sum(cla_pop)),
            cla_rd = ((sum(cla_total)/sum(cla_pop))*100000)-((sum(cla_total_adv)/sum(cla_pop))*100000),
            
            nutri_change = sum(nutri_change),
            nutri_total = sum(nutri_total),
            nutri_rate = (sum(nutri_total)/sum(nutri_pop))*100000,
            nutri_change_adv = sum(nutri_change_adv),
            nutri_rate_adv = (sum(nutri_total_adv)/sum(nutri_pop))*100000,
            nutri_diff = sum(nutri_change)-sum(nutri_change_adv),
            nutri_rr = (sum(nutri_total)/sum(nutri_pop))/(sum(nutri_total_adv)/sum(nutri_pop)),
            nutri_rd = ((sum(nutri_total)/sum(nutri_pop))*100000)-((sum(nutri_total_adv)/sum(nutri_pop))*100000),
            
            admis_change = sum(admis_change),
            admis_total = sum(admis_total),
            admis_rate = (sum(admis_total)/sum(admis_pop))*100000,
            admis_change_adv = sum(admis_change_adv),
            admis_rate_adv = (sum(admis_total_adv)/sum(admis_pop))*100000,
            admis_diff = sum(admis_change)-sum(admis_change_adv),
            admis_rr = (sum(admis_total)/sum(admis_pop))/(sum(admis_total_adv)/sum(admis_pop)),
            admis_rd = ((sum(admis_total)/sum(admis_pop))*100000)-((sum(admis_total_adv)/sum(admis_pop))*100000),
            .groups = "drop") %>% #sum mort change by each group sum mort change
  group_by(IMD_tertile, scenario) %>%
  summarise(
    mort_change_median = median(mort_change, na.rm = TRUE),
    mort_change_pct025 = quantile(mort_change, 0.025, na.rm = TRUE),
    mort_change_pct975 = quantile(mort_change, 0.975, na.rm = TRUE),
    
    mort_total_median = median(mort_total, na.rm = TRUE),
    mort_total_pct025 = quantile(mort_total, 0.025, na.rm = TRUE),
    mort_total_pct975 = quantile(mort_total, 0.975, na.rm = TRUE),
    
    mort_rate_median = median(mort_rate, na.rm = TRUE),
    mort_rate_pct025 = quantile(mort_rate, 0.025, na.rm = TRUE),
    mort_rate_pct975 = quantile(mort_rate, 0.975, na.rm = TRUE),
    
    mort_change_adv_median = median(mort_change_adv, na.rm = TRUE),
    mort_change_adv_pct025 = quantile(mort_change_adv, 0.025, na.rm = TRUE),
    mort_change_adv_pct975 = quantile(mort_change_adv, 0.975, na.rm = TRUE),
    
    mort_rate_adv_median = median(mort_rate_adv, na.rm = TRUE),
    mort_rate_adv_pct025 = quantile(mort_rate_adv, 0.025, na.rm = TRUE),
    mort_rate_adv_pct975 = quantile(mort_rate_adv, 0.975, na.rm = TRUE),
    
    mort_diff_median = median(mort_diff, na.rm = TRUE),
    mort_diff_pct025 = quantile(mort_diff, 0.025, na.rm = TRUE),
    mort_diff_pct975 = quantile(mort_diff, 0.975, na.rm = TRUE),
    
    mort_rr_median = median(mort_rr, na.rm = TRUE),
    mort_rr_pct025 = quantile(mort_rr, 0.025, na.rm = TRUE),
    mort_rr_pct975 = quantile(mort_rr, 0.975, na.rm = TRUE),
    
    mort_rd_median = median(mort_rd, na.rm = TRUE),
    mort_rd_pct025 = quantile(mort_rd, 0.025, na.rm = TRUE),
    mort_rd_pct975 = quantile(mort_rd, 0.975, na.rm = TRUE),
    
    cla_change_median = median(cla_change, na.rm = TRUE),
    cla_change_pct025 = quantile(cla_change, 0.025, na.rm = TRUE),
    cla_change_pct975 = quantile(cla_change, 0.975, na.rm = TRUE),
    
    cla_total_median = median(cla_total, na.rm = TRUE),
    cla_total_pct025 = quantile(cla_total, 0.025, na.rm = TRUE),
    cla_total_pct975 = quantile(cla_total, 0.975, na.rm = TRUE),
    
    cla_rate_median = median(cla_rate, na.rm = TRUE),
    cla_rate_pct025 = quantile(cla_rate, 0.025, na.rm = TRUE),
    cla_rate_pct975 = quantile(cla_rate, 0.975, na.rm = TRUE),
    
    cla_change_adv_median = median(cla_change_adv, na.rm = TRUE),
    cla_change_adv_pct025 = quantile(cla_change_adv, 0.025, na.rm = TRUE),
    cla_change_adv_pct975 = quantile(cla_change_adv, 0.975, na.rm = TRUE),
    
    cla_rate_adv_median = median(cla_rate_adv, na.rm = TRUE),
    cla_rate_adv_pct025 = quantile(cla_rate_adv, 0.025, na.rm = TRUE),
    cla_rate_adv_pct975 = quantile(cla_rate_adv, 0.975, na.rm = TRUE),
    
    cla_diff_median = median(cla_diff, na.rm = TRUE),
    cla_diff_pct025 = quantile(cla_diff, 0.025, na.rm = TRUE),
    cla_diff_pct975 = quantile(cla_diff, 0.975, na.rm = TRUE),
    
    cla_rr_median = median(cla_rr, na.rm = TRUE),
    cla_rr_pct025 = quantile(cla_rr, 0.025, na.rm = TRUE),
    cla_rr_pct975 = quantile(cla_rr, 0.975, na.rm = TRUE),
    
    cla_rd_median = median(cla_rd, na.rm = TRUE),
    cla_rd_pct025 = quantile(cla_rd, 0.025, na.rm = TRUE),
    cla_rd_pct975 = quantile(cla_rd, 0.975, na.rm = TRUE),
    
    nutri_change_median = median(nutri_change, na.rm = TRUE),
    nutri_change_pct025 = quantile(nutri_change, 0.025, na.rm = TRUE),
    nutri_change_pct975 = quantile(nutri_change, 0.975, na.rm = TRUE),
    
    nutri_total_median = median(nutri_total, na.rm = TRUE),
    nutri_total_pct025 = quantile(nutri_total, 0.025, na.rm = TRUE),
    nutri_total_pct975 = quantile(nutri_total, 0.975, na.rm = TRUE),
    
    nutri_rate_median = median(nutri_rate, na.rm = TRUE),
    nutri_rate_pct025 = quantile(nutri_rate, 0.025, na.rm = TRUE),
    nutri_rate_pct975 = quantile(nutri_rate, 0.975, na.rm = TRUE),
    
    nutri_diff_median = median(nutri_diff, na.rm = TRUE),
    nutri_diff_pct025 = quantile(nutri_diff, 0.025, na.rm = TRUE),
    nutri_diff_pct975 = quantile(nutri_diff, 0.975, na.rm = TRUE),
    
    nutri_change_adv_median = median(nutri_change_adv, na.rm = TRUE),
    nutri_change_adv_pct025 = quantile(nutri_change_adv, 0.025, na.rm = TRUE),
    nutri_change_adv_pct975 = quantile(nutri_change_adv, 0.975, na.rm = TRUE),
    
    nutri_rate_adv_median = median(nutri_rate_adv, na.rm = TRUE),
    nutri_rate_adv_pct025 = quantile(nutri_rate_adv, 0.025, na.rm = TRUE),
    nutri_rate_adv_pct975 = quantile(nutri_rate_adv, 0.975, na.rm = TRUE),
    
    nutri_rr_median = median(nutri_rr, na.rm = TRUE),
    nutri_rr_pct025 = quantile(nutri_rr, 0.025, na.rm = TRUE),
    nutri_rr_pct975 = quantile(nutri_rr, 0.975, na.rm = TRUE),
    
    nutri_rd_median = median(nutri_rd, na.rm = TRUE),
    nutri_rd_pct025 = quantile(nutri_rd, 0.025, na.rm = TRUE),
    nutri_rd_pct975 = quantile(nutri_rd, 0.975, na.rm = TRUE),
    
    admis_change_median = median(admis_change, na.rm = TRUE),
    admis_change_pct025 = quantile(admis_change, 0.025, na.rm = TRUE),
    admis_change_pct975 = quantile(admis_change, 0.975, na.rm = TRUE),
    
    admis_total_median = median(admis_total, na.rm = TRUE),
    admis_total_pct025 = quantile(admis_total, 0.025, na.rm = TRUE),
    admis_total_pct975 = quantile(admis_total, 0.975, na.rm = TRUE),
    
    admis_rate_median = median(admis_rate, na.rm = TRUE),
    admis_rate_pct025 = quantile(admis_rate, 0.025, na.rm = TRUE),
    admis_rate_pct975 = quantile(admis_rate, 0.975, na.rm = TRUE),
    
    admis_change_adv_median = median(admis_change_adv, na.rm = TRUE),
    admis_change_adv_pct025 = quantile(admis_change_adv, 0.025, na.rm = TRUE),
    admis_change_adv_pct975 = quantile(admis_change_adv, 0.975, na.rm = TRUE),
    
    admis_rate_adv_median = median(admis_rate_adv, na.rm = TRUE),
    admis_rate_adv_pct025 = quantile(admis_rate_adv, 0.025, na.rm = TRUE),
    admis_rate_adv_pct975 = quantile(admis_rate_adv, 0.975, na.rm = TRUE),
    
    admis_diff_median = median(admis_diff, na.rm = TRUE),
    admis_diff_pct025 = quantile(admis_diff, 0.025, na.rm = TRUE),
    admis_diff_pct975 = quantile(admis_diff, 0.975, na.rm = TRUE),
    
    admis_rr_median = median(admis_rr, na.rm = TRUE),
    admis_rr_pct025 = quantile(admis_rr, 0.025, na.rm = TRUE),
    admis_rr_pct975 = quantile(admis_rr, 0.975, na.rm = TRUE),
    
    admis_rd_median = median(admis_rd, na.rm = TRUE),
    admis_rd_pct025 = quantile(admis_rd, 0.025, na.rm = TRUE),
    admis_rd_pct975 = quantile(admis_rd, 0.975, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    mort_change_ci = sprintf("%.0f (%.0f, %.0f)", mort_change_median, mort_change_pct025, mort_change_pct975),
    mort_total_ci = sprintf("%.0f (%.0f, %.0f)", mort_total_median, mort_total_pct025, mort_total_pct975),
    mort_rate_ci = sprintf("%.0f (%.0f, %.0f)", mort_rate_median, mort_rate_pct025, mort_rate_pct975),
    mort_change_adv_ci = sprintf("%.0f (%.0f, %.0f)", mort_change_adv_median, mort_change_adv_pct025, mort_change_adv_pct975),
    mort_rate_adv_ci = sprintf("%.3f (%.3f, %.3f)", mort_rate_adv_median, mort_rate_adv_pct025, mort_rate_adv_pct975),
    mort_diff_ci = sprintf("%.0f (%.0f, %.0f)", mort_diff_median, mort_diff_pct025, mort_diff_pct975),
    mort_rr_ci = sprintf("%.3f (%.3f, %.3f)", mort_rr_median, mort_rr_pct025, mort_rr_pct975),
    mort_rd_ci = sprintf("%.3f (%.3f, %.3f)", mort_rd_median, mort_rd_pct025, mort_rd_pct975),
    
    cla_change_ci = sprintf("%.0f (%.0f, %.0f)", cla_change_median, cla_change_pct025, cla_change_pct975),
    cla_total_ci = sprintf("%.0f (%.0f, %.0f)", cla_total_median, cla_total_pct025, cla_total_pct975),
    cla_rate_ci = sprintf("%.0f (%.0f, %.0f)", cla_rate_median, cla_rate_pct025, cla_rate_pct975),
    cla_change_adv_ci = sprintf("%.0f (%.0f, %.0f)", cla_change_adv_median, cla_change_adv_pct025, cla_change_adv_pct975),
    cla_rate_adv_ci = sprintf("%.3f (%.3f, %.3f)", cla_rate_adv_median, cla_rate_adv_pct025, cla_rate_adv_pct975),
    cla_diff_ci = sprintf("%.0f (%.0f, %.0f)", cla_diff_median, cla_diff_pct025, cla_diff_pct975),
    cla_rr_ci = sprintf("%.3f (%.3f, %.3f)", cla_rr_median, cla_rr_pct025, cla_rr_pct975),
    cla_rd_ci = sprintf("%.3f (%.3f, %.3f)", cla_rd_median, cla_rd_pct025, cla_rd_pct975),
    
    nutri_change_ci = sprintf("%.0f (%.0f, %.0f)", nutri_change_median, nutri_change_pct025, nutri_change_pct975),
    nutri_total_ci = sprintf("%.0f (%.0f, %.0f)", nutri_total_median, nutri_total_pct025, nutri_total_pct975),
    nutri_rate_ci = sprintf("%.0f (%.0f, %.0f)", nutri_rate_median, nutri_rate_pct025, nutri_rate_pct975),
    nutri_change_adv_ci = sprintf("%.0f (%.0f, %.0f)", nutri_change_adv_median, nutri_change_adv_pct025, nutri_change_adv_pct975),
    nutri_rate_adv_ci = sprintf("%.3f (%.3f, %.3f)", nutri_rate_adv_median, nutri_rate_adv_pct025, nutri_rate_adv_pct975),
    nutri_diff_ci = sprintf("%.0f (%.0f, %.0f)", nutri_diff_median, nutri_diff_pct025, nutri_diff_pct975),
    nutri_rr_ci = sprintf("%.3f (%.3f, %.3f)", nutri_rr_median, nutri_rr_pct025, nutri_rr_pct975),
    nutri_rd_ci = sprintf("%.3f (%.3f, %.3f)", nutri_rd_median, nutri_rd_pct025, nutri_rd_pct975),
    
    admis_change_ci = sprintf("%.0f (%.0f, %.0f)", admis_change_median, admis_change_pct025, admis_change_pct975),
    admis_total_ci = sprintf("%.0f (%.0f, %.0f)", admis_total_median, admis_total_pct025, admis_total_pct975),
    admis_rate_ci = sprintf("%.0f (%.0f, %.0f)", admis_rate_median, admis_rate_pct025, admis_rate_pct975),
    admis_change_adv_ci = sprintf("%.0f (%.0f, %.0f)", admis_change_adv_median, admis_change_adv_pct025, admis_change_adv_pct975),
    admis_rate_adv_ci = sprintf("%.3f (%.3f, %.3f)", admis_rate_adv_median, admis_rate_adv_pct025, admis_rate_adv_pct975),
    admis_diff_ci = sprintf("%.0f (%.0f, %.0f)", admis_diff_median, admis_diff_pct025, admis_diff_pct975),
    admis_rr_ci = sprintf("%.3f (%.3f, %.3f)", admis_rr_median, admis_rr_pct025, admis_rr_pct975),
    admis_rd_ci = sprintf("%.3f (%.3f, %.3f)", admis_rd_median, admis_rd_pct025, admis_rd_pct975)
  ) 
view(IMD_run_result) #discuss results

IMD_run_result <- IMD_run_result %>%
  group_by(IMD_tertile) %>%
  arrange(factor(scenario, levels = c("cp_low", "cp_med", "cp_high")))

#### Area run ####
area_run_result <- output_2 %>%
  group_by(area_code, area_name, scenario, area, run) %>%
  filter(year > 2023) %>% #filter for years above 2023 
  summarise(mort_change = sum(mort_change),
            mort_total = sum(mort_total),
            mort_change_adv = sum(mort_change_adv),
            mort_total_adv = sum(mort_total_adv),
            mort_pop = sum(mort_pop), 
            
            cla_change =  sum(cla_change),
            cla_total = sum(cla_total),
            cla_change_adv = sum(cla_change_adv),
            cla_total_adv = sum(cla_total_adv),
            cla_pop= sum(cla_pop),
            
            nutri_change = sum(nutri_change),  
            nutri_total = sum(nutri_total),
            nutri_change_adv = sum(nutri_change_adv),
            nutri_total_adv = sum(nutri_total_adv),
            nutri_pop= sum(nutri_pop),
            
            admis_change = sum(admis_change),
            admis_total = sum(admis_total),
            admis_change_adv = sum(admis_change_adv),
            admis_total_adv = sum(admis_total_adv),
            admis_pop= sum(admis_pop),
            .groups = "drop") %>%
  group_by(area, scenario, run) %>% 
  summarise(mort_change = sum(mort_change),
            mort_total = sum(mort_total),
            mort_rate = sum((mort_total)/sum(mort_pop))*100000,
            mort_change_adv = sum(mort_change_adv),
            mort_rate_adv = (sum(mort_total_adv)/sum(mort_pop))*100000,
            mort_diff = sum(mort_change)-sum(mort_change_adv),
            mort_rr = (sum(mort_total)/sum(mort_pop))/(sum(mort_total_adv)/sum(mort_pop)),
            mort_rd = ((sum(mort_total)/sum(mort_pop))*100000)-((sum(mort_total_adv)/sum(mort_pop))*100000),
            
            cla_change = sum(cla_change),
            cla_total = sum(cla_total),
            cla_rate = (sum(cla_total)/sum(cla_pop))*100000,
            cla_change_adv = sum(cla_change_adv),
            cla_rate_adv = (sum(cla_total_adv)/sum(cla_pop))*100000,
            cla_diff = sum(cla_change)-sum(cla_change_adv),
            cla_rr = (sum(cla_total)/sum(cla_pop))/(sum(cla_total_adv)/sum(cla_pop)),
            cla_rd = ((sum(cla_total)/sum(cla_pop))*100000)-((sum(cla_total_adv)/sum(cla_pop))*100000),
            
            nutri_change = sum(nutri_change),
            nutri_total = sum(nutri_total),
            nutri_rate = (sum(nutri_total)/sum(nutri_pop))*100000,
            nutri_change_adv = sum(nutri_change_adv),
            nutri_rate_adv = (sum(nutri_total_adv)/sum(nutri_pop))*100000,
            nutri_diff = sum(nutri_change)-sum(nutri_change_adv),
            nutri_rr = (sum(nutri_total)/sum(nutri_pop))/(sum(nutri_total_adv)/sum(nutri_pop)),
            nutri_rd = ((sum(nutri_total)/sum(nutri_pop))*100000)-((sum(nutri_total_adv)/sum(nutri_pop))*100000),
            
            admis_change = sum(admis_change),
            admis_total = sum(admis_total),
            admis_rate = (sum(admis_total)/sum(admis_pop))*100000,
            admis_change_adv = sum(admis_change_adv),
            admis_rate_adv = (sum(admis_total_adv)/sum(admis_pop))*100000,
            admis_diff = sum(admis_change)-sum(admis_change_adv),
            admis_rr = (sum(admis_total)/sum(admis_pop))/(sum(admis_total_adv)/sum(admis_pop)),
            admis_rd = ((sum(admis_total)/sum(admis_pop))*100000)-((sum(admis_total_adv)/sum(admis_pop))*100000),
            .groups = "drop") %>% 
  group_by(area, scenario) %>%
  summarise(mort_change_median = median(mort_change, na.rm = TRUE),
            mort_change_pct025 = quantile(mort_change, 0.025, na.rm = TRUE),
            mort_change_pct975 = quantile(mort_change, 0.975, na.rm = TRUE),
            
            mort_total_median = median(mort_total, na.rm = TRUE),
            mort_total_pct025 = quantile(mort_total, 0.025, na.rm = TRUE),
            mort_total_pct975 = quantile(mort_total, 0.975, na.rm = TRUE),
            
            mort_rate_median = median(mort_rate, na.rm = TRUE),
            mort_rate_pct025 = quantile(mort_rate, 0.025, na.rm = TRUE),
            mort_rate_pct975 = quantile(mort_rate, 0.975, na.rm = TRUE),
            
            mort_change_adv_median = median(mort_change_adv, na.rm = TRUE),
            mort_change_adv_pct025 = quantile(mort_change_adv, 0.025, na.rm = TRUE),
            mort_change_adv_pct975 = quantile(mort_change_adv, 0.975, na.rm = TRUE),
            
            mort_rate_adv_median = median(mort_rate_adv, na.rm = TRUE),
            mort_rate_adv_pct025 = quantile(mort_rate_adv, 0.025, na.rm = TRUE),
            mort_rate_adv_pct975 = quantile(mort_rate_adv, 0.975, na.rm = TRUE),
            
            mort_diff_median = median(mort_diff, na.rm = TRUE),
            mort_diff_pct025 = quantile(mort_diff, 0.025, na.rm = TRUE),
            mort_diff_pct975 = quantile(mort_diff, 0.975, na.rm = TRUE),
            
            mort_rr_median = median(mort_rr, na.rm = TRUE),
            mort_rr_pct025 = quantile(mort_rr, 0.025, na.rm = TRUE),
            mort_rr_pct975 = quantile(mort_rr, 0.975, na.rm = TRUE),
            
            mort_rd_median = median(mort_rd, na.rm = TRUE),
            mort_rd_pct025 = quantile(mort_rd, 0.025, na.rm = TRUE),
            mort_rd_pct975 = quantile(mort_rd, 0.975, na.rm = TRUE),
            
            cla_change_median = median(cla_change, na.rm = TRUE),
            cla_change_pct025 = quantile(cla_change, 0.025, na.rm = TRUE),
            cla_change_pct975 = quantile(cla_change, 0.975, na.rm = TRUE),
            
            cla_total_median = median(cla_total, na.rm = TRUE),
            cla_total_pct025 = quantile(cla_total, 0.025, na.rm = TRUE),
            cla_total_pct975 = quantile(cla_total, 0.975, na.rm = TRUE),
            
            cla_rate_median = median(cla_rate, na.rm = TRUE),
            cla_rate_pct025 = quantile(cla_rate, 0.025, na.rm = TRUE),
            cla_rate_pct975 = quantile(cla_rate, 0.975, na.rm = TRUE),
            
            cla_change_adv_median = median(cla_change_adv, na.rm = TRUE),
            cla_change_adv_pct025 = quantile(cla_change_adv, 0.025, na.rm = TRUE),
            cla_change_adv_pct975 = quantile(cla_change_adv, 0.975, na.rm = TRUE),
            
            cla_rate_adv_median = median(cla_rate_adv, na.rm = TRUE),
            cla_rate_adv_pct025 = quantile(cla_rate_adv, 0.025, na.rm = TRUE),
            cla_rate_adv_pct975 = quantile(cla_rate_adv, 0.975, na.rm = TRUE),
            
            cla_diff_median = median(cla_diff, na.rm = TRUE),
            cla_diff_pct025 = quantile(cla_diff, 0.025, na.rm = TRUE),
            cla_diff_pct975 = quantile(cla_diff, 0.975, na.rm = TRUE),
            
            cla_rr_median = median(cla_rr, na.rm = TRUE),
            cla_rr_pct025 = quantile(cla_rr, 0.025, na.rm = TRUE),
            cla_rr_pct975 = quantile(cla_rr, 0.975, na.rm = TRUE),
            
            cla_rd_median = median(cla_rd, na.rm = TRUE),
            cla_rd_pct025 = quantile(cla_rd, 0.025, na.rm = TRUE),
            cla_rd_pct975 = quantile(cla_rd, 0.975, na.rm = TRUE),
            
            nutri_change_median = median(nutri_change, na.rm = TRUE),
            nutri_change_pct025 = quantile(nutri_change, 0.025, na.rm = TRUE),
            nutri_change_pct975 = quantile(nutri_change, 0.975, na.rm = TRUE),
            
            nutri_total_median = median(nutri_total, na.rm = TRUE),
            nutri_total_pct025 = quantile(nutri_total, 0.025, na.rm = TRUE),
            nutri_total_pct975 = quantile(nutri_total, 0.975, na.rm = TRUE),
            
            nutri_rate_median = median(nutri_rate, na.rm = TRUE),
            nutri_rate_pct025 = quantile(nutri_rate, 0.025, na.rm = TRUE),
            nutri_rate_pct975 = quantile(nutri_rate, 0.975, na.rm = TRUE),
            
            nutri_diff_median = median(nutri_diff, na.rm = TRUE),
            nutri_diff_pct025 = quantile(nutri_diff, 0.025, na.rm = TRUE),
            nutri_diff_pct975 = quantile(nutri_diff, 0.975, na.rm = TRUE),
            
            nutri_change_adv_median = median(nutri_change_adv, na.rm = TRUE),
            nutri_change_adv_pct025 = quantile(nutri_change_adv, 0.025, na.rm = TRUE),
            nutri_change_adv_pct975 = quantile(nutri_change_adv, 0.975, na.rm = TRUE),
            
            nutri_rate_adv_median = median(nutri_rate_adv, na.rm = TRUE),
            nutri_rate_adv_pct025 = quantile(nutri_rate_adv, 0.025, na.rm = TRUE),
            nutri_rate_adv_pct975 = quantile(nutri_rate_adv, 0.975, na.rm = TRUE),
            
            nutri_rr_median = median(nutri_rr, na.rm = TRUE),
            nutri_rr_pct025 = quantile(nutri_rr, 0.025, na.rm = TRUE),
            nutri_rr_pct975 = quantile(nutri_rr, 0.975, na.rm = TRUE),
            
            nutri_rd_median = median(nutri_rd, na.rm = TRUE),
            nutri_rd_pct025 = quantile(nutri_rd, 0.025, na.rm = TRUE),
            nutri_rd_pct975 = quantile(nutri_rd, 0.975, na.rm = TRUE),
            
            admis_change_median = median(admis_change, na.rm = TRUE),
            admis_change_pct025 = quantile(admis_change, 0.025, na.rm = TRUE),
            admis_change_pct975 = quantile(admis_change, 0.975, na.rm = TRUE),
            
            admis_total_median = median(admis_total, na.rm = TRUE),
            admis_total_pct025 = quantile(admis_total, 0.025, na.rm = TRUE),
            admis_total_pct975 = quantile(admis_total, 0.975, na.rm = TRUE),
            
            admis_rate_median = median(admis_rate, na.rm = TRUE),
            admis_rate_pct025 = quantile(admis_rate, 0.025, na.rm = TRUE),
            admis_rate_pct975 = quantile(admis_rate, 0.975, na.rm = TRUE),
            
            admis_change_adv_median = median(admis_change_adv, na.rm = TRUE),
            admis_change_adv_pct025 = quantile(admis_change_adv, 0.025, na.rm = TRUE),
            admis_change_adv_pct975 = quantile(admis_change_adv, 0.975, na.rm = TRUE),
            
            admis_rate_adv_median = median(admis_rate_adv, na.rm = TRUE),
            admis_rate_adv_pct025 = quantile(admis_rate_adv, 0.025, na.rm = TRUE),
            admis_rate_adv_pct975 = quantile(admis_rate_adv, 0.975, na.rm = TRUE),
            
            admis_diff_median = median(admis_diff, na.rm = TRUE),
            admis_diff_pct025 = quantile(admis_diff, 0.025, na.rm = TRUE),
            admis_diff_pct975 = quantile(admis_diff, 0.975, na.rm = TRUE),
            
            admis_rr_median = median(admis_rr, na.rm = TRUE),
            admis_rr_pct025 = quantile(admis_rr, 0.025, na.rm = TRUE),
            admis_rr_pct975 = quantile(admis_rr, 0.975, na.rm = TRUE),
            
            admis_rd_median = median(admis_rd, na.rm = TRUE),
            admis_rd_pct025 = quantile(admis_rd, 0.025, na.rm = TRUE),
            admis_rd_pct975 = quantile(admis_rd, 0.975, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(mort_change_ci = sprintf("%.0f (%.0f, %.0f)", mort_change_median, mort_change_pct025, mort_change_pct975),
         mort_total_ci = sprintf("%.0f (%.0f, %.0f)", mort_total_median, mort_total_pct025, mort_total_pct975),
         mort_rate_ci = sprintf("%.0f (%.0f, %.0f)", mort_rate_median, mort_rate_pct025, mort_rate_pct975),
         mort_change_adv_ci = sprintf("%.0f (%.0f, %.0f)", mort_change_adv_median, mort_change_adv_pct025, mort_change_adv_pct975),
         mort_rate_adv_ci = sprintf("%.3f (%.3f, %.3f)", mort_rate_adv_median, mort_rate_adv_pct025, mort_rate_adv_pct975),
         mort_diff_ci = sprintf("%.0f (%.0f, %.0f)", mort_diff_median, mort_diff_pct025, mort_diff_pct975),
         mort_rr_ci = sprintf("%.3f (%.3f, %.3f)", mort_rr_median, mort_rr_pct025, mort_rr_pct975),
         mort_rd_ci = sprintf("%.3f (%.3f, %.3f)", mort_rd_median, mort_rd_pct025, mort_rd_pct975),
         
         cla_change_ci = sprintf("%.0f (%.0f, %.0f)", cla_change_median, cla_change_pct025, cla_change_pct975),
         cla_total_ci = sprintf("%.0f (%.0f, %.0f)", cla_total_median, cla_total_pct025, cla_total_pct975),
         cla_rate_ci = sprintf("%.0f (%.0f, %.0f)", cla_rate_median, cla_rate_pct025, cla_rate_pct975),
         cla_change_adv_ci = sprintf("%.0f (%.0f, %.0f)", cla_change_adv_median, cla_change_adv_pct025, cla_change_adv_pct975),
         cla_rate_adv_ci = sprintf("%.3f (%.3f, %.3f)", cla_rate_adv_median, cla_rate_adv_pct025, cla_rate_adv_pct975),
         cla_diff_ci = sprintf("%.0f (%.0f, %.0f)", cla_diff_median, cla_diff_pct025, cla_diff_pct975),
         cla_rr_ci = sprintf("%.3f (%.3f, %.3f)", cla_rr_median, cla_rr_pct025, cla_rr_pct975),
         cla_rd_ci = sprintf("%.3f (%.3f, %.3f)", cla_rd_median, cla_rd_pct025, cla_rd_pct975),
         
         nutri_change_ci = sprintf("%.0f (%.0f, %.0f)", nutri_change_median, nutri_change_pct025, nutri_change_pct975),
         nutri_total_ci = sprintf("%.0f (%.0f, %.0f)", nutri_total_median, nutri_total_pct025, nutri_total_pct975),
         nutri_rate_ci = sprintf("%.0f (%.0f, %.0f)", nutri_rate_median, nutri_rate_pct025, nutri_rate_pct975),
         nutri_change_adv_ci = sprintf("%.0f (%.0f, %.0f)", nutri_change_adv_median, nutri_change_adv_pct025, nutri_change_adv_pct975),
         nutri_rate_adv_ci = sprintf("%.3f (%.3f, %.3f)", nutri_rate_adv_median, nutri_rate_adv_pct025, nutri_rate_adv_pct975),
         nutri_diff_ci = sprintf("%.0f (%.0f, %.0f)", nutri_diff_median, nutri_diff_pct025, nutri_diff_pct975),
         nutri_rr_ci = sprintf("%.3f (%.3f, %.3f)", nutri_rr_median, nutri_rr_pct025, nutri_rr_pct975),
         nutri_rd_ci = sprintf("%.3f (%.3f, %.3f)", nutri_rd_median, nutri_rd_pct025, nutri_rd_pct975),
         
         admis_change_ci = sprintf("%.0f (%.0f, %.0f)", admis_change_median, admis_change_pct025, admis_change_pct975),
         admis_total_ci = sprintf("%.0f (%.0f, %.0f)", admis_total_median, admis_total_pct025, admis_total_pct975),
         admis_rate_ci = sprintf("%.0f (%.0f, %.0f)", admis_rate_median, admis_rate_pct025, admis_rate_pct975),
         admis_change_adv_ci = sprintf("%.0f (%.0f, %.0f)", admis_change_adv_median, admis_change_adv_pct025, admis_change_adv_pct975),
         admis_rate_adv_ci = sprintf("%.3f (%.3f, %.3f)", admis_rate_adv_median, admis_rate_adv_pct025, admis_rate_adv_pct975),
         admis_diff_ci = sprintf("%.0f (%.0f, %.0f)", admis_diff_median, admis_diff_pct025, admis_diff_pct975),
         admis_rr_ci = sprintf("%.3f (%.3f, %.3f)", admis_rr_median, admis_rr_pct025, admis_rr_pct975),
         admis_rd_ci = sprintf("%.3f (%.3f, %.3f)", admis_rd_median, admis_rd_pct025, admis_rd_pct975))


view(area_run_result) #discuss results

area_run_result <- area_run_result %>%
  arrange(factor(scenario, levels = c("cp_low", "cp_med", "cp_high")))


#### National run ####

#Result summary w national level
national_run_result <- output_2 %>%
  group_by(country, scenario, run) %>%
  filter(year > 2023) %>% #filter for years above 2023 
  summarise(mort_change = sum(mort_change),
            mort_total = sum(mort_total),
            mort_rate = sum((mort_total)/sum(mort_pop))*100000,
            mort_change_adv = sum(mort_change_adv),
            mort_rate_adv = (sum(mort_total_adv)/sum(mort_pop))*100000,
            mort_diff = sum(mort_change)-sum(mort_change_adv),
            mort_rr = (sum(mort_total)/sum(mort_pop))/(sum(mort_total_adv)/sum(mort_pop)),
            mort_rd = ((sum(mort_total)/sum(mort_pop))*100000)-((sum(mort_total_adv)/sum(mort_pop))*100000),
            
            cla_change = sum(cla_change),
            cla_total = sum(cla_total),
            cla_rate = (sum(cla_total)/sum(cla_pop))*100000,
            cla_change_adv = sum(cla_change_adv),
            cla_rate_adv = (sum(cla_total_adv)/sum(cla_pop))*100000,
            cla_diff = sum(cla_change)-sum(cla_change_adv),
            cla_rr = (sum(cla_total)/sum(cla_pop))/(sum(cla_total_adv)/sum(cla_pop)),
            cla_rd = ((sum(cla_total)/sum(cla_pop))*100000)-((sum(cla_total_adv)/sum(cla_pop))*100000),
            
            nutri_change = sum(nutri_change),
            nutri_total = sum(nutri_total),
            nutri_rate = (sum(nutri_total)/sum(nutri_pop))*100000,
            nutri_change_adv = sum(nutri_change_adv),
            nutri_rate_adv = (sum(nutri_total_adv)/sum(nutri_pop))*100000,
            nutri_diff = sum(nutri_change)-sum(nutri_change_adv),
            nutri_rr = (sum(nutri_total)/sum(nutri_pop))/(sum(nutri_total_adv)/sum(nutri_pop)),
            nutri_rd = ((sum(nutri_total)/sum(nutri_pop))*100000)-((sum(nutri_total_adv)/sum(nutri_pop))*100000),
            
            admis_change = sum(admis_change),
            admis_total = sum(admis_total),
            admis_rate = (sum(admis_total)/sum(admis_pop))*100000,
            admis_change_adv = sum(admis_change_adv),
            admis_rate_adv = (sum(admis_total_adv)/sum(admis_pop))*100000,
            admis_diff = sum(admis_change)-sum(admis_change_adv),
            admis_rr = (sum(admis_total)/sum(admis_pop))/(sum(admis_total_adv)/sum(admis_pop)),
            admis_rd = ((sum(admis_total)/sum(admis_pop))*100000)-((sum(admis_total_adv)/sum(admis_pop))*100000),
            .groups = "drop") %>% 
  group_by(country, scenario) %>%
  summarise(mort_change_median = median(mort_change, na.rm = TRUE),
            mort_change_pct025 = quantile(mort_change, 0.025, na.rm = TRUE),
            mort_change_pct975 = quantile(mort_change, 0.975, na.rm = TRUE),
            
            mort_total_median = median(mort_total, na.rm = TRUE),
            mort_total_pct025 = quantile(mort_total, 0.025, na.rm = TRUE),
            mort_total_pct975 = quantile(mort_total, 0.975, na.rm = TRUE),
            
            mort_rate_median = median(mort_rate, na.rm = TRUE),
            mort_rate_pct025 = quantile(mort_rate, 0.025, na.rm = TRUE),
            mort_rate_pct975 = quantile(mort_rate, 0.975, na.rm = TRUE),
            
            mort_change_adv_median = median(mort_change_adv, na.rm = TRUE),
            mort_change_adv_pct025 = quantile(mort_change_adv, 0.025, na.rm = TRUE),
            mort_change_adv_pct975 = quantile(mort_change_adv, 0.975, na.rm = TRUE),
            
            mort_rate_adv_median = median(mort_rate_adv, na.rm = TRUE),
            mort_rate_adv_pct025 = quantile(mort_rate_adv, 0.025, na.rm = TRUE),
            mort_rate_adv_pct975 = quantile(mort_rate_adv, 0.975, na.rm = TRUE),
            
            mort_diff_median = median(mort_diff, na.rm = TRUE),
            mort_diff_pct025 = quantile(mort_diff, 0.025, na.rm = TRUE),
            mort_diff_pct975 = quantile(mort_diff, 0.975, na.rm = TRUE),
            
            mort_rr_median = median(mort_rr, na.rm = TRUE),
            mort_rr_pct025 = quantile(mort_rr, 0.025, na.rm = TRUE),
            mort_rr_pct975 = quantile(mort_rr, 0.975, na.rm = TRUE),
            
            mort_rd_median = median(mort_rd, na.rm = TRUE),
            mort_rd_pct025 = quantile(mort_rd, 0.025, na.rm = TRUE),
            mort_rd_pct975 = quantile(mort_rd, 0.975, na.rm = TRUE),
            
            cla_change_median = median(cla_change, na.rm = TRUE),
            cla_change_pct025 = quantile(cla_change, 0.025, na.rm = TRUE),
            cla_change_pct975 = quantile(cla_change, 0.975, na.rm = TRUE),
            
            cla_total_median = median(cla_total, na.rm = TRUE),
            cla_total_pct025 = quantile(cla_total, 0.025, na.rm = TRUE),
            cla_total_pct975 = quantile(cla_total, 0.975, na.rm = TRUE),
            
            cla_rate_median = median(cla_rate, na.rm = TRUE),
            cla_rate_pct025 = quantile(cla_rate, 0.025, na.rm = TRUE),
            cla_rate_pct975 = quantile(cla_rate, 0.975, na.rm = TRUE),
            
            cla_change_adv_median = median(cla_change_adv, na.rm = TRUE),
            cla_change_adv_pct025 = quantile(cla_change_adv, 0.025, na.rm = TRUE),
            cla_change_adv_pct975 = quantile(cla_change_adv, 0.975, na.rm = TRUE),
            
            cla_rate_adv_median = median(cla_rate_adv, na.rm = TRUE),
            cla_rate_adv_pct025 = quantile(cla_rate_adv, 0.025, na.rm = TRUE),
            cla_rate_adv_pct975 = quantile(cla_rate_adv, 0.975, na.rm = TRUE),
            
            cla_diff_median = median(cla_diff, na.rm = TRUE),
            cla_diff_pct025 = quantile(cla_diff, 0.025, na.rm = TRUE),
            cla_diff_pct975 = quantile(cla_diff, 0.975, na.rm = TRUE),
            
            cla_rr_median = median(cla_rr, na.rm = TRUE),
            cla_rr_pct025 = quantile(cla_rr, 0.025, na.rm = TRUE),
            cla_rr_pct975 = quantile(cla_rr, 0.975, na.rm = TRUE),
            
            cla_rd_median = median(cla_rd, na.rm = TRUE),
            cla_rd_pct025 = quantile(cla_rd, 0.025, na.rm = TRUE),
            cla_rd_pct975 = quantile(cla_rd, 0.975, na.rm = TRUE),
            
            nutri_change_median = median(nutri_change, na.rm = TRUE),
            nutri_change_pct025 = quantile(nutri_change, 0.025, na.rm = TRUE),
            nutri_change_pct975 = quantile(nutri_change, 0.975, na.rm = TRUE),
            
            nutri_total_median = median(nutri_total, na.rm = TRUE),
            nutri_total_pct025 = quantile(nutri_total, 0.025, na.rm = TRUE),
            nutri_total_pct975 = quantile(nutri_total, 0.975, na.rm = TRUE),
            
            nutri_rate_median = median(nutri_rate, na.rm = TRUE),
            nutri_rate_pct025 = quantile(nutri_rate, 0.025, na.rm = TRUE),
            nutri_rate_pct975 = quantile(nutri_rate, 0.975, na.rm = TRUE),
            
            nutri_diff_median = median(nutri_diff, na.rm = TRUE),
            nutri_diff_pct025 = quantile(nutri_diff, 0.025, na.rm = TRUE),
            nutri_diff_pct975 = quantile(nutri_diff, 0.975, na.rm = TRUE),
            
            nutri_change_adv_median = median(nutri_change_adv, na.rm = TRUE),
            nutri_change_adv_pct025 = quantile(nutri_change_adv, 0.025, na.rm = TRUE),
            nutri_change_adv_pct975 = quantile(nutri_change_adv, 0.975, na.rm = TRUE),
            
            nutri_rate_adv_median = median(nutri_rate_adv, na.rm = TRUE),
            nutri_rate_adv_pct025 = quantile(nutri_rate_adv, 0.025, na.rm = TRUE),
            nutri_rate_adv_pct975 = quantile(nutri_rate_adv, 0.975, na.rm = TRUE),
            
            nutri_rr_median = median(nutri_rr, na.rm = TRUE),
            nutri_rr_pct025 = quantile(nutri_rr, 0.025, na.rm = TRUE),
            nutri_rr_pct975 = quantile(nutri_rr, 0.975, na.rm = TRUE),
            
            nutri_rd_median = median(nutri_rd, na.rm = TRUE),
            nutri_rd_pct025 = quantile(nutri_rd, 0.025, na.rm = TRUE),
            nutri_rd_pct975 = quantile(nutri_rd, 0.975, na.rm = TRUE),
            
            admis_change_median = median(admis_change, na.rm = TRUE),
            admis_change_pct025 = quantile(admis_change, 0.025, na.rm = TRUE),
            admis_change_pct975 = quantile(admis_change, 0.975, na.rm = TRUE),
            
            admis_total_median = median(admis_total, na.rm = TRUE),
            admis_total_pct025 = quantile(admis_total, 0.025, na.rm = TRUE),
            admis_total_pct975 = quantile(admis_total, 0.975, na.rm = TRUE),
            
            admis_rate_median = median(admis_rate, na.rm = TRUE),
            admis_rate_pct025 = quantile(admis_rate, 0.025, na.rm = TRUE),
            admis_rate_pct975 = quantile(admis_rate, 0.975, na.rm = TRUE),
            
            admis_change_adv_median = median(admis_change_adv, na.rm = TRUE),
            admis_change_adv_pct025 = quantile(admis_change_adv, 0.025, na.rm = TRUE),
            admis_change_adv_pct975 = quantile(admis_change_adv, 0.975, na.rm = TRUE),
            
            admis_rate_adv_median = median(admis_rate_adv, na.rm = TRUE),
            admis_rate_adv_pct025 = quantile(admis_rate_adv, 0.025, na.rm = TRUE),
            admis_rate_adv_pct975 = quantile(admis_rate_adv, 0.975, na.rm = TRUE),
            
            admis_diff_median = median(admis_diff, na.rm = TRUE),
            admis_diff_pct025 = quantile(admis_diff, 0.025, na.rm = TRUE),
            admis_diff_pct975 = quantile(admis_diff, 0.975, na.rm = TRUE),
            
            admis_rr_median = median(admis_rr, na.rm = TRUE),
            admis_rr_pct025 = quantile(admis_rr, 0.025, na.rm = TRUE),
            admis_rr_pct975 = quantile(admis_rr, 0.975, na.rm = TRUE),
            
            admis_rd_median = median(admis_rd, na.rm = TRUE),
            admis_rd_pct025 = quantile(admis_rd, 0.025, na.rm = TRUE),
            admis_rd_pct975 = quantile(admis_rd, 0.975, na.rm = TRUE),
    
    .groups = "drop",
  ) %>%
  mutate(mort_change_ci = sprintf("%.0f (%.0f, %.0f)", mort_change_median, mort_change_pct025, mort_change_pct975),
         mort_total_ci = sprintf("%.0f (%.0f, %.0f)", mort_total_median, mort_total_pct025, mort_total_pct975),
         mort_rate_ci = sprintf("%.0f (%.0f, %.0f)", mort_rate_median, mort_rate_pct025, mort_rate_pct975),
         mort_change_adv_ci = sprintf("%.0f (%.0f, %.0f)", mort_change_adv_median, mort_change_adv_pct025, mort_change_adv_pct975),
         mort_rate_adv_ci = sprintf("%.3f (%.3f, %.3f)", mort_rate_adv_median, mort_rate_adv_pct025, mort_rate_adv_pct975),
         mort_diff_ci = sprintf("%.0f (%.0f, %.0f)", mort_diff_median, mort_diff_pct025, mort_diff_pct975),
         mort_rr_ci = sprintf("%.3f (%.3f, %.3f)", mort_rr_median, mort_rr_pct025, mort_rr_pct975),
         mort_rd_ci = sprintf("%.3f (%.3f, %.3f)", mort_rd_median, mort_rd_pct025, mort_rd_pct975),
         
         cla_change_ci = sprintf("%.0f (%.0f, %.0f)", cla_change_median, cla_change_pct025, cla_change_pct975),
         cla_total_ci = sprintf("%.0f (%.0f, %.0f)", cla_total_median, cla_total_pct025, cla_total_pct975),
         cla_rate_ci = sprintf("%.0f (%.0f, %.0f)", cla_rate_median, cla_rate_pct025, cla_rate_pct975),
         cla_change_adv_ci = sprintf("%.0f (%.0f, %.0f)", cla_change_adv_median, cla_change_adv_pct025, cla_change_adv_pct975),
         cla_rate_adv_ci = sprintf("%.3f (%.3f, %.3f)", cla_rate_adv_median, cla_rate_adv_pct025, cla_rate_adv_pct975),
         cla_diff_ci = sprintf("%.0f (%.0f, %.0f)", cla_diff_median, cla_diff_pct025, cla_diff_pct975),
         cla_rr_ci = sprintf("%.3f (%.3f, %.3f)", cla_rr_median, cla_rr_pct025, cla_rr_pct975),
         cla_rd_ci = sprintf("%.3f (%.3f, %.3f)", cla_rd_median, cla_rd_pct025, cla_rd_pct975),
         
         nutri_change_ci = sprintf("%.0f (%.0f, %.0f)", nutri_change_median, nutri_change_pct025, nutri_change_pct975),
         nutri_total_ci = sprintf("%.0f (%.0f, %.0f)", nutri_total_median, nutri_total_pct025, nutri_total_pct975),
         nutri_rate_ci = sprintf("%.0f (%.0f, %.0f)", nutri_rate_median, nutri_rate_pct025, nutri_rate_pct975),
         nutri_change_adv_ci = sprintf("%.0f (%.0f, %.0f)", nutri_change_adv_median, nutri_change_adv_pct025, nutri_change_adv_pct975),
         nutri_rate_adv_ci = sprintf("%.3f (%.3f, %.3f)", nutri_rate_adv_median, nutri_rate_adv_pct025, nutri_rate_adv_pct975),
         nutri_diff_ci = sprintf("%.0f (%.0f, %.0f)", nutri_diff_median, nutri_diff_pct025, nutri_diff_pct975),
         nutri_rr_ci = sprintf("%.3f (%.3f, %.3f)", nutri_rr_median, nutri_rr_pct025, nutri_rr_pct975),
         nutri_rd_ci = sprintf("%.3f (%.3f, %.3f)", nutri_rd_median, nutri_rd_pct025, nutri_rd_pct975),
         
         admis_change_ci = sprintf("%.0f (%.0f, %.0f)", admis_change_median, admis_change_pct025, admis_change_pct975),
         admis_total_ci = sprintf("%.0f (%.0f, %.0f)", admis_total_median, admis_total_pct025, admis_total_pct975),
         admis_rate_ci = sprintf("%.0f (%.0f, %.0f)", admis_rate_median, admis_rate_pct025, admis_rate_pct975),
         admis_change_adv_ci = sprintf("%.0f (%.0f, %.0f)", admis_change_adv_median, admis_change_adv_pct025, admis_change_adv_pct975),
         admis_rate_adv_ci = sprintf("%.3f (%.3f, %.3f)", admis_rate_adv_median, admis_rate_adv_pct025, admis_rate_adv_pct975),
         admis_diff_ci = sprintf("%.0f (%.0f, %.0f)", admis_diff_median, admis_diff_pct025, admis_diff_pct975),
         admis_rr_ci = sprintf("%.3f (%.3f, %.3f)", admis_rr_median, admis_rr_pct025, admis_rr_pct975),
         admis_rd_ci = sprintf("%.3f (%.3f, %.3f)", admis_rd_median, admis_rd_pct025, admis_rd_pct975))

view(national_run_result) #discuss results

national_run_result <- national_run_result %>%
  arrange(factor(scenario, levels = c("cp_low", "cp_med", "cp_high")))

#### IMD run by year ####

IMD_trend <- output_2 %>%
  group_by(area_code, area_name, scenario, IMD_tertile, year, run) %>%
  filter(year > 2023) %>%
  summarise(mort_change = sum(mort_change),
            mort_total = sum(mort_total),
            mort_change_adv = sum(mort_change_adv),
            mort_total_adv = sum(mort_total_adv),
            mort_pop = sum(mort_pop), 
            
            cla_change =  sum(cla_change),
            cla_total = sum(cla_total),
            cla_change_adv = sum(cla_change_adv),
            cla_total_adv = sum(cla_total_adv),
            cla_pop= sum(cla_pop),
            
            nutri_change = sum(nutri_change),  
            nutri_total = sum(nutri_total),
            nutri_change_adv = sum(nutri_change_adv),
            nutri_total_adv = sum(nutri_total_adv),
            nutri_pop= sum(nutri_pop),
            
            admis_change = sum(admis_change),
            admis_total = sum(admis_total),
            admis_change_adv = sum(admis_change_adv),
            admis_total_adv = sum(admis_total_adv),
            admis_pop= sum(admis_pop),
            .groups = "drop")  %>% 
  group_by(IMD_tertile, scenario, year, run) %>% 
  summarize(mort_change = sum(mort_change),
            mort_total = sum(mort_total),
            mort_rate = sum((mort_total)/sum(mort_pop))*100000,
            mort_change_adv = sum(mort_change_adv),
            mort_rate_adv = (sum(mort_total_adv)/sum(mort_pop))*100000,
            mort_diff = sum(mort_change)-sum(mort_change_adv),
            mort_rr = (sum(mort_total)/sum(mort_pop))/(sum(mort_total_adv)/sum(mort_pop)),
            mort_rd = ((sum(mort_total)/sum(mort_pop))*100000)-((sum(mort_total_adv)/sum(mort_pop))*100000),
            
            cla_change = sum(cla_change),
            cla_total = sum(cla_total),
            cla_rate = (sum(cla_total)/sum(cla_pop))*100000,
            cla_change_adv = sum(cla_change_adv),
            cla_rate_adv = (sum(cla_total_adv)/sum(cla_pop))*100000,
            cla_diff = sum(cla_change)-sum(cla_change_adv),
            cla_rr = (sum(cla_total)/sum(cla_pop))/(sum(cla_total_adv)/sum(cla_pop)),
            cla_rd = ((sum(cla_total)/sum(cla_pop))*100000)-((sum(cla_total_adv)/sum(cla_pop))*100000),
            
            nutri_change = sum(nutri_change),
            nutri_total = sum(nutri_total),
            nutri_rate = (sum(nutri_total)/sum(nutri_pop))*100000,
            nutri_change_adv = sum(nutri_change_adv),
            nutri_rate_adv = (sum(nutri_total_adv)/sum(nutri_pop))*100000,
            nutri_diff = sum(nutri_change)-sum(nutri_change_adv),
            nutri_rr = (sum(nutri_total)/sum(nutri_pop))/(sum(nutri_total_adv)/sum(nutri_pop)),
            nutri_rd = ((sum(nutri_total)/sum(nutri_pop))*100000)-((sum(nutri_total_adv)/sum(nutri_pop))*100000),
            
            admis_change = sum(admis_change),
            admis_total = sum(admis_total),
            admis_rate = (sum(admis_total)/sum(admis_pop))*100000,
            admis_change_adv = sum(admis_change_adv),
            admis_rate_adv = (sum(admis_total_adv)/sum(admis_pop))*100000,
            admis_diff = sum(admis_change)-sum(admis_change_adv),
            admis_rr = (sum(admis_total)/sum(admis_pop))/(sum(admis_total_adv)/sum(admis_pop)),
            admis_rd = ((sum(admis_total)/sum(admis_pop))*100000)-((sum(admis_total_adv)/sum(admis_pop))*100000),  
            .groups = "drop") %>% 
  # Generate the summary data (of 1000 runs)
  group_by(IMD_tertile, scenario, year) %>%
  summarise(mort_change_median = median(mort_change, na.rm = TRUE),
            mort_change_pct025 = quantile(mort_change, 0.025, na.rm = TRUE),
            mort_change_pct975 = quantile(mort_change, 0.975, na.rm = TRUE),
            
            mort_total_median = median(mort_total, na.rm = TRUE),
            mort_total_pct025 = quantile(mort_total, 0.025, na.rm = TRUE),
            mort_total_pct975 = quantile(mort_total, 0.975, na.rm = TRUE),
            
            mort_rate_median = median(mort_rate, na.rm = TRUE),
            mort_rate_pct025 = quantile(mort_rate, 0.025, na.rm = TRUE),
            mort_rate_pct975 = quantile(mort_rate, 0.975, na.rm = TRUE),
            
            mort_change_adv_median = median(mort_change_adv, na.rm = TRUE),
            mort_change_adv_pct025 = quantile(mort_change_adv, 0.025, na.rm = TRUE),
            mort_change_adv_pct975 = quantile(mort_change_adv, 0.975, na.rm = TRUE),
            
            mort_rate_adv_median = median(mort_rate_adv, na.rm = TRUE),
            mort_rate_adv_pct025 = quantile(mort_rate_adv, 0.025, na.rm = TRUE),
            mort_rate_adv_pct975 = quantile(mort_rate_adv, 0.975, na.rm = TRUE),
            
            mort_diff_median = median(mort_diff, na.rm = TRUE),
            mort_diff_pct025 = quantile(mort_diff, 0.025, na.rm = TRUE),
            mort_diff_pct975 = quantile(mort_diff, 0.975, na.rm = TRUE),
            
            mort_rr_median = median(mort_rr, na.rm = TRUE),
            mort_rr_pct025 = quantile(mort_rr, 0.025, na.rm = TRUE),
            mort_rr_pct975 = quantile(mort_rr, 0.975, na.rm = TRUE),
            
            mort_rd_median = median(mort_rd, na.rm = TRUE),
            mort_rd_pct025 = quantile(mort_rd, 0.025, na.rm = TRUE),
            mort_rd_pct975 = quantile(mort_rd, 0.975, na.rm = TRUE),
            
            cla_change_median = median(cla_change, na.rm = TRUE),
            cla_change_pct025 = quantile(cla_change, 0.025, na.rm = TRUE),
            cla_change_pct975 = quantile(cla_change, 0.975, na.rm = TRUE),
            
            cla_total_median = median(cla_total, na.rm = TRUE),
            cla_total_pct025 = quantile(cla_total, 0.025, na.rm = TRUE),
            cla_total_pct975 = quantile(cla_total, 0.975, na.rm = TRUE),
            
            cla_rate_median = median(cla_rate, na.rm = TRUE),
            cla_rate_pct025 = quantile(cla_rate, 0.025, na.rm = TRUE),
            cla_rate_pct975 = quantile(cla_rate, 0.975, na.rm = TRUE),
            
            cla_change_adv_median = median(cla_change_adv, na.rm = TRUE),
            cla_change_adv_pct025 = quantile(cla_change_adv, 0.025, na.rm = TRUE),
            cla_change_adv_pct975 = quantile(cla_change_adv, 0.975, na.rm = TRUE),
            
            cla_rate_adv_median = median(cla_rate_adv, na.rm = TRUE),
            cla_rate_adv_pct025 = quantile(cla_rate_adv, 0.025, na.rm = TRUE),
            cla_rate_adv_pct975 = quantile(cla_rate_adv, 0.975, na.rm = TRUE),
            
            cla_diff_median = median(cla_diff, na.rm = TRUE),
            cla_diff_pct025 = quantile(cla_diff, 0.025, na.rm = TRUE),
            cla_diff_pct975 = quantile(cla_diff, 0.975, na.rm = TRUE),
            
            cla_rr_median = median(cla_rr, na.rm = TRUE),
            cla_rr_pct025 = quantile(cla_rr, 0.025, na.rm = TRUE),
            cla_rr_pct975 = quantile(cla_rr, 0.975, na.rm = TRUE),
            
            cla_rd_median = median(cla_rd, na.rm = TRUE),
            cla_rd_pct025 = quantile(cla_rd, 0.025, na.rm = TRUE),
            cla_rd_pct975 = quantile(cla_rd, 0.975, na.rm = TRUE),
            
            nutri_change_median = median(nutri_change, na.rm = TRUE),
            nutri_change_pct025 = quantile(nutri_change, 0.025, na.rm = TRUE),
            nutri_change_pct975 = quantile(nutri_change, 0.975, na.rm = TRUE),
            
            nutri_total_median = median(nutri_total, na.rm = TRUE),
            nutri_total_pct025 = quantile(nutri_total, 0.025, na.rm = TRUE),
            nutri_total_pct975 = quantile(nutri_total, 0.975, na.rm = TRUE),
            
            nutri_rate_median = median(nutri_rate, na.rm = TRUE),
            nutri_rate_pct025 = quantile(nutri_rate, 0.025, na.rm = TRUE),
            nutri_rate_pct975 = quantile(nutri_rate, 0.975, na.rm = TRUE),
            
            nutri_diff_median = median(nutri_diff, na.rm = TRUE),
            nutri_diff_pct025 = quantile(nutri_diff, 0.025, na.rm = TRUE),
            nutri_diff_pct975 = quantile(nutri_diff, 0.975, na.rm = TRUE),
            
            nutri_change_adv_median = median(nutri_change_adv, na.rm = TRUE),
            nutri_change_adv_pct025 = quantile(nutri_change_adv, 0.025, na.rm = TRUE),
            nutri_change_adv_pct975 = quantile(nutri_change_adv, 0.975, na.rm = TRUE),
            
            nutri_rate_adv_median = median(nutri_rate_adv, na.rm = TRUE),
            nutri_rate_adv_pct025 = quantile(nutri_rate_adv, 0.025, na.rm = TRUE),
            nutri_rate_adv_pct975 = quantile(nutri_rate_adv, 0.975, na.rm = TRUE),
            
            nutri_rr_median = median(nutri_rr, na.rm = TRUE),
            nutri_rr_pct025 = quantile(nutri_rr, 0.025, na.rm = TRUE),
            nutri_rr_pct975 = quantile(nutri_rr, 0.975, na.rm = TRUE),
            
            nutri_rd_median = median(nutri_rd, na.rm = TRUE),
            nutri_rd_pct025 = quantile(nutri_rd, 0.025, na.rm = TRUE),
            nutri_rd_pct975 = quantile(nutri_rd, 0.975, na.rm = TRUE),
            
            admis_change_median = median(admis_change, na.rm = TRUE),
            admis_change_pct025 = quantile(admis_change, 0.025, na.rm = TRUE),
            admis_change_pct975 = quantile(admis_change, 0.975, na.rm = TRUE),
            
            admis_total_median = median(admis_total, na.rm = TRUE),
            admis_total_pct025 = quantile(admis_total, 0.025, na.rm = TRUE),
            admis_total_pct975 = quantile(admis_total, 0.975, na.rm = TRUE),
            
            admis_rate_median = median(admis_rate, na.rm = TRUE),
            admis_rate_pct025 = quantile(admis_rate, 0.025, na.rm = TRUE),
            admis_rate_pct975 = quantile(admis_rate, 0.975, na.rm = TRUE),
            
            admis_change_adv_median = median(admis_change_adv, na.rm = TRUE),
            admis_change_adv_pct025 = quantile(admis_change_adv, 0.025, na.rm = TRUE),
            admis_change_adv_pct975 = quantile(admis_change_adv, 0.975, na.rm = TRUE),
            
            admis_rate_adv_median = median(admis_rate_adv, na.rm = TRUE),
            admis_rate_adv_pct025 = quantile(admis_rate_adv, 0.025, na.rm = TRUE),
            admis_rate_adv_pct975 = quantile(admis_rate_adv, 0.975, na.rm = TRUE),
            
            admis_diff_median = median(admis_diff, na.rm = TRUE),
            admis_diff_pct025 = quantile(admis_diff, 0.025, na.rm = TRUE),
            admis_diff_pct975 = quantile(admis_diff, 0.975, na.rm = TRUE),
            
            admis_rr_median = median(admis_rr, na.rm = TRUE),
            admis_rr_pct025 = quantile(admis_rr, 0.025, na.rm = TRUE),
            admis_rr_pct975 = quantile(admis_rr, 0.975, na.rm = TRUE),
            
            admis_rd_median = median(admis_rd, na.rm = TRUE),
            admis_rd_pct025 = quantile(admis_rd, 0.025, na.rm = TRUE),
            admis_rd_pct975 = quantile(admis_rd, 0.975, na.rm = TRUE),
    
    
    .groups = "drop"
  ) %>%
  mutate(mort_change_ci = sprintf("%.0f (%.0f, %.0f)", mort_change_median, mort_change_pct025, mort_change_pct975),
         mort_total_ci = sprintf("%.0f (%.0f, %.0f)", mort_total_median, mort_total_pct025, mort_total_pct975),
         mort_rate_ci = sprintf("%.0f (%.0f, %.0f)", mort_rate_median, mort_rate_pct025, mort_rate_pct975),
         mort_change_adv_ci = sprintf("%.0f (%.0f, %.0f)", mort_change_adv_median, mort_change_adv_pct025, mort_change_adv_pct975),
         mort_rate_adv_ci = sprintf("%.3f (%.3f, %.3f)", mort_rate_adv_median, mort_rate_adv_pct025, mort_rate_adv_pct975),
         mort_diff_ci = sprintf("%.0f (%.0f, %.0f)", mort_diff_median, mort_diff_pct025, mort_diff_pct975),
         mort_rr_ci = sprintf("%.3f (%.3f, %.3f)", mort_rr_median, mort_rr_pct025, mort_rr_pct975),
         mort_rd_ci = sprintf("%.3f (%.3f, %.3f)", mort_rd_median, mort_rd_pct025, mort_rd_pct975),
         
         cla_change_ci = sprintf("%.0f (%.0f, %.0f)", cla_change_median, cla_change_pct025, cla_change_pct975),
         cla_total_ci = sprintf("%.0f (%.0f, %.0f)", cla_total_median, cla_total_pct025, cla_total_pct975),
         cla_rate_ci = sprintf("%.0f (%.0f, %.0f)", cla_rate_median, cla_rate_pct025, cla_rate_pct975),
         cla_change_adv_ci = sprintf("%.0f (%.0f, %.0f)", cla_change_adv_median, cla_change_adv_pct025, cla_change_adv_pct975),
         cla_rate_adv_ci = sprintf("%.3f (%.3f, %.3f)", cla_rate_adv_median, cla_rate_adv_pct025, cla_rate_adv_pct975),
         cla_diff_ci = sprintf("%.0f (%.0f, %.0f)", cla_diff_median, cla_diff_pct025, cla_diff_pct975),
         cla_rr_ci = sprintf("%.3f (%.3f, %.3f)", cla_rr_median, cla_rr_pct025, cla_rr_pct975),
         cla_rd_ci = sprintf("%.3f (%.3f, %.3f)", cla_rd_median, cla_rd_pct025, cla_rd_pct975),
         
         nutri_change_ci = sprintf("%.0f (%.0f, %.0f)", nutri_change_median, nutri_change_pct025, nutri_change_pct975),
         nutri_total_ci = sprintf("%.0f (%.0f, %.0f)", nutri_total_median, nutri_total_pct025, nutri_total_pct975),
         nutri_rate_ci = sprintf("%.0f (%.0f, %.0f)", nutri_rate_median, nutri_rate_pct025, nutri_rate_pct975),
         nutri_change_adv_ci = sprintf("%.0f (%.0f, %.0f)", nutri_change_adv_median, nutri_change_adv_pct025, nutri_change_adv_pct975),
         nutri_rate_adv_ci = sprintf("%.3f (%.3f, %.3f)", nutri_rate_adv_median, nutri_rate_adv_pct025, nutri_rate_adv_pct975),
         nutri_diff_ci = sprintf("%.0f (%.0f, %.0f)", nutri_diff_median, nutri_diff_pct025, nutri_diff_pct975),
         nutri_rr_ci = sprintf("%.3f (%.3f, %.3f)", nutri_rr_median, nutri_rr_pct025, nutri_rr_pct975),
         nutri_rd_ci = sprintf("%.3f (%.3f, %.3f)", nutri_rd_median, nutri_rd_pct025, nutri_rd_pct975),
         
         admis_change_ci = sprintf("%.0f (%.0f, %.0f)", admis_change_median, admis_change_pct025, admis_change_pct975),
         admis_total_ci = sprintf("%.0f (%.0f, %.0f)", admis_total_median, admis_total_pct025, admis_total_pct975),
         admis_rate_ci = sprintf("%.0f (%.0f, %.0f)", admis_rate_median, admis_rate_pct025, admis_rate_pct975),
         admis_change_adv_ci = sprintf("%.0f (%.0f, %.0f)", admis_change_adv_median, admis_change_adv_pct025, admis_change_adv_pct975),
         admis_rate_adv_ci = sprintf("%.3f (%.3f, %.3f)", admis_rate_adv_median, admis_rate_adv_pct025, admis_rate_adv_pct975),
         admis_diff_ci = sprintf("%.0f (%.0f, %.0f)", admis_diff_median, admis_diff_pct025, admis_diff_pct975),
         admis_rr_ci = sprintf("%.3f (%.3f, %.3f)", admis_rr_median, admis_rr_pct025, admis_rr_pct975),
         admis_rd_ci = sprintf("%.3f (%.3f, %.3f)", admis_rd_median, admis_rd_pct025, admis_rd_pct975)) 

#### National run by year ####

national_trend <- output_2 %>%
  group_by(country, scenario, year, run) %>%
  filter(year > 2023) %>% 
  summarize(mort_change = sum(mort_change),
            mort_total = sum(mort_total),
            mort_rate = sum((mort_total)/sum(mort_pop))*100000,
            mort_change_adv = sum(mort_change_adv),
            mort_rate_adv = (sum(mort_total_adv)/sum(mort_pop))*100000,
            mort_diff = sum(mort_change)-sum(mort_change_adv),
            mort_rr = (sum(mort_total)/sum(mort_pop))/(sum(mort_total_adv)/sum(mort_pop)),
            mort_rd = ((sum(mort_total)/sum(mort_pop))*100000)-((sum(mort_total_adv)/sum(mort_pop))*100000),
            
            cla_change = sum(cla_change),
            cla_total = sum(cla_total),
            cla_rate = (sum(cla_total)/sum(cla_pop))*100000,
            cla_change_adv = sum(cla_change_adv),
            cla_rate_adv = (sum(cla_total_adv)/sum(cla_pop))*100000,
            cla_diff = sum(cla_change)-sum(cla_change_adv),
            cla_rr = (sum(cla_total)/sum(cla_pop))/(sum(cla_total_adv)/sum(cla_pop)),
            cla_rd = ((sum(cla_total)/sum(cla_pop))*100000)-((sum(cla_total_adv)/sum(cla_pop))*100000),
            
            nutri_change = sum(nutri_change),
            nutri_total = sum(nutri_total),
            nutri_rate = (sum(nutri_total)/sum(nutri_pop))*100000,
            nutri_change_adv = sum(nutri_change_adv),
            nutri_rate_adv = (sum(nutri_total_adv)/sum(nutri_pop))*100000,
            nutri_diff = sum(nutri_change)-sum(nutri_change_adv),
            nutri_rr = (sum(nutri_total)/sum(nutri_pop))/(sum(nutri_total_adv)/sum(nutri_pop)),
            nutri_rd = ((sum(nutri_total)/sum(nutri_pop))*100000)-((sum(nutri_total_adv)/sum(nutri_pop))*100000),
            
            admis_change = sum(admis_change),
            admis_total = sum(admis_total),
            admis_rate = (sum(admis_total)/sum(admis_pop))*100000,
            admis_change_adv = sum(admis_change_adv),
            admis_rate_adv = (sum(admis_total_adv)/sum(admis_pop))*100000,
            admis_diff = sum(admis_change)-sum(admis_change_adv),
            admis_rr = (sum(admis_total)/sum(admis_pop))/(sum(admis_total_adv)/sum(admis_pop)),
            admis_rd = ((sum(admis_total)/sum(admis_pop))*100000)-((sum(admis_total_adv)/sum(admis_pop))*100000),  
            .groups = "drop") %>% 
  # Generate the summary data (of 1000 runs)
  group_by(country, scenario, year) %>%
  summarise(mort_change_median = median(mort_change, na.rm = TRUE),
            mort_change_pct025 = quantile(mort_change, 0.025, na.rm = TRUE),
            mort_change_pct975 = quantile(mort_change, 0.975, na.rm = TRUE),
            
            mort_total_median = median(mort_total, na.rm = TRUE),
            mort_total_pct025 = quantile(mort_total, 0.025, na.rm = TRUE),
            mort_total_pct975 = quantile(mort_total, 0.975, na.rm = TRUE),
            
            mort_rate_median = median(mort_rate, na.rm = TRUE),
            mort_rate_pct025 = quantile(mort_rate, 0.025, na.rm = TRUE),
            mort_rate_pct975 = quantile(mort_rate, 0.975, na.rm = TRUE),
            
            mort_change_adv_median = median(mort_change_adv, na.rm = TRUE),
            mort_change_adv_pct025 = quantile(mort_change_adv, 0.025, na.rm = TRUE),
            mort_change_adv_pct975 = quantile(mort_change_adv, 0.975, na.rm = TRUE),
            
            mort_rate_adv_median = median(mort_rate_adv, na.rm = TRUE),
            mort_rate_adv_pct025 = quantile(mort_rate_adv, 0.025, na.rm = TRUE),
            mort_rate_adv_pct975 = quantile(mort_rate_adv, 0.975, na.rm = TRUE),
            
            mort_diff_median = median(mort_diff, na.rm = TRUE),
            mort_diff_pct025 = quantile(mort_diff, 0.025, na.rm = TRUE),
            mort_diff_pct975 = quantile(mort_diff, 0.975, na.rm = TRUE),
            
            mort_rr_median = median(mort_rr, na.rm = TRUE),
            mort_rr_pct025 = quantile(mort_rr, 0.025, na.rm = TRUE),
            mort_rr_pct975 = quantile(mort_rr, 0.975, na.rm = TRUE),
            
            mort_rd_median = median(mort_rd, na.rm = TRUE),
            mort_rd_pct025 = quantile(mort_rd, 0.025, na.rm = TRUE),
            mort_rd_pct975 = quantile(mort_rd, 0.975, na.rm = TRUE),
            
            cla_change_median = median(cla_change, na.rm = TRUE),
            cla_change_pct025 = quantile(cla_change, 0.025, na.rm = TRUE),
            cla_change_pct975 = quantile(cla_change, 0.975, na.rm = TRUE),
            
            cla_total_median = median(cla_total, na.rm = TRUE),
            cla_total_pct025 = quantile(cla_total, 0.025, na.rm = TRUE),
            cla_total_pct975 = quantile(cla_total, 0.975, na.rm = TRUE),
            
            cla_rate_median = median(cla_rate, na.rm = TRUE),
            cla_rate_pct025 = quantile(cla_rate, 0.025, na.rm = TRUE),
            cla_rate_pct975 = quantile(cla_rate, 0.975, na.rm = TRUE),
            
            cla_change_adv_median = median(cla_change_adv, na.rm = TRUE),
            cla_change_adv_pct025 = quantile(cla_change_adv, 0.025, na.rm = TRUE),
            cla_change_adv_pct975 = quantile(cla_change_adv, 0.975, na.rm = TRUE),
            
            cla_rate_adv_median = median(cla_rate_adv, na.rm = TRUE),
            cla_rate_adv_pct025 = quantile(cla_rate_adv, 0.025, na.rm = TRUE),
            cla_rate_adv_pct975 = quantile(cla_rate_adv, 0.975, na.rm = TRUE),
            
            cla_diff_median = median(cla_diff, na.rm = TRUE),
            cla_diff_pct025 = quantile(cla_diff, 0.025, na.rm = TRUE),
            cla_diff_pct975 = quantile(cla_diff, 0.975, na.rm = TRUE),
            
            cla_rr_median = median(cla_rr, na.rm = TRUE),
            cla_rr_pct025 = quantile(cla_rr, 0.025, na.rm = TRUE),
            cla_rr_pct975 = quantile(cla_rr, 0.975, na.rm = TRUE),
            
            cla_rd_median = median(cla_rd, na.rm = TRUE),
            cla_rd_pct025 = quantile(cla_rd, 0.025, na.rm = TRUE),
            cla_rd_pct975 = quantile(cla_rd, 0.975, na.rm = TRUE),
            
            nutri_change_median = median(nutri_change, na.rm = TRUE),
            nutri_change_pct025 = quantile(nutri_change, 0.025, na.rm = TRUE),
            nutri_change_pct975 = quantile(nutri_change, 0.975, na.rm = TRUE),
            
            nutri_total_median = median(nutri_total, na.rm = TRUE),
            nutri_total_pct025 = quantile(nutri_total, 0.025, na.rm = TRUE),
            nutri_total_pct975 = quantile(nutri_total, 0.975, na.rm = TRUE),
            
            nutri_rate_median = median(nutri_rate, na.rm = TRUE),
            nutri_rate_pct025 = quantile(nutri_rate, 0.025, na.rm = TRUE),
            nutri_rate_pct975 = quantile(nutri_rate, 0.975, na.rm = TRUE),
            
            nutri_diff_median = median(nutri_diff, na.rm = TRUE),
            nutri_diff_pct025 = quantile(nutri_diff, 0.025, na.rm = TRUE),
            nutri_diff_pct975 = quantile(nutri_diff, 0.975, na.rm = TRUE),
            
            nutri_change_adv_median = median(nutri_change_adv, na.rm = TRUE),
            nutri_change_adv_pct025 = quantile(nutri_change_adv, 0.025, na.rm = TRUE),
            nutri_change_adv_pct975 = quantile(nutri_change_adv, 0.975, na.rm = TRUE),
            
            nutri_rate_adv_median = median(nutri_rate_adv, na.rm = TRUE),
            nutri_rate_adv_pct025 = quantile(nutri_rate_adv, 0.025, na.rm = TRUE),
            nutri_rate_adv_pct975 = quantile(nutri_rate_adv, 0.975, na.rm = TRUE),
            
            nutri_rr_median = median(nutri_rr, na.rm = TRUE),
            nutri_rr_pct025 = quantile(nutri_rr, 0.025, na.rm = TRUE),
            nutri_rr_pct975 = quantile(nutri_rr, 0.975, na.rm = TRUE),
            
            nutri_rd_median = median(nutri_rd, na.rm = TRUE),
            nutri_rd_pct025 = quantile(nutri_rd, 0.025, na.rm = TRUE),
            nutri_rd_pct975 = quantile(nutri_rd, 0.975, na.rm = TRUE),
            
            admis_change_median = median(admis_change, na.rm = TRUE),
            admis_change_pct025 = quantile(admis_change, 0.025, na.rm = TRUE),
            admis_change_pct975 = quantile(admis_change, 0.975, na.rm = TRUE),
            
            admis_total_median = median(admis_total, na.rm = TRUE),
            admis_total_pct025 = quantile(admis_total, 0.025, na.rm = TRUE),
            admis_total_pct975 = quantile(admis_total, 0.975, na.rm = TRUE),
            
            admis_rate_median = median(admis_rate, na.rm = TRUE),
            admis_rate_pct025 = quantile(admis_rate, 0.025, na.rm = TRUE),
            admis_rate_pct975 = quantile(admis_rate, 0.975, na.rm = TRUE),
            
            admis_change_adv_median = median(admis_change_adv, na.rm = TRUE),
            admis_change_adv_pct025 = quantile(admis_change_adv, 0.025, na.rm = TRUE),
            admis_change_adv_pct975 = quantile(admis_change_adv, 0.975, na.rm = TRUE),
            
            admis_rate_adv_median = median(admis_rate_adv, na.rm = TRUE),
            admis_rate_adv_pct025 = quantile(admis_rate_adv, 0.025, na.rm = TRUE),
            admis_rate_adv_pct975 = quantile(admis_rate_adv, 0.975, na.rm = TRUE),
            
            admis_diff_median = median(admis_diff, na.rm = TRUE),
            admis_diff_pct025 = quantile(admis_diff, 0.025, na.rm = TRUE),
            admis_diff_pct975 = quantile(admis_diff, 0.975, na.rm = TRUE),
            
            admis_rr_median = median(admis_rr, na.rm = TRUE),
            admis_rr_pct025 = quantile(admis_rr, 0.025, na.rm = TRUE),
            admis_rr_pct975 = quantile(admis_rr, 0.975, na.rm = TRUE),
            
            admis_rd_median = median(admis_rd, na.rm = TRUE),
            admis_rd_pct025 = quantile(admis_rd, 0.025, na.rm = TRUE),
            admis_rd_pct975 = quantile(admis_rd, 0.975, na.rm = TRUE),
    
    
    .groups = "drop"
  ) %>%
  mutate(mort_change_ci = sprintf("%.0f (%.0f, %.0f)", mort_change_median, mort_change_pct025, mort_change_pct975),
         mort_total_ci = sprintf("%.0f (%.0f, %.0f)", mort_total_median, mort_total_pct025, mort_total_pct975),
         mort_rate_ci = sprintf("%.0f (%.0f, %.0f)", mort_rate_median, mort_rate_pct025, mort_rate_pct975),
         mort_change_adv_ci = sprintf("%.0f (%.0f, %.0f)", mort_change_adv_median, mort_change_adv_pct025, mort_change_adv_pct975),
         mort_rate_adv_ci = sprintf("%.3f (%.3f, %.3f)", mort_rate_adv_median, mort_rate_adv_pct025, mort_rate_adv_pct975),
         mort_diff_ci = sprintf("%.0f (%.0f, %.0f)", mort_diff_median, mort_diff_pct025, mort_diff_pct975),
         mort_rr_ci = sprintf("%.3f (%.3f, %.3f)", mort_rr_median, mort_rr_pct025, mort_rr_pct975),
         mort_rd_ci = sprintf("%.3f (%.3f, %.3f)", mort_rd_median, mort_rd_pct025, mort_rd_pct975),
         
         cla_change_ci = sprintf("%.0f (%.0f, %.0f)", cla_change_median, cla_change_pct025, cla_change_pct975),
         cla_total_ci = sprintf("%.0f (%.0f, %.0f)", cla_total_median, cla_total_pct025, cla_total_pct975),
         cla_rate_ci = sprintf("%.0f (%.0f, %.0f)", cla_rate_median, cla_rate_pct025, cla_rate_pct975),
         cla_change_adv_ci = sprintf("%.0f (%.0f, %.0f)", cla_change_adv_median, cla_change_adv_pct025, cla_change_adv_pct975),
         cla_rate_adv_ci = sprintf("%.3f (%.3f, %.3f)", cla_rate_adv_median, cla_rate_adv_pct025, cla_rate_adv_pct975),
         cla_diff_ci = sprintf("%.0f (%.0f, %.0f)", cla_diff_median, cla_diff_pct025, cla_diff_pct975),
         cla_rr_ci = sprintf("%.3f (%.3f, %.3f)", cla_rr_median, cla_rr_pct025, cla_rr_pct975),
         cla_rd_ci = sprintf("%.3f (%.3f, %.3f)", cla_rd_median, cla_rd_pct025, cla_rd_pct975),
         
         nutri_change_ci = sprintf("%.0f (%.0f, %.0f)", nutri_change_median, nutri_change_pct025, nutri_change_pct975),
         nutri_total_ci = sprintf("%.0f (%.0f, %.0f)", nutri_total_median, nutri_total_pct025, nutri_total_pct975),
         nutri_rate_ci = sprintf("%.0f (%.0f, %.0f)", nutri_rate_median, nutri_rate_pct025, nutri_rate_pct975),
         nutri_change_adv_ci = sprintf("%.0f (%.0f, %.0f)", nutri_change_adv_median, nutri_change_adv_pct025, nutri_change_adv_pct975),
         nutri_rate_adv_ci = sprintf("%.3f (%.3f, %.3f)", nutri_rate_adv_median, nutri_rate_adv_pct025, nutri_rate_adv_pct975),
         nutri_diff_ci = sprintf("%.0f (%.0f, %.0f)", nutri_diff_median, nutri_diff_pct025, nutri_diff_pct975),
         nutri_rr_ci = sprintf("%.3f (%.3f, %.3f)", nutri_rr_median, nutri_rr_pct025, nutri_rr_pct975),
         nutri_rd_ci = sprintf("%.3f (%.3f, %.3f)", nutri_rd_median, nutri_rd_pct025, nutri_rd_pct975),
         
         admis_change_ci = sprintf("%.0f (%.0f, %.0f)", admis_change_median, admis_change_pct025, admis_change_pct975),
         admis_total_ci = sprintf("%.0f (%.0f, %.0f)", admis_total_median, admis_total_pct025, admis_total_pct975),
         admis_rate_ci = sprintf("%.0f (%.0f, %.0f)", admis_rate_median, admis_rate_pct025, admis_rate_pct975),
         admis_change_adv_ci = sprintf("%.0f (%.0f, %.0f)", admis_change_adv_median, admis_change_adv_pct025, admis_change_adv_pct975),
         admis_rate_adv_ci = sprintf("%.3f (%.3f, %.3f)", admis_rate_adv_median, admis_rate_adv_pct025, admis_rate_adv_pct975),
         admis_diff_ci = sprintf("%.0f (%.0f, %.0f)", admis_diff_median, admis_diff_pct025, admis_diff_pct975),
         admis_rr_ci = sprintf("%.3f (%.3f, %.3f)", admis_rr_median, admis_rr_pct025, admis_rr_pct975),
         admis_rd_ci = sprintf("%.3f (%.3f, %.3f)", admis_rd_median, admis_rd_pct025, admis_rd_pct975)) 








#### SII & RII dataset (no longer needed, ignore, see below) ####
RII_run_result_300524 <- output_2 %>%
  group_by(area_code, area_name, IMD_rank, scenario, run) %>%
  filter(year > 2023) %>% #filter for years above 2023 
  summarise(mort_total = sum(mort_total),
            mort_total_adv = sum(mort_total_adv),
            mort_pop = sum(mort_pop),
            
            cla_total = sum(cla_total),
            cla_total_adv = sum(cla_total_adv),
            cla_pop = sum(cla_pop),
            
            nutri_total = sum(nutri_total),
            nutri_total_adv = sum(nutri_total_adv),
            nutri_pop = sum(nutri_pop),
            
            admis_total = sum(admis_total),
            admis_total_adv = sum(admis_total_adv),
            admis_pop = sum(admis_pop),
            .groups = "drop") %>% 
  # Generate the summary data (of 1000 runs)
  group_by(area_code, area_name, IMD_rank, scenario) %>%
  summarise(
    
    mort_total_median = median(mort_total, na.rm = TRUE),
    mort_total_pct025 = quantile(mort_total, 0.025, na.rm = TRUE),
    mort_total_pct975 = quantile(mort_total, 0.975, na.rm = TRUE),
    
    mort_total_adv_median = median(mort_total_adv, na.rm = TRUE),
    mort_total_adv_pct025 = quantile(mort_total_adv, 0.025, na.rm = TRUE),
    mort_total_adv_pct975 = quantile(mort_total_adv, 0.975, na.rm = TRUE),
    
    mort_pop_median = median(mort_pop, na.rm = TRUE),
    
    cla_total_median = median(cla_total, na.rm = TRUE),
    cla_total_pct025 = quantile(cla_total, 0.025, na.rm = TRUE),
    cla_total_pct975 = quantile(cla_total, 0.975, na.rm = TRUE),
    
    cla_total_adv_median = median(cla_total_adv, na.rm = TRUE),
    cla_total_adv_pct025 = quantile(cla_total_adv, 0.025, na.rm = TRUE),
    cla_total_adv_pct975 = quantile(cla_total_adv, 0.975, na.rm = TRUE),
    
    cla_pop_median = median(cla_pop, na.rm = TRUE),
    
    nutri_total_median = median(nutri_total, na.rm = TRUE),
    nutri_total_pct025 = quantile(nutri_total, 0.025, na.rm = TRUE),
    nutri_total_pct975 = quantile(nutri_total, 0.975, na.rm = TRUE),
    
    nutri_total_adv_median = median(nutri_total_adv, na.rm = TRUE),
    nutri_total_adv_pct025 = quantile(nutri_total_adv, 0.025, na.rm = TRUE),
    nutri_total_adv_pct975 = quantile(nutri_total_adv, 0.975, na.rm = TRUE),
    
    nutri_pop_median = median(nutri_pop, na.rm = TRUE),
    
    admis_total_median = median(admis_total, na.rm = TRUE),
    admis_total_pct025 = quantile(admis_total, 0.025, na.rm = TRUE),
    admis_total_pct975 = quantile(admis_total, 0.975, na.rm = TRUE),
    
    admis_total_adv_median = median(admis_total_adv, na.rm = TRUE),
    admis_total_adv_pct025 = quantile(admis_total_adv, 0.025, na.rm = TRUE),
    admis_total_adv_pct975 = quantile(admis_total_adv, 0.975, na.rm = TRUE),
    
    admis_pop_median = median(admis_pop, na.rm = TRUE),
    
    
    .groups = "drop"
  ) 




#### SII & RII calculation (no longer needed, ignore, see below) ####

SII_result <- output_2 %>%
  group_by(scenario, run, IMD_rank) %>%
  filter(year > 2023) %>% #filter for years above 2023 
  summarise(mort_total = sum(mort_total),
            mort_total_adv = sum(mort_total_adv),
            mort_pop = sum(mort_pop),
            
            cla_total = sum(cla_total),
            cla_total_adv = sum(cla_total_adv),
            cla_pop = sum(cla_pop),
            
            nutri_total = sum(nutri_total),
            nutri_total_adv = sum(nutri_total_adv),
            nutri_pop = sum(nutri_pop),
            
            admis_total = sum(admis_total),
            admis_total_adv = sum(admis_total_adv),
            admis_pop = sum(admis_pop),
            
            .groups = "drop") %>%
  group_by(scenario, run) %>%
  arrange(IMD_rank, .by_group = TRUE) %>%
  mutate(mean_mort = mean(mort_total),
         mean_mort_adv = mean(mort_total_adv),
         total_pop_mort = sum(mort_pop),
         prop_pop_mort = mort_pop/total_pop_mort,
         
         cumulative_pro_mort = cumsum(prop_pop_mort),  
         rel_rank_mort = case_when(
           IMD_rank == 1 ~ 0.5*prop_pop_mort,
           IMD_rank != 1 ~ lag(cumulative_pro_mort) + 0.5*prop_pop_mort),
         sqr_prop_pop_mort = sqrt(prop_pop_mort), 
         relrank_sqr_proppop_mort = rel_rank_mort * sqr_prop_pop_mort,
         value_sqr_proppop_mort = sqr_prop_pop_mort * mort_total,
         value_sqr_proppop_mort_adv = sqr_prop_pop_mort * mort_total_adv,
         
         mean_cla = mean(cla_total),
         mean_cla_adv = mean(cla_total_adv),
         total_pop_cla = sum(cla_pop),
         prop_pop_cla = cla_pop/total_pop_cla,
         
         cumulative_pro_cla = cumsum(prop_pop_cla),  
         rel_rank_cla = case_when(
           IMD_rank == 1 ~ 0.5*prop_pop_cla,
           IMD_rank != 1 ~ lag(cumulative_pro_cla) + 0.5*prop_pop_cla),
         sqr_prop_pop_cla = sqrt(prop_pop_cla), 
         relrank_sqr_proppop_cla = rel_rank_cla * sqr_prop_pop_cla,
         value_sqr_proppop_cla = sqr_prop_pop_cla * cla_total,
         value_sqr_proppop_cla_adv = sqr_prop_pop_cla * cla_total_adv,
         
         mean_nutri = mean(nutri_total),
         mean_nutri_adv = mean(nutri_total_adv),
         total_pop_nutri = sum(nutri_pop),
         prop_pop_nutri = nutri_pop/total_pop_nutri,
         
         cumulative_pro_nutri = cumsum(prop_pop_nutri),  
         rel_rank_nutri = case_when(
           IMD_rank == 1 ~ 0.5*prop_pop_nutri,
           IMD_rank != 1 ~ lag(cumulative_pro_nutri) + 0.5*prop_pop_nutri),
         sqr_prop_pop_nutri = sqrt(prop_pop_nutri), 
         relrank_sqr_proppop_nutri = rel_rank_nutri * sqr_prop_pop_nutri,
         value_sqr_proppop_nutri = sqr_prop_pop_nutri * nutri_total,
         value_sqr_proppop_nutri_adv = sqr_prop_pop_nutri * nutri_total_adv,
         
         mean_admis = mean(admis_total),
         mean_admis_adv = mean(admis_total_adv),
         total_pop_admis = sum(admis_pop),
         prop_pop_admis = admis_pop/total_pop_admis,
         
         cumulative_pro_admis = cumsum(prop_pop_admis),  
         rel_rank_admis = case_when(
           IMD_rank == 1 ~ 0.5*prop_pop_admis,
           IMD_rank != 1 ~ lag(cumulative_pro_admis) + 0.5*prop_pop_admis),
         sqr_prop_pop_admis = sqrt(prop_pop_admis), 
         relrank_sqr_proppop_admis = rel_rank_admis * sqr_prop_pop_admis,
         value_sqr_proppop_admis = sqr_prop_pop_admis * admis_total,
         value_sqr_proppop_admis_adv = sqr_prop_pop_admis * admis_total_adv,
         
         
         .groups = "drop")



test <- SII_result %>% 
  group_by(scenario, run) %>%
  nest() %>% 
  mutate(model_mort = map(data, ~ lm(value_sqr_proppop_mort ~ sqr_prop_pop_mort + relrank_sqr_proppop_mort + 0, data = .)),
         mort_sii = -1 * as.numeric(map(map(model_mort, "coefficients"), "relrank_sqr_proppop_mort")),
         model_mort_adv = map(data, ~ lm(value_sqr_proppop_mort_adv ~ sqr_prop_pop_mort + relrank_sqr_proppop_mort + 0, data = .)),
         mort_adv_sii = -1 * as.numeric(map(map(model_mort_adv, "coefficients"), "relrank_sqr_proppop_mort")),
         mort_diff = mort_sii-mort_adv_sii,
         mort_ratio = mort_sii/mort_adv_sii,
         
         model_cla = map(data, ~ lm(value_sqr_proppop_cla ~ sqr_prop_pop_cla + relrank_sqr_proppop_cla + 0, data = .)),
         cla_sii = -1 * as.numeric(map(map(model_cla, "coefficients"), "relrank_sqr_proppop_cla")),
         model_cla_adv = map(data, ~ lm(value_sqr_proppop_cla_adv ~ sqr_prop_pop_cla + relrank_sqr_proppop_cla + 0, data = .)),
         cla_adv_sii = -1 * as.numeric(map(map(model_cla_adv, "coefficients"), "relrank_sqr_proppop_cla")),
         cla_diff = cla_sii-cla_adv_sii,
         cla_ratio = cla_sii/cla_adv_sii,
         
         model_nutri = map(data, ~ lm(value_sqr_proppop_nutri ~ sqr_prop_pop_nutri + relrank_sqr_proppop_nutri + 0, data = .)),
         nutri_sii = -1 * as.numeric(map(map(model_nutri, "coefficients"), "relrank_sqr_proppop_nutri")),
         model_nutri_adv = map(data, ~ lm(value_sqr_proppop_nutri_adv ~ sqr_prop_pop_nutri + relrank_sqr_proppop_nutri + 0, data = .)),
         nutri_adv_sii = -1 * as.numeric(map(map(model_nutri_adv, "coefficients"), "relrank_sqr_proppop_nutri")),
         nutri_diff = nutri_sii-nutri_adv_sii,
         nutri_ratio = nutri_sii/nutri_adv_sii,
         
         model_admis = map(data, ~ lm(value_sqr_proppop_admis ~ sqr_prop_pop_admis + relrank_sqr_proppop_admis + 0, data = .)),
         admis_sii = -1 * as.numeric(map(map(model_admis, "coefficients"), "relrank_sqr_proppop_admis")),
         model_admis_adv = map(data, ~ lm(value_sqr_proppop_admis_adv ~ sqr_prop_pop_admis + relrank_sqr_proppop_admis + 0, data = .)),
         admis_adv_sii = -1 * as.numeric(map(map(model_admis_adv, "coefficients"), "relrank_sqr_proppop_admis")),
         admis_diff = admis_sii-admis_adv_sii,
         admis_ratio = admis_sii/admis_adv_sii,
         
         .groups = "drop") 



test2 <- SII_result %>%
  filter(scenario == 'cp_high' & run == 1) %>%
  group_by(scenario, run) %>%
  nest() %>% 
  mutate(model_mort = map(data, ~ lm(value_sqr_proppop_mort ~ sqr_prop_pop_mort + relrank_sqr_proppop_mort + 0, data = .)),
         mort_sii = -1 * as.numeric(map(map(model_mort, "coefficients"), "relrank_sqr_proppop_mort")),
         model_mort_adv = map(data, ~ lm(value_sqr_proppop_mort_adv ~ sqr_prop_pop_mort + relrank_sqr_proppop_mort + 0, data = .)),
         mort_adv_sii = -1 * as.numeric(map(map(model_mort_adv, "coefficients"), "relrank_sqr_proppop_mort")),
         mort_diff = mort_sii-mort_adv_sii,
         mort_ratio = mort_sii/mort_adv_sii,
         
         model_cla = map(data, ~ lm(value_sqr_proppop_cla ~ sqr_prop_pop_cla + relrank_sqr_proppop_cla + 0, data = .)),
         cla_sii = -1 * as.numeric(map(map(model_cla, "coefficients"), "relrank_sqr_proppop_cla")),
         model_cla_adv = map(data, ~ lm(value_sqr_proppop_cla_adv ~ sqr_prop_pop_cla + relrank_sqr_proppop_cla + 0, data = .)),
         cla_adv_sii = -1 * as.numeric(map(map(model_cla_adv, "coefficients"), "relrank_sqr_proppop_cla")),
         cla_diff = cla_sii-cla_adv_sii,
         cla_ratio = cla_sii/cla_adv_sii,
         
         model_nutri = map(data, ~ lm(value_sqr_proppop_nutri ~ sqr_prop_pop_nutri + relrank_sqr_proppop_nutri + 0, data = .)),
         nutri_sii = -1 * as.numeric(map(map(model_nutri, "coefficients"), "relrank_sqr_proppop_nutri")),
         model_nutri_adv = map(data, ~ lm(value_sqr_proppop_nutri_adv ~ sqr_prop_pop_nutri + relrank_sqr_proppop_nutri + 0, data = .)),
         nutri_adv_sii = -1 * as.numeric(map(map(model_nutri_adv, "coefficients"), "relrank_sqr_proppop_nutri")),
         nutri_diff = nutri_sii-nutri_adv_sii,
         nutri_ratio = nutri_sii/nutri_adv_sii,
         
         model_admis = map(data, ~ lm(value_sqr_proppop_admis ~ sqr_prop_pop_admis + relrank_sqr_proppop_admis + 0, data = .)),
         admis_sii = -1 * as.numeric(map(map(model_admis, "coefficients"), "relrank_sqr_proppop_admis")),
         model_admis_adv = map(data, ~ lm(value_sqr_proppop_admis_adv ~ sqr_prop_pop_admis + relrank_sqr_proppop_admis + 0, data = .)),
         admis_adv_sii = -1 * as.numeric(map(map(model_admis_adv, "coefficients"), "relrank_sqr_proppop_admis")),
         admis_diff = admis_sii-admis_adv_sii,
         admis_ratio = admis_sii/admis_adv_sii,
         
         .groups = "drop") 





#### SII & RII calculation, EI #####
tictoc::tic("SII calculation")
SII_result <- output_2 %>%
  #filter(run <= 10) %>%
  filter(year > 2023) %>% #filter for years above 2023
  group_by(scenario, run, IMD_rank) %>%
  summarise(
    across(ends_with(c("_total", "_total_adv", "pop")), sum),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = !c(scenario, run, IMD_rank),
    names_to = c("outcome", ".value"),
    names_pattern = "([^_]*)_(.*)"
  ) %>%
  group_by(scenario, run, outcome) %>%
  # Sort from least to most deprived (so we'll get a positive SII)
  arrange(-IMD_rank, .by_group = TRUE) %>%
  mutate(
    # Important: all outcomes are per 100,000 population!
    rate = 100000*total/pop,
    rate_adv = 100000*total_adv/pop,
    cumpop = cumsum(pop),
    cumpop_midpoint = cumpop - 0.5*pop,
    rank = cumpop_midpoint / sum(pop)
  ) %>%
  view()
  nest() %>%
  mutate(
    model     = map(data, ~lm(rate ~ rank, data = .)),
    model_adv = map(data, ~lm(rate_adv ~ rank, data = .)),
    
    slope         = map_dbl(model, ~coef(summary(.))["rank", "Estimate"]),
    intercept     = map_dbl(model, ~coef(summary(.))["(Intercept)", "Estimate"]),
    slope_adv     = map_dbl(model_adv, ~coef(summary(.))["rank", "Estimate"]),
    intercept_adv = map_dbl(model_adv, ~coef(summary(.))["(Intercept)", "Estimate"]),
  ) %>%
  transmute(
    scenario,
    run,
    outcome,
    
    sii = slope,
    sii_adv = slope_adv,
    rii = (intercept + slope) / intercept,
    rii_adv = (intercept_adv + slope_adv) / intercept_adv,
    
    sii_diff = sii - sii_adv,
    sii_ratio = sii / sii_adv,
    rii_diff = rii - rii_adv,
    rii_ratio = rii / rii_adv,
  ) %>%
  group_by(scenario, outcome) %>%
  summarise(
    sii_median = median(sii),
    sii_pct025 = quantile(sii, 0.025),
    sii_pct975 = quantile(sii, 0.975),
    
    sii_adv_median = median(sii_adv),
    sii_adv_pct025 = quantile(sii_adv, 0.025),
    sii_adv_pct975 = quantile(sii_adv, 0.975),
    
    sii_diff_median = median(sii_diff),
    sii_diff_pct025 = quantile(sii_diff, 0.025),
    sii_diff_pct975 = quantile(sii_diff, 0.975),
    
    sii_ratio_median = median(sii_ratio),
    sii_ratio_pct025 = quantile(sii_ratio, 0.025),
    sii_ratio_pct975 = quantile(sii_ratio, 0.975),
    
    rii_median = median(rii),
    rii_pct025 = quantile(rii, 0.025),
    rii_pct975 = quantile(rii, 0.975),
    
    rii_adv_median = median(rii_adv),
    rii_adv_pct025 = quantile(rii_adv, 0.025),
    rii_adv_pct975 = quantile(rii_adv, 0.975),
    
    rii_diff_median = median(rii_diff),
    rii_diff_pct025 = quantile(rii_diff, 0.025),
    rii_diff_pct975 = quantile(rii_diff, 0.975),
    
    rii_ratio_median = median(rii_ratio),
    rii_ratio_pct025 = quantile(rii_ratio, 0.025),
    rii_ratio_pct975 = quantile(rii_ratio, 0.975),
    .groups = "drop"
  ) %>%
  mutate(
    sii_ci = sprintf("%.2f (%.2f, %.2f)", sii_median, sii_pct025, sii_pct975),
    sii_adv_ci = sprintf("%.2f (%.2f, %.2f)", sii_adv_median, sii_adv_pct025, sii_adv_pct975),
    sii_diff_ci = sprintf("%.2f (%.2f, %.2f)", sii_diff_median, sii_diff_pct025, sii_diff_pct975),
    sii_ratio_ci = sprintf("%.2f (%.2f, %.2f)", sii_ratio_median, sii_ratio_pct025, sii_ratio_pct975),
    
    rii_ci = sprintf("%.4f (%.4f, %.4f)", rii_median, rii_pct025, rii_pct975),
    rii_adv_ci = sprintf("%.4f (%.4f, %.4f)", rii_adv_median, rii_adv_pct025, rii_adv_pct975),
    rii_diff_ci = sprintf("%.4f (%.4f, %.4f)", rii_diff_median, rii_diff_pct025, rii_diff_pct975),
    rii_ratio_ci = sprintf("%.4f (%.4f, %.4f)", rii_ratio_median, rii_ratio_pct025, rii_ratio_pct975)) 



tictoc::toc()
#### export output ####
write_csv(base_run_result,"base_run_rawdata_220324.csv")
write_csv(area_run_result,"area_run_rawdata_170524.csv")
write_csv(national_run_result,"national_run_rawdata_220324.csv")
write_csv(IMD_run_result,"IMD_run_rawdata_220324.csv")
write_csv(IMD_trend,"IMD_trend_rawdata_220324.csv")
write_csv(national_trend,"national_trend_rawdata_220324.csv")
write_csv(SII_result,"SII_result_030624.csv")


