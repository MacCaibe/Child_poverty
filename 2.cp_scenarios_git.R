library(tidyverse)


data <- read_csv("cp_data_complete.csv") %>%
  filter(area_code != "E06000060")

data <- data %>% 
  select(area_code, area_name, area, year, poverty_rate_rel_bhc_U16, poverty_rate_rel_ahc_U16, CLA_started_U16, pop_U16,
         mortality, live_births, events_34, pop_34, events_03, pop_03, IMD_tertile, IMD_rank_avg_rank) %>%
  rename(IMD_rank = IMD_rank_avg_rank)

names(data)

table(data$IMD_rank)


## increase study time frame by adding rows until end date for each area

end_date <- 2033

data_rows <- data %>%
  group_by(area_code) %>%
  group_modify(~ add_row(., year=seq(max(.$year)+1,
                                     end_date,
                                     by=1),
                         area_code=.$area_code[.$year==2020],
                         area_name=.$area_name[.$year==2020],
                         area =.$area[.$year==2020],
                         poverty_rate_rel_bhc_U16=.$poverty_rate_rel_bhc_U16[.$year==2020],
                         poverty_rate_rel_ahc_U16=.$poverty_rate_rel_ahc_U16[.$year==2020],
                         CLA_started_U16=.$CLA_started_U16[.$year==2020],
                         pop_U16=.$pop_U16[.$year==2020],
                         mortality=.$mortality[.$year==2020],
                         live_births=.$live_births[.$year==2020],
                         IMD_tertile=.$IMD_tertile[.$year==2020],
                         IMD_rank=.$IMD_rank[.$year==2020],
                         .after = max(.$year)
  ))



## extend the hosp variables from 2019 to end of study period

data_rows_2 <- data_rows %>%
  fill(events_34, .direction = "down") %>%
  fill(pop_34, .direction = "down") %>%
  fill(events_03, .direction = "down") %>%
  fill(pop_03, .direction = "down")


data_scen_base <- data_rows_2 %>%
  mutate(base_change = if_else(year<2021, 1, if_else(
    year==2021, 1.0279,if_else(
      year==2022, 1.0511, if_else(
        year==2023, 1.0703,if_else(
          year==2024, 1.0863,if_else(
            year==2025, 1.0996, if_else(
              year==2026, 1.1106, if_else(
                year==2027, 1.1198, if_else(
                  year==2028, 1.1274, if_else(
                    year==2029, 1.1337, if_else(
                      year==2030, 1.1390, if_else(
                        year==2031, 1.1434, if_else(
                          year==2032, 1.1470, 1.1500)))))))))))))) %>%
  mutate(cp_baseline = poverty_rate_rel_bhc_U16*base_change)



data_scen_complete <- data_scen_base %>%
  mutate(low_change = if_else(year<2024, 1, if_else(
    year==2024, 1.0000,if_else(
      year==2025, 1.0000, if_else(
        year==2026, 1.0000,if_else(
          year==2027, 0.9928,if_else(
            year==2028, 0.9831, if_else(
              year==2029, 0.9699, if_else(
                year==2030, 0.9520, if_else(
                  year==2031, 0.9277, if_else(
                    year==2032, 0.8947, 0.8500)))))))))),
    med_change = if_else(year<2024, 1, if_else(
      year==2024, 1.0000,if_else(
        year==2025, 1.0000, if_else(
          year==2026, 0.9902,if_else(
            year==2027, 0.9772,if_else(
              year==2028, 0.9601, if_else(
                year==2029, 0.9376, if_else(
                  year==2030, 0.9080, if_else(
                    year==2031, 0.8690, if_else(
                      year==2032, 0.8176, 0.7500)))))))))),
    high_change = if_else(year<2024, 1, if_else(
      year==2024, 1.0000,if_else(
        year==2025, 0.9883, if_else(
          year==2026, 0.9734,if_else(
            year==2027, 0.9541,if_else(
              year==2028, 0.9294, if_else(
                year==2029, 0.8976, if_else(
                  year==2030, 0.8567, if_else(
                    year==2031, 0.8042, if_else(
                      year==2032, 0.7367, 0.6500)))))))))))  

data_scen_complete2 <- data_scen_complete %>%
  mutate(cp_low= if_else(year<2024, cp_baseline, cp_baseline[year==2023]*low_change),
         cp_med= if_else(year<2024, cp_baseline, cp_baseline[year==2023]*med_change),
         cp_high= if_else(year<2024, cp_baseline, cp_baseline[year==2023]*high_change))



data_scen_complete3 <- data_scen_complete2 %>%
  select(-base_change, -low_change, -med_change, -high_change)


## save file

write.csv(data_scen_complete3, file = "cp_data_complete_analysis_200324.csv")

















