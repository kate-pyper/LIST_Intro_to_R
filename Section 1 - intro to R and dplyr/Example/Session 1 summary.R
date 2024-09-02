#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# RStudio Workbench is strictly for use by Public Health Scotland staff and     
# authorised users only, and is governed by the Acceptable Usage Policy https://github.com/Public-Health-Scotland/R-Resources/blob/master/posit_workbench_acceptable_use_policy.md.
#
# This is a shared resource and is hosted on a pay-as-you-go cloud computing
# platform.  Your usage will incur direct financial cost to Public Health
# Scotland.  As such, please ensure
#
#   1. that this session is appropriately sized with the minimum number of CPUs
#      and memory required for the size and scale of your analysis;
#   2. the code you write in this script is optimal and only writes out the
#      data required, nothing more.
#   3. you close this session when not in use; idle sessions still cost PHS
#      money!
#
# For further guidance, please see https://github.com/Public-Health-Scotland/R-Resources/blob/master/posit_workbench_best_practice_with_r.md.
#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

library(dplyr)
library(readr)
library(lubridate)

dd_beddays <- read_csv("/conf/LIST_analytics/West Hub/03 - Training/Posit Training Cross Team/Introduction to R/Session 1/Data/2024-02_delayed-discharge-beddays-council-area.csv")

dd_beddays %>% 
  select(MonthOfDelay, CA, AgeGroup, ReasonForDelay, NumberOfDelayedBedDays) %>% 
  filter(MonthOfDelay >= 202201, AgeGroup != "18plus") %>%
  mutate(Reason = case_when(ReasonForDelay == "All Delay Reasons" ~"All Reasons",
                            ReasonForDelay %in% c("Code 9 AWI", "Code 9 Non-AWI")~"Code 9",
                            .default = "Non Code 9"),
         year_month = ym(MonthOfDelay),
         year = year(year_month)) %>% 
  group_by(CA, year, AgeGroup, Reason) %>% 
  summarise(DelayedBedDays = sum(NumberOfDelayedBedDays))
  

ca_lookup <- read_csv("/conf/LIST_analytics/West Hub/03 - Training/Posit Training Cross Team/Introduction to R/Session 1/Data/ca11_ca19.csv")

ca_lookup <- select(ca_lookup, CA, HBName) %>% distinct()

dd_beddays %>% 
  select(MonthOfDelay, CA, AgeGroup, ReasonForDelay, NumberOfDelayedBedDays) %>% 
  filter(MonthOfDelay >= 202201, AgeGroup != "18plus") %>%
  mutate(Reason = case_when(ReasonForDelay == "All Delay Reasons" ~"All Reasons",
                            ReasonForDelay %in% c("Code 9 AWI", "Code 9 Non-AWI")~"Code 9",
                            .default = "Non Code 9"),
         year_month = ym(MonthOfDelay),
         year = year(year_month)) %>% 
  left_join(ca_lookup, by = "CA") %>% 
  group_by(HBName, year, AgeGroup, Reason) %>% 
  summarise(DelayedBedDays = sum(NumberOfDelayedBedDays))

 