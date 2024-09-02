library(readr)
library(stringr)

dd_beddays <- read_csv("/conf/LIST_analytics/West Hub/03 - Training/Posit Training Cross Team/Introduction to R/Session 1/Data/2024-02_delayed-discharge-beddays-council-area.csv")
colnames(dd_beddays)
dd_beddays$MonthOfDelay
unique(dd_beddays$MonthOfDelay)
unique(dd_beddays$ReasonForDelay)

# Within the data we are interested in delayed discharges from 2022 onwards. We want to pull out information
# on the total number of delayed bed days within each year for Code 9 reasons, non-code 9 reasons and a total
# for each council area and separated into two age groups: 18-74, and 75+.

dd_beddays_2022 <- dd_beddays %>% 
  #Filter out observations where the first 4 characters of MonthofDelay is >= 2022
  filter(str_sub(MonthOfDelay, start = 1, end = 4) >= 2022) %>% 
  # Exclude columns which end with "QF"
  select(!ends_with("QF"))
  

# These two lines are the equivalent of the above code without using the pipeline
# The pipe makes code more readable, and flows more easily - can be read as "and then"
dd_beddays_2022 <- filter(dd_beddays, str_sub(MonthOfDelay, start = 1, end = 4) >= 2022) 
dd_beddays_2022 <- select(dd_beddays_2022, !ends_with("QF"))

           

         