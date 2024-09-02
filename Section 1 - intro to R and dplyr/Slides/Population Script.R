# load packages
library(readr)
library(dplyr)

# Read in population data
pops <- read_rds("/conf/LIST_analytics/West Hub/03 - Training/Posit Training Cross Team/Introduction to R/Session 1/Data/dz_pops.rds")

# Subset rows and columns using square bracket notation
pops[c(1, 2, 3), c(2, 3)]
## All of column 2
pops[ , 2]
## All of row 2
pops[2, ]

# subsetting columns by name
## dollar notation
pops$pop
## square bracket notation
pops[ , "pop"]


# Summarising data
summary(pops)
mean(pops$pop)
unique(pops$sex)
unique(pops$year)


# Logical statements
pops$year != 2014

pops$year == 2011 | pops$year == 2015

pops$year %in% c(2011, 2015)


# Filter
pops_2021 <- filter(pops, year == 2021)

# Select
pops_2021_1 <- select(pops_2021, c(year, datazone2011, sex, pop))
pops_2021_2 <- select(pops_2021, !c(datazone2011, datazone2011name))
pops_2021_3 <- select(pops_2021, -c(datazone2011, datazone2011name)) # same as above
pops_2021_4 <- select(pops_2021, !starts_with("datazone2011")) # select all columns which don't start with datazone2011 
pops_2021_4 <- select(pops_2021, where(is.character)) # select all of the columns that contain text
pops_2021_5 <- select(pops_2021, c(year:datazone2011name, pop)) # not recommended

# Summarise

populations <- read_csv("/conf/linkage/output/lookups/Unicode/Populations/Estimates/HSCP2019_pop_est_1981_2022.csv")                                       
populations

populations_2022 <- populations %>% 
  filter(year == max(year)) %>% 
  select(-c(sex, hscp2018, hscp2016))

pop1 <- populations_2022 %>% 
  group_by(hscp2019, hscp2019name, age) %>% 
  summarise(pop = sum(pop)) %>% 
  mutate(prop = pop/sum(pop))


pop2 <- populations_2022 %>% 
  group_by(hscp2019, hscp2019name, age) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() %>% 
  mutate(prop = pop/sum(pop))

## Joins
pres_url <- "https://www.opendata.nhs.scot/dataset/4b4be829-c15e-480e-a2fc-996460ff63c6/resource/fa5bbede-475a-4ca9-a71f-3d521657e7c6/download/prescribed-dispensed-annual-2024.csv"
pres_dat <- read_csv(pres_url)

disp_pres <- pres_dat %>% 
  group_by(DispenserLocation) %>% 
  summarise(combined_items = sum(NumberOfPaidItems),
            DispenserLocationPostcode = unique(DispenserLocationPostcode)) %>% 
  mutate(DispenserLocation = as.character(DispenserLocation))


all_disp <- pres_dat %>% 
  summarise(combined_items = sum(NumberOfPaidItems),
            DispenserLocation = "All",
            DispenserLocationPostcode = "All")

pres_dat %>%
  summarise(combined_items = sum(NumberOfPaidItems),
            DispenserLocation = "All",
            DispenserLocationPostcode = "All") %>%
  bind_rows(disp_pres, .) %>%
  head()

all_disp1 <- pres_dat %>% 
  summarise(combined_items = sum(NumberOfPaidItems))

bind_rows(all_disp1, disp_pres)

all_disp1 <- pres_dat %>% 
  summarise(combined_items = sum(NumberOfPaidItems),
            DispenserLocation = "All",
            DispenserLocationPoostcode = "All")

bind_rows(all_disp1, disp_pres)

PCs <- read_csv("/conf/linkage/output/lookups/Unicode/Geography/ONS Postcode Lookup/ONSPD_MAY_2022_UK.csv") %>% 
  select(pcd, oslaua) %>% 
  mutate(pcd = phsmethods::format_postcode(pcd, "pc7"))

disp_pres1 <- disp_pres %>%
  filter(!is.na(DispenserLocationPostcode)) %>% 
  mutate(DispenserLocationPostcode = phsmethods::format_postcode(DispenserLocationPostcode, "pc7"))

disp_pres1%>% 
  left_join(PCs, by = join_by(DispenserLocationPostcode == pcd))

PCs %>% 
  right_join(disp_pres1, by = join_by(pcd == DispenserLocationPostcode))

disp_pres %>%
  filter(!is.na(DispenserLocationPostcode)) %>% 
  mutate(DispenserLocationPostcode = phsmethods::format_postcode(DispenserLocationPostcode, "pc7")) %>% 
  right_join(PCs, ., by = join_by(pcd == DispenserLocationPostcode))


disp_pres1%>% 
  right_join(PCs, by = join_by(DispenserLocationPostcode == pcd))


