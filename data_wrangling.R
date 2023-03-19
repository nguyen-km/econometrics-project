library(tidyverse)
library(readxl)
library(lubridate)

setwd("/data")

#import
colorado <- read_csv("Unemployment_Estimates_in_Colorado.csv")
panel <- read_csv("covidpanel.csv")
inc_party <- read_csv("covidincomepartyMetro.csv")

## Utah unemployment data import
jan20 <- read_xlsx("ut_jan20.xlsx")
jan19 <- read_xlsx("ut_jan19.xlsx")
feb20 <- read_xlsx("ut_feb20.xlsx")
feb19 <- read_xlsx("ut_feb19.xlsx")
mar20 <- read_xlsx("ut_mar20.xlsx")
mar19 <- read_xlsx("ut_mar19.xlsx")
apr20 <- read_xlsx("ut_apr20.xlsx")
apr19 <- read_xlsx("ut_apr19.xlsx")
may20 <- read_xlsx("ut_may20.xlsx")
may19 <- read_xlsx("ut_may19.xlsx")
jun20 <- read_xlsx("ut_jun20.xlsx")
jun19 <- read_xlsx("ut_jun19.xlsx")
jul20 <- read_xlsx("ut_jul20.xlsx")
jul19 <- read_xlsx("ut_jul19.xlsx")
aug20 <- read_xlsx("ut_aug20.xlsx")
aug19 <- read_xlsx("ut_aug19.xlsx")
sep20 <- read_xlsx("ut_sep20.xlsx")
sep19 <- read_xlsx("ut_sep19.xlsx")
oct20 <- read_xlsx("ut_oct20.xlsx")
oct19 <- read_xlsx("ut_oct19.xlsx")
nov19 <- read_xlsx("ut_nov19.xlsx")
dec19 <- read_xlsx("ut_dec19.xlsx")


# Merge all Utah data into one data frame
utah <- full_join(jan20, jan19) %>% full_join(feb20) %>% full_join(feb19) %>% full_join(mar20) %>% 
  full_join(mar19) %>% full_join(apr20) %>% full_join(apr19) %>% full_join(may20) %>% full_join(may19) %>%
  full_join(jun20) %>% full_join(jun19) %>% full_join(jul20) %>% full_join(jul19) %>% full_join(aug20) %>%
  full_join(aug19) %>% full_join(sep20) %>% full_join(sep19)%>% full_join(oct20) %>% full_join(oct19) %>%
  full_join(nov19) %>% full_join(dec19) %>% rename(County = ...1,
                                                   unemprate = "Unemployment Rate",
                                                   laborforce = "Labor Force") %>% # rename some variables
  filter(region == "County") # Keep only county data


# only Utah 
inc_partyUT <- filter(inc_party, State == "UT")
panelUT <- panel %>% filter(State == "UT") %>% rename(County = County.Name)

# months with 31 days
odd <- c(1, 3, 5, 7,8, 10, 12)

utah <- utah %>% filter(year == 2020)
  
# add dates to utah data frame (last day of month for monthly data)
utah <- utah %>% rowwise() %>% mutate(dates = make_date(year, month, if(month %in% odd){31} 
                                          else if (month == 2 & year == 2020){29} 
                                          else if (month == 2) {28}
                                          else {30})) 
  
#merge income party metro and panel data to Utah data
utah <- utah %>% left_join(inc_partyUT) %>% left_join(panelUT)

utah<- utah %>% rowwise() %>% mutate(cumcasepc  = coalesce(cumcasepc, 0),
                                             newcases = coalesce(newcases, 0),
                                             roll_mean = coalesce(roll_mean, 0),
                                             cum_cases = coalesce(cum_cases, 0),
                                             after = ifelse(year == 2020 & month > 3, 1, 0)) # after stay at home order
                                             

urbanUT <- c("Salt Lake County")
suburbanUT <- c("Cache County", "Weber County", "Davis County", "Utah County", 
                "Washington County")


utah <- utah %>% rowwise() %>% 
  mutate(designation = if(County %in% urbanUT){"urban"} 
         else if (County %in% suburbanUT){"suburban"} 
         else {"rural"})

utah <- utah %>% rowwise() %>% filter(dates <= ymd("2020-09-30")) 

write_csv(utah, "MAIN_ut.csv")

## Colorado 


panel <- panel %>% filter(State == "CO") %>% rename(County = County.Name) # only Colorado, change county variable
panel$County <- gsub("Broomfield County and City", "Broomfield County", panel$County) # fix Broomfield

# only county data from 2019 onward

colorado <- colorado %>% filter(periodyear == 2020,
                                pertypdesc == "Monthly", 
                                areatyname == "County",
                                benchmark == 2019) %>% 
  rename(County = areaname) %>% 
  mutate(after = ifelse(period > 3 & periodyear == 2020, 1, 0)) %>% # add after dummy
  rowwise() %>% mutate(dates = make_date(periodyear, period, if(period %in% odd){31} 
                                         else if (period == 2 & periodyear == 2020){29} 
                                         else if (period == 2) {28}
                                         else {30})) # add dates
                                      
aco <- colorado %>% filter(County == "Adams County", periodyear == 2019)
# only CO data
inc_partyCO <- filter(inc_party, State == "CO")
inc_partyCO[8,2] <- "Broomfield County" # fix Broomfield

# Urban, Suburban, Rural
urban <- c("Adams County", "Arapahoe County", "Boulder County", "Denver County", "El Paso County", "Jefferson County")
suburban <- c("Broomfield County", "Douglas County")

colorado <- colorado %>% rowwise() %>% 
  mutate(designation = if(County %in% urban){"urban"} 
         else if (County %in% suburban){"suburban"} 
         else {"rural"})

colorado <- colorado %>% merge(inc_partyCO, by = "County") # merge demographic data

#merge covid data
colorado <- colorado %>% left_join(panel)

colorado<- colorado %>% rowwise() %>% mutate(cumcasepc  = coalesce(cumcasepc, 0),
                                             newcases = coalesce(newcases, 0),
                                             roll_mean = coalesce(roll_mean, 0),
                                             cum_cases = coalesce(cum_cases, 0),
                                             after = ifelse(periodyear == 2020 & period > 3, 1, 0)) # after stay at home order
                                             


write_csv(colorado, "MAIN_co.csv")
