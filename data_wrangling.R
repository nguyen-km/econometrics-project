library(tidyverse)
library(readxl)
library(lubridate)

setwd("/Users/kevnguyen/Library/CloudStorage/GoogleDrive-keng2413@colorado.edu/My Drive/archive/ECON/ECON 4848 - Applied Econometrics/econ4848_project/data/")

#import
colorado <- read_csv("colorado/Unemployment_Estimates_in_Colorado.csv")
panel <- read_csv("covidpanel.csv") #covid data
inc_party <- read_csv("covidincomepartyMetro.csv") # political data

#Merge all utah data
mos = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 
        'nov', 'dec')
year = c('19', '20')

#Import individual month data
for (i in 1:length(mos)) {
  for(j in 1:length(year)){
    if((i == 11 || i == 12) && j == 2){
      break
    } else {
      path = paste('utah/ut_', mos[i], year[j], '.xlsx', sep = '')
      tmp_df = read_xlsx(path)
      if(i == 1 && j == 1){
        utah = tmp_df # init df
      }
      utah = utah %>% full_join(tmp_df) # join all utah data
    }
  }
}

utah = utah %>% rename(County = ...1,
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
                                             


write_csv(colorado, "MAIN_co.csv") #export final data

