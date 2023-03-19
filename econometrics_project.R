library(tidyverse)
setwd("/data")

colorado <- read_csv("MAIN_co.csv")


#Metro vs non Metro
model1 <- lm(unemprate ~ metro_binary*after, data = colorado)
summary(model1)

# Urban, suburban, rural
model2 <- lm(unemprate ~ designation*after, data = colorado)
summary(model2)

#excluding rural counties
coMetro <- colorado %>% filter(designation != "rural")
model3 <- lm(unemprate ~ designation*after + cumcasepc + laborforce, data = coMetro)
summary(model3)

# income 
coInc <- coMetro %>% mutate(quantile = ntile(AGI, 3)) 
coInc <- coInc %>% rowwise() %>% mutate(quant_cat = if(quantile == 1){"low"} 
                                          else if(quantile == 2){"mid"} 
                                          else {"high"})

model4 <- lm(unemprate ~ quant_cat*after + cumcasepc, data = coInc)
summary(model4)


#Aggregate 
coAgg <- colorado  %>% group_by(designation, dates) %>% summarise(tot_unemp = sum(unemp),
                                                                 tot_labforce = sum(laborforce))
coAgg <- coAgg %>% mutate(agg_unemprate = (tot_unemp/tot_labforce) * 100)

# average
coAvg <- colorado  %>% 
  group_by(designation, dates) %>% summarise(avg_unemprate = mean(unemprate))

ggplot(aes(dates, avg_unemprate, color = designation), data = coAvg) + geom_line() + 
  annotate(geom = "vline",
           x = c(ymd("2020-03-23"), ymd("2020-05-27")),
           xintercept = c(ymd("2020-03-23"), ymd("2020-05-27")),
           linetype = c("solid", "dashed")) +
  annotate(geom = "text",
           label = c("Stay-at-home order", "Resturants resume in-person dining"),
           x = c(ymd("2020-03-23"), ymd("2020-05-27")),
           y = c(10, 5),
           angle = 90,
           vjust = -1) +
  labs(title = "Unemployment during COVID-19 in Colorado", 
       y = "Average unemployment rate (%)",
       x = "Date")


## UTAH

utah <- read_csv("MAIN_ut.csv")

#Metro vs non Metro
model1_UT <- lm(unemprate ~ metro_binary*after, data = utah)

summary(model1_UT)

# Urban, suburban, rural
model2_UT <- lm(unemprate ~ designation*after, data = utah)
summary(model2_UT)

#excluding rural counties
utMetro <- utah %>% filter(designation != "rural")
model3_UT <- lm(unemprate ~ designation*after + cumcasepc + laborforce, data = utMetro)
summary(model3_UT)

# income 
# coMetro <- coMetro %>% mutate(quantile = ntile(AGI, 3)) 
# coMetro <- coMetro %>% rowwise() %>% mutate(quant_cat = if(quantile == 1){"low"} 
#                                             else if(quantile == 2){"mid"} 
#                                             else {"high"})
# 
# model4 <- lm(unemprate ~ quant_cat*after + cumcasepc , data = coMetro)
# summary(model4)


#Aggregate 
# utAgg <- utah  %>% group_by(designation, dates) %>% summarise(tot_unemp = sum(unemp),
#                                                                   tot_labforce = sum(laborforce))
# utAgg <- utAgg %>% mutate(agg_unemprate = (tot_unemp/tot_labforce) * 100)

# average
utAvg <- utah  %>% 
  group_by(designation, dates) %>% summarise(avg_unemprate = mean(unemprate))

ggplot(aes(dates, avg_unemprate, color = designation), data = utAvg) + geom_line() + 
  annotate(geom = "vline",
           x = c(ymd("2020-03-23"), ymd("2020-05-27")),
           xintercept = c(ymd("2020-03-27"), ymd("2020-05-27")),
           linetype = c("solid", "dashed")) +
  annotate(geom = "text",
           label = "Voluntary stay-at-home",
           x = ymd("2020-03-23"),
           y = 10,
           angle = 90,
           vjust = -0.5) +
  annotate(geom = "text",
           label = "Loosened COVID restrictions",
           x = ymd("2020-05-1"),
           y = 5,
           angle = 90,
           vjust = 1) + 
  labs(title = "Unemployment during COVID-19 in Utah", 
       y = "Average unemployment rate (%)",
       x = "Date")


## BOTH

both <- full_join(utah, colorado)
both <- both %>% mutate(treatment = ifelse(State == "CO", 1, 0))

#dif in dif
model1_both <- lm(unemprate ~ treatment*after, data = both)

summary(model1_both)

# Urban, suburban, rural
model2_both <- lm(unemprate ~ designation*after, data = both)
summary(model2_both)

#excluding rural counties
bothMetro <- both %>% filter(designation != "rural")
model3_both <- lm(unemprate ~ designation*after + cumcasepc, data = bothMetro)
summary(model3_both)

bothAvg <- both  %>% 
  group_by(designation, dates) %>% summarise(avg_unemprate = mean(unemprate))
ggplot(aes(dates, avg_unemprate, color = designation), data = bothAvg) + geom_line() +
  labs(title = "Unemployment in CO and UT during COVID", 
       y = "Average unemployment rate (%)",
       x = "Date")



