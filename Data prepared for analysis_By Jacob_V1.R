library(dplyr)
library(ggplot2)

df <- read.csv("database.csv", header = TRUE)


names_to_drop <- c('Agency.Code', 'Record.ID', 'Agency.Name', 'Record.Source', 
                   'Perpetrator.Ethnicity')
  

murder.num <- df %>%
  filter(Crime.Type == "Murder or Manslaughter") %>%
  select(-one_of(names_to_drop)) %>%
  filter(Victim.Age > 0 & Victim.Age < 99) %>%
  filter(!(Perpetrator.Race == 'Unknown' & Crime.Solved == 'Yes')) %>%
  mutate(Victim.Counts = Victim.Count + 1, Perpetrator.Counts = Perpetrator.Count +1) %>%
  select(-one_of('Victim.Count', 'Perpetrator.Count'))

summary(murder.num)
names(murder.num)
head(murder.num)

cali.cities <- murder.num %>% 
  group_by(State, City) %>%
  filter(State == 'California') %>%
  summarise(n = n()) %>%
  filter( n > 32)

cali.df <- murder.num %>% 
  group_by(State, City) %>%
  filter(State == 'California') %>%
  ungroup() %>%
  group_by(City) %>%
  mutate(n = n()) %>%
  filter(n>32) %>%
  ungroup()

head(cali.df)  


num.cities.approved <- n_distinct(cali.cities$City) #Equals to 51 cities in california with 32+ crimes
cali.cities


solved.vs.number <- glm(Crime.Solved ~ n, family = binomial(link='logit'),data=cali.df)

glimpse(cali.df)

solve.num <- cali.df %>%
  group_by(City, Crime.Solved) %>%
  summarise(count = n()) %>%
  filter(Crime.Solved == 'Yes') %>%
  ungroup() %>%
  select(one_of('count'))

crime.num <- cali.df %>%
  select(one_of('City')) %>%
  group_by(City)  %>%
  summarise(count = n()) 

ratio.solved <- (solve.num / crime.num$count)

cities.with.ratios <- data.frame(crime.num$City, ratio.solved)
cities.with.ratios <- arrange(cities.with.ratios, cities.with.ratios$count)

head(cities.with.ratios, 5)

tail(cities.with.ratios, 5)



