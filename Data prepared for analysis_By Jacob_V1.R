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

#############################################################################
crime.solved.year <- murder.num %>%
  mutate(solved.bin = ifelse(murder.num$Crime.Solved == 'Yes', 1, 0)) %>%
  group_by(Year, City) %>%
  summarise(n = n(), solved.bin.n = sum(solved.bin)) %>%
  mutate(perc = solved.bin.n/n) %>%
  filter(City == c('Los Angeles', 'New York'))

ggplot(crime.solved.year) + geom_point(aes(x = Year, y = perc, colour = City)) + 
  geom_line(aes(x = Year, y = perc, colour = City))

for.summary.stats <- murder.num %>% 
  filter(City == c('Los Angeles', 'New York'))

summary(for.summary.stats)

number.crimes <- murder.num %>%
  filter(City == c('Los Angeles', 'New York')) %>%
  group_by(Year, City) %>%
  summarise(n = n())

ggplot(number.crimes) + geom_point(aes(x = Year, y = n, colour = City)) + 
  geom_line(aes(x = Year, y = n, colour = City))

number.agencies <- murder.num %>%
  filter(City == c('Los Angeles', 'New York')) %>%
  filter(Year == 2014) %>%
  group_by(City) %>%
  summarise(n.agencies = n())

number.agencies
  

#############################################################################

los.angeles.df <- murder.num %>%            #Filter to get data for my city
  filter(City == "Los Angeles") %>%
  select(-one_of(c("State", "City")))       #Unnecesary, once you know which city you are working with

names(los.angeles.df)                       #Self-Explanatory


lm1 <- lm(Victim.Age ~ Victim.Sex + Perpetrator.Sex, los.angeles.df)       
summary(lm1)                                #Lin-Reg between V.Age ~ V.Sex + P.sex

ggplot(los.angeles.df) + geom_boxplot(aes(x=Victim.Sex, y = Victim.Age, fill=Victim.Sex)) + 
  facet_wrap(~Perpetrator.Sex)              #Boxplot*3 Sex vs Age w/ breakdown by Perp.Sex

ggplot(los.angeles.df) + geom_density(aes(x= Victim.Age, colour= Victim.Sex)) + 
  theme(legend.title = element_blank())     #Variable Dist of Victim Age by sex

ggplot(los.angeles.df) + geom_bar(aes(x=Victim.Sex, fill=Perpetrator.Sex), position = position_dodge())
ggplot(los.angeles.df) + geom_bar(aes(x=Perpetrator.Sex, fill=Victim.Sex), position = position_dodge())
                                            #Same thing but in reverse of who's killing who

sex.ratio <- los.angeles.df %>%             #Creating a proportion column for who's killing who
  group_by(Victim.Sex) %>%
  mutate(vic.sex.count=n()) %>%
  ungroup() %>%
  group_by(Victim.Sex, Perpetrator.Sex) %>%
  mutate(Perp.sex.count=n()) %>%
  ungroup() %>%
  group_by(Victim.Sex, Perpetrator.Sex) %>%
  summarise(vic.sex.spec = mean(Perp.sex.count), vic.sex.total = mean(vic.sex.count)) %>%
  mutate(Proportion = round(((vic.sex.spec/vic.sex.total)*100),2)) %>%
  select(one_of(c('Victim.Sex', 'Perpetrator.Sex', 'ratio')))

ggplot(sex.ratio) + geom_point(aes(x = Victim.Sex, y = ratio, colour = Perpetrator.Sex))
                                            #Who's killing who, but this time by proportion 

lm2 <- lm(Victim.Age ~ Weapon, los.angeles.df)
summary(lm2)                                #Linear regression - self explanatory

ggplot(los.angeles.df) + geom_boxplot(aes(x=Weapon, y = Victim.Age, fill=Victim.Sex))
                                            #Boxplot, weapon by Victim age broken down by gender

ggplot(los.angeles.df) + geom_bar(aes(x=Weapon, fill=Month), position = position_dodge()) + facet_wrap(~Month)
                                            #Same thing, facet wrapped by month
                                            
#Vic.Age vs. Perp.Age
Age.Cleaned <- filter(los.angeles.df, !(Perpetrator.Age==0))

#t.test for relating Victim age and Perpetrator Age
t.test(Age.Cleaned$Victim.Age, Age.Cleaned$Perpetrator.Age) 

#cor.test
cor.test(Age.Cleaned$Victim.Age, Age.Cleaned$Perpetrator.Age)

