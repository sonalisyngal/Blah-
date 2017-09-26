library(dplyr)

setwd('C:/Users/Nontharatt/Desktop/Coursework/Study Modules/Maths and Statistics foundations for Analytics/Group Project/Data/homicide-reports')
homicide <- read.csv('database.csv')
names(homicide) <- tolower(names(homicide))

# Distinct Agency Info
homicide.agency.distinct <- unique(homicide[,c('agency.code','agency.name')])
homicide.agency.distinct.check <- homicide.agency.distinct %>% group_by(Agency.Code) %>% count() %>% filter(n > 1)
head(homicide.agency.distinct.check)


homicide.cnt_by_month <- homicide[,c('year','month')] %>% group_by(year,month) %>% count() %>% as.data.frame()
homicide.cnt_by_month_year <- homicide.cnt_by_month %>% group_by(year) %>% mutate(summ=mean(n)) %>% as.data.frame()
head(homicide.cnt_by_month_year,20)
homicide.cnt_by_month_year$index <- homicide.cnt_by_month_year$n / homicide.cnt_by_month_year$summ
month_map <- data.frame(month = c('January','February','March','April','May','June','July','August','September','October','November','December'), month_id = c(1,2,3,4,5,6,7,8,9,10,11,12))
homicide.cnt_by_month_year <- merge(homicide.cnt_by_month_year, month_map, by = "month")
head(homicide.cnt_by_month_year)

library(ggplot2)
# Seasonality index by month
ggplot(homicide.cnt_by_month_year,aes(x=month_id,y=index,col=as.factor(year))) + geom_line() + ylim(0,2)

# Assumption 
ggplot(homicide,aes(x=perpetrator.age,y=victim.count)) + geom_point()


# Distinct combination of incidents
homicide.distinct <- homicide %>% distinct(agency.name,agency.type,city,state,year,month,incident)

# Add Unique ID for each distinct combination
homicide.distinct <- mutate(homicide.distinct, id = c(1:nrow(homicide.distinct)))

# Merge ID into the main data file
homicide.merged <- merge(homicide, homicide.distinct, by = c('agency.name','agency.type','city','state','year','month','incident')) %>% as.data.frame()
head(homicide.merged,10)

# Count distinct perpetrator by each ID
perpetrator.by.id <- homicide.merged %>% group_by(id) %>% summarise(n_dist = n_distinct(perpetrator.age,perpetrator.sex,perpetrator.race,perpetrator.ethnicity,perpetrator.count))
head(perpetrator.by.id, 10)

# Sample of record with same incident but containing multiple perpetrators
filter(homicide.merged, id == 66547)

# Summary
summary <- perpetrator.by.id %>% group_by(n_dist) %>% summarise(n=n(), countd=n_distinct(id))