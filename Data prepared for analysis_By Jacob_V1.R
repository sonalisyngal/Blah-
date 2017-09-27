library(dplyr)
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

print('percy')


