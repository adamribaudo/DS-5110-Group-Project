### School Ratings ###

## library(tidyverse)
## library(readxl)


sch_rate<-read_excel("./data/MA/Accountability.xlsx", skip = 2)%>%
  filter(startsWith(Level, 'Level'))%>%
  mutate(sch_code =`School Code`,
         Level = as.integer(substr(Level, 7, 8)))%>%
  select(-`School Code`, -`X__1`)

parcc_rate<-parcc_tib%>%filter(grade == '3-8', subject == 'math')%>%
  left_join(sch_rate, by = 'sch_code')
 
ggplot(parcc_rate, aes(as.factor(Level), `Average Scaled Score`))+geom_boxplot()

ggplot(parcc_rate, aes(`School Percentile (1-99)`, `Average Scaled Score`))+geom_point(alpha=1/10)+geom_smooth()
