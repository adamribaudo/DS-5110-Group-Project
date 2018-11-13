### School Ratings ###

## library(tidyverse)
## library(readxl)

parcc_tot<-map(list.files(path = './data/MA/PARCC/overall/', pattern = "*"), 
               ~ read_excel(paste0('./data/MA/PARCC/overall/', .), skip = 7, na = 'N/A'))

parcc_tot<-map(parcc_tot, ~ filter(.,!is.na(`Grade/Subject`))%>%select(-`SGP #`, -`Trans. SGP Median`))

parcc_tib<-parcc_tot[[1]]
for(i in 2:length(parcc_tot)){
  parcc_tib<-union(parcc_tib, parcc_tot[[i]])
}
parcc_tib<-parcc_tib%>%separate(`Org Code`, c('dis_code', 'sch_code'), sep = 4)

sch_rate<-read_excel("./data/MA/Accountability.xlsx", skip = 2)%>%
  filter(startsWith(Level, 'Level'))%>%
  mutate(sch_code = substr(`School Code`, 5,8),
         Level = as.integer(substr(Level, 7, 8)))%>%
  select(-`School Code`, -`X__1`)

parcc_rate<-parcc_tib%>%filter(startsWith(`Grade/Subject`, "GRADES 3-8 MATH"))%>%
  left_join(sch_rate, by = 'sch_code')
 
ggplot(parcc_rate, aes(as.factor(Level), `Average Scaled Score`))+geom_boxplot()

ggplot(parcc_rate, aes(`School Percentile (1-99)`, `Average Scaled Score`))+geom_point(alpha=1/10)+geom_smooth()
