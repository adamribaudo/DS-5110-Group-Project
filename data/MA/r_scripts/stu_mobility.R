### Student Mobility ###

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

stu_mob<-read_excel("./data/MA/mobilityrates.xlsx", skip = 1)%>%
  mutate(sch_code = substr(`School Code`, 5,8))%>%
  select(-`School Code`)

parcc_tib%>%filter(startsWith(`Grade/Subject`, "GRADES 3-8 ELA/L") )%>%
  left_join(stu_mob, by = "sch_code")%>%
  ggplot(aes(as.numeric(`% Intake`), `Average Scaled Score`))+
  geom_point(alpha = 1/100)+geom_smooth()+xlim(0,10)
 