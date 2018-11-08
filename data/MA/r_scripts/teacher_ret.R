## library(tidyverse)
## library(readxl)

parcc_tot<-map(list.files(path = './data/MA/PARCC/overall/', pattern = "*"), 
               ~ read_excel(paste0('./data/MA/PARCC/overall/', .), skip = 7, na = 'N/A'))

parcc_tot<-map(parcc_tot, ~ filter(.,!is.na(`Grade/Subject`))%>%select(-`SGP #`, -`Trans. SGP Median`))

parcc_tib<-parcc_tot[[1]]
for(i in 2:length(parcc_tot)){
  parcc_tib<-union(parcc_tib, parcc_tot[[i]])
}
parcc_tib<-parcc_tib%>%mutate(sch_code = `Org Code`)%>%select(-`Org Code`)

staff_ret<-read_excel("./data/MA/staffingretention.xlsx", skip = 1)%>%mutate(sch_code = `School Code`)%>%select(-`School Code`)

parcc_staff<-left_join(parcc_tib, staff_ret, by = 'sch_code')
  # have each grade, subject and grades 3-8 for each school


ggplot(parcc_staff, aes(as.numeric(`Teacher % Retained`), `Average Scaled Score`))+
  geom_point(alpha = 1/10)+geom_smooth(color = 'red')+xlim(50,100)

ggplot(parcc_staff, aes(as.numeric(`Teacher % Retained`), `L4+5 %`))+
  geom_point(alpha = 1/10)+geom_smooth(color = 'red')+xlim(50,100)+
  labs(x="Percent of Teachers Retained", y="Percent of Students who passed")
