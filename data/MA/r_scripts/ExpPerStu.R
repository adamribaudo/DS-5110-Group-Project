### Per pupil Expenditure ###

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
  

pp_exp<-read_excel("./data/MA/PerPupilExpenditures.xlsx", skip = 1)%>%
  mutate(dis_code = substr(`District Code`,1,4),
         tot_exp = as.numeric(gsub("[^[:alnum:]]", "", `Total Expenditures per Pupil`)))%>%
  select(-`District Code`, -`X__1`)

parcc_pp_ela<-parcc_tib%>%
  filter(`Grade/Subject` == "GRADES 3-8 ELA/L")%>%
  left_join(pp_exp, by = 'dis_code')

parcc_pp_math<-parcc_tib%>%
  filter(`Grade/Subject` == "GRADES 3-8 MATH")%>%
  left_join(pp_exp, by = 'dis_code')

ggplot(parcc_pp_ela, aes(tot_exp, `L4+5 %`))+geom_point(alpha = 1/10)+geom_smooth()

parcc_pp<-parcc_tib%>%
  filter(startsWith(`Grade/Subject`,"GRADES 3-8"))%>%
  left_join(pp_exp, by = 'dis_code')
ggplot(parcc_pp, aes(tot_exp, `Average Scaled Score`))+geom_point(alpha=1/10)+geom_smooth()
