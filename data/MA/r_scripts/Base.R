##    Creates basic Parcc scores tibble
###           Not a tidy data frame because Grade/Subject is repeated to include all permutations of Grade/Subject


parcc_tot<-map(list.files(path = './data/MA/PARCC/overall/', pattern = "*"), 
               ~ read_excel(paste0('./data/MA/PARCC/overall/', .), skip = 7, na = 'N/A'))

parcc_tot<-map(parcc_tot, ~ filter(.,!is.na(`Grade/Subject`))%>%select(-`SGP #`, -`Trans. SGP Median`))

parcc_tib<-parcc_tot[[1]]
for(i in 2:length(parcc_tot)){
  parcc_tib<-union(parcc_tib, parcc_tot[[i]])
}
rx<-"[\QGRADE \E|\QGRADES \E]"
parcc_tib<-parcc_tib%>%mutate(sch_code = `Org Code`,
                              dis_code = substr(`Org Code`, 1,4))%>%
  mutate(`Grade/Subject` = str_replace(`Grade/Subject`, "GRADES ", ''))%>%
  mutate(`Grade/Subject` = str_replace(`Grade/Subject`, "GRADE ", ''))%>%
  separate(`Grade/Subject`, c('grade', 'subject'), sep = " ")%>%
  mutate(subject = tolower(subject))


