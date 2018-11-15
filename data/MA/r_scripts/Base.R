##    Creates basic Parcc scores tibble
###           Not a tidy data frame because Grade/Subject is repeated to include all permutations of Grade/Subject


parcc_tot<-map(list.files(path = './data/MA/PARCC/overall/', pattern = "*"), 
               ~ read_excel(paste0('./data/MA/PARCC/overall/', .), skip = 7, na = 'N/A'))

parcc_tot<-map(parcc_tot, ~ filter(.,!is.na(`Grade/Subject`))%>%select(-`SGP #`, -`Trans. SGP Median`))

parcc_tib<-parcc_tot[[1]]
for(i in 2:length(parcc_tot)){
  parcc_tib<-union(parcc_tib, parcc_tot[[i]])
}

parcc_tib<-parcc_tib%>%mutate(sch_code = `Org Code`,
                              dis_code = substr(`Org Code`, 1,4))%>%
  mutate(`Grade/Subject` = str_replace(`Grade/Subject`, "GRADES ", ''))%>%
  mutate(`Grade/Subject` = str_replace(`Grade/Subject`, "GRADE ", ''))%>%
  separate(`Grade/Subject`, c('grade', 'subject'), sep = " ")%>%
  mutate(subject = tolower(subject))

#rm(parcc_tot)
#rm(i)
###################### Total students and overall PARCC Passing 
enr<-read_excel("./data/MA/District-Grade.xlsx",na="N/A")%>%filter(Gr.4 != '***')
sum(enr$District_Total, na.rm=T) #total student enrollment

#student enrollment grade 3-8
enr%>%summarise(el_gr = sum(as.numeric(Gr.3), na.rm = T)+sum(as.numeric(Gr.4), na.rm = T)+
                  sum(as.numeric(Gr.5), na.rm = T)+sum(as.numeric(Gr.6), na.rm = T)+
                  sum(as.numeric(Gr.7), na.rm = T)+sum(as.numeric(Gr.8), na.rm = T))
  
#math
district<-read_excel("./data/MA/parcc.xls", skip = 7, na = "N/A")
sum(district$`Stud. Incl. #`, na.rm = T)
district%>%summarise(testers = sum(`Stud. Incl. #`, na.rm = T),
                     l4 = sum(`L4 #`, na.rm = T),
                     l5 = sum(`L5 #`, na.rm =T))%>%
  mutate(passing = (l4+l5)/testers)

#ela
dis_ela<-read_excel("./data/MA/parcc_ela.xls", skip = 7, na = "N/A")
sum(dis_ela$`Stud. Incl. #`, na.rm = T)
dis_ela%>%summarise(testers = sum(`Stud. Incl. #`, na.rm = T),
                     l4 = sum(`L4 #`, na.rm = T),
                     l5 = sum(`L5 #`, na.rm =T))%>%
  mutate(passing = (l4+l5)/testers)
