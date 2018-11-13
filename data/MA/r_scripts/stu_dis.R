### STUDENT DISCIPLINE RESULTS ###


stu_dis<-read_excel("./data/MA/ssdr.xlsx", skip = 1)%>%
  mutate(sch_code = `School Code`,
         ooss = ifelse(is.na(`% Out-of-School Suspension`), 0, `% Out-of-School Suspension`),
         ooss = as.numeric(ooss),
         iss = ifelse(is.na(`% In-School Suspension`), 0, `% In-School Suspension`),
         iss = as.numeric(iss))
parcc_dis<-stu_dis%>%left_join(parcc_tib, by = 'sch_code')

parcc_dis%>%filter(subject == 'ela/l', grade == '3-8',ooss > 0)%>%
  ggplot(aes(x=ooss, y=`L4+5 %`))+geom_point(alpha=1/10)+geom_smooth()+xlim(0,25)



