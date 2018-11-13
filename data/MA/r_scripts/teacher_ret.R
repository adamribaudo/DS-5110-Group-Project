#### STAFF RETENTION RATES ####



staff_ret<-read_excel("./data/MA/staffingretention.xlsx", skip = 1)%>%
  mutate(sch_code = `School Code`)%>%select(-`School Code`)



parcc_staff<-left_join(staff_ret,parcc_tib,  by = 'sch_code')
  # have each grade, subject and grades 3-8 for each school

 #### Graphs for all grades 3-8 ####
parcc_staff%>%filter(grade == '3-8', subject == "math")%>%
  ggplot(aes(as.numeric(`Teacher % Retained`), `Average Scaled Score`))+
  geom_point(alpha = 1/10)+geom_smooth(color = 'red')+xlim(50,100)

parcc_staff%>%filter(grade == '3-8', subject=="ela/l")%>%
  ggplot(aes(as.numeric(`Teacher % Retained`), `Average Scaled Score`))+
  geom_point(alpha = 1/10)+geom_smooth(color = 'red')+xlim(50,100)

parcc_staff%>%filter(grade == '3-8', subject=="math")%>%
  ggplot(aes(as.numeric(`Teacher % Retained`), `L4+5 %`))+
  geom_point(alpha = 1/10)+geom_smooth(color = 'red')+xlim(50,100)

parcc_staff%>%filter(grade == '3-8', subject=="ela/l")%>%
  ggplot(aes(as.numeric(`Teacher % Retained`), `L4+5 %`))+
  geom_point(alpha = 1/10)+geom_smooth(color = 'red')+xlim(50,100)

### Graphs faceted by Grades ###
parcc_staff%>%filter(subject=='ela/l', grade != '3-8')%>%
  ggplot(aes(as.numeric(`Teacher % Retained`), `L4+5 %`))+
  geom_point(alpha = 1/10)+geom_smooth(color = 'red')+xlim(50,100)+facet_wrap(~ grade)


