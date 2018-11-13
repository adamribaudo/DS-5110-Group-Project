###### STUDENT ATTENDANCE RATES ######

stu_att<-read_excel('./data/MA/StudentIndicators.xlsx', skip = 1)%>%
  mutate(sch_code = `School Code`,
         att_rate = as.numeric(`Attendance Rate`),
         abs_ten = as.numeric(`Absent 10 or more days`))

att_parcc<-stu_att%>%left_join(parcc_tib, by = "sch_code")

att_parcc%>%filter(grade=='3-8', subject=='ela/l')%>%
  ggplot(aes(x=att_rate, y=`L4+5 %`))+geom_point(alpha=1/10)+geom_smooth()+xlim(90,99)

