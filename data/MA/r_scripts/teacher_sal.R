
teach_sal<-read_excel('./data/MA/teacher/TeacherSalaries.xlsx', skip = 1)%>%
  mutate(dis_code = substr(`District Code`, 1,4))%>%select(-`District Code`)

# the combined grades and subject from school-level data on PARCC (prelim_data.R)



parcc_sal<-parcc_tib%>%left_join(teach_sal, by = "dis_code")%>%filter(!is.na(`Average Salary`))%>%
  mutate(avg_sal = as.numeric(gsub("[^[:alnum:]]", "", `Average Salary`)))%>%select(-`Average Salary`)
ggplot(parcc_sal, aes(avg_sal, `L4+5 %`))+geom_point(alpha = 1/15)+geom_smooth(color='red')+labs(x="Teacher Salary (by district)", y="Passing Percent (by school)", title="Salary by School Passing Percentage")
ggplot(parcc_sal, aes(avg_sal, `Average Scaled Score`))+geom_point(alpha = 1/15)+geom_smooth(color='red')+
  labs(x="Teacher Salary (by district)", y="Scaled Scores (by school)", title="Salary by Scaled Score")
