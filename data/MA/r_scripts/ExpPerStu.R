### Per pupil Expenditure ###



pp_exp<-read_excel("./data/MA/PerPupilExpenditures.xlsx", skip = 1)%>%
  mutate(dis_code = substr(`District Code`,1,4),
         tot_exp = as.numeric(gsub("[^[:alnum:]]", "", `Total Expenditures per Pupil`)))%>%
  select(-`District Code`, -`X__1`)

parcc_pp_math<-parcc_tib%>%
  filter(grade=='3-8', subject=='math')%>%
  left_join(pp_exp, by = 'dis_code')

ggplot(parcc_pp_math, aes(tot_exp, `L4+5 %`))+geom_point(alpha = 1/10)+geom_smooth()

