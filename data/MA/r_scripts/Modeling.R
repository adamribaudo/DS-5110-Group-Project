###### Modeling using    (required files)
    # Teacher Retention  (teacher_ret.R)
    # Out of school suspension (stu_dis.R)
    # Attendence Rate   (stu_attend.R)
    # Absent >=10 days  (stu_attend.R)
library(modelr)

ret_mod<-staff_ret%>%mutate(retained = `Teacher % Retained`)%>%
  select(retained, sch_code)
dis_mod<-stu_dis%>%select(sch_code, ooss, iss)
att_mod<-stu_att%>%select(sch_code, att_rate, abs_ten)

model_tib<-parcc_tib%>%mutate(pass_rt = `L4+5 %`, scaled_sc = `Average Scaled Score`)%>%
  select(pass_rt, scaled_sc, sch_code)%>%
  left_join(ret_mod, by = 'sch_code')%>%
  left_join(dis_mod, by = 'sch_code')%>%
  left_join(att_mod, by = 'sch_code')%>%
  mutate(retained = as.numeric(retained))

####### Fits 
set.seed(1)
part_mod<-resample_partition(model_tib, c(train = 0.8, 
                                          valid = 0.1,
                                          test = 0.1))
mod_train<-as.tibble(part_mod$train)
fit1<-lm(pass_rt ~ retained + ooss + abs_ten, data = mod_train)
rmse(fit1, data=part_mod$valid)
rmse(fit1, data=mod_train)
model_tib%>%add_residuals(fit1)%>%
  ggplot(aes(x=att_rate, y=resid))+geom_point(alpha=1/10)

model_tib%>%add_predictions(fit1)%>%
  ggplot(aes(x=ooss))+geom_point(aes(y=pass_rt), alpha=1/10, color='blue')+geom_point(aes(y=pred), color='yellow', alpha=1/10)



