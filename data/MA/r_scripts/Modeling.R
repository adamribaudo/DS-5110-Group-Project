###### Modeling using    (required files)
    # Teacher Retention  (teacher_ret.R)
    # Out of school suspension (stu_dis.R)
    # Attendence Rate   (stu_attend.R)
    # Absent >=10 days  (stu_attend.R)
library(modelr)

ret_mod<-staff_ret%>%mutate(retained = as.numeric(`Teacher % Retained`) * 0.01)%>%
  select(retained, sch_code)
dis_mod<-stu_dis%>%
  mutate(ooss = ooss*0.01,
         iss = iss*0.01)%>%
  select(sch_code, ooss, iss)
att_mod<-stu_att%>%
  mutate(att_rate = att_rate * 0.01,
         abs_ten = abs_ten * 0.01)%>%
  select(sch_code, att_rate, abs_ten)

model_tib<-parcc_tib%>%mutate(pass_rt = `L4+5 %` *0.01, scaled_sc = `Average Scaled Score`)%>%
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
rmse(fit1, data=part_mod$test)
model_tib%>%add_residuals(fit1)%>%
  ggplot(aes(x=att_rate, y=resid))+geom_point(alpha=1/10)

as.tibble(part_mod$test)%>%add_predictions(fit1)%>%
  ggplot(aes(x=retained))+geom_point(aes(y=pass_rt), alpha=1/5)+
  geom_smooth(aes(y=pred, color='Predicted'), se=F)+
  geom_smooth(aes(y=pass_rt, color = 'Actual'), se = F)+xlim(.6,1)+
  labs(x="Teacher Retention Rate (%)",
       y="PARCC MATH Passing Rate (%)")

as.tibble(part_mod$test)%>%add_predictions(fit1)%>%
  ggplot(aes(x=abs_ten))+geom_point(aes(y=pass_rt), alpha=1/5)+
  geom_smooth(aes(y=pred, color='Predicted'), se=F)+
  geom_smooth(aes(y=pass_rt, color = 'Actual'), se = F)+
  labs(x="Absent >10 Days (%)",
       y="PARCC MATH Passing Rate (%)")

mod_train%>%ggplot(aes(x=att_rate, y=pass_rt))+geom_point(alpha=1/10)+
  geom_smooth(se=F)+xlim(90,100)+
  labs(y="Passing Rate", x="Attendence Rate")

mod_att<-lm(pass_rt ~ abs_ten, data = mod_train)
mod_train%>%add_residuals(mod_att)%>%
  ggplot(aes(y=resid, x=att_rate))+
  geom_point(alpha=1/10)+geom_smooth()+xlim(90,100)+
  labs(y="Residuals from Passing Rate VS >10 Absent days", x="Attendence Rate")


ggplot(mod_train, aes(x=retained))+geom_histogram(bins=40)+xlim(.5,1)
## Teacher sal and pp_exp 