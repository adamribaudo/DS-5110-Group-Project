
#### Finding change in Math Proficiency when other variables change

save(fit1, 'fit1', file="ma_fit.RData")
intercept<-predict(fit1,tribble(~retained, ~abs_ten, ~ooss,0,0,0))


retained_pred <- tibble(proficiency=predict(fit1, data.frame(retained=seq(.00,1,.01), 
                                                             ooss=rep(0,101),
                                                             abs_ten = rep(0,101))) - intercept) %>% 
  bind_cols(data.frame(rate=seq(.00,1,.01))) %>% mutate(variable="MA Teacher Ret.")

ooss_pred <- tibble(proficiency=predict(fit1, data.frame(retained=rep(0,101), 
                                                         ooss=seq(.00,1,.01),
                                                         abs_ten = rep(0,101))) - intercept) %>% 
  bind_cols(data.frame(rate=seq(.00,1,.01))) %>% mutate(variable="MA Suspension")

abs_pred <- tibble(proficiency=predict(fit1, data.frame(retained=rep(0,101), 
                                                        ooss=rep(0,101),
                                                        abs_ten = seq(.00,1,.01))) - intercept) %>% 
  bind_cols(data.frame(rate=seq(.00,1,.01))) %>% mutate(variable="MA Absent >10")


final_pred <- retained_pred %>% union(ooss_pred) %>% union(abs_pred)

ggplot(final_pred) + geom_line(aes(rate,proficiency, color=variable ))
