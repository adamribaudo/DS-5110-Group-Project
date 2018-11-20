fit3 <- lm(data=dc_train, sqrt(math_proficiency) ~ Safe_Truancy_Current_Year + suspensions)
summary(fit3)
modelr::rmse(data=dc,fit3)
dc_train <- dc_train %>% select(math_proficiency, Safe_Truancy_Current_Year, suspensions) %>% na.omit()
dc_test <- dc_test %>% select(math_proficiency, Safe_Truancy_Current_Year, suspensions) %>% na.omit()

#Create vector of formulas with increasing dimensionality of our 2 variables
forms <- lapply(1:20, 
                function(x)as.formula(
                  paste("sqrt(math_proficiency) ~ poly(Safe_Truancy_Current_Year, raw=T, degree=", x, 
                        ") + poly(suspensions, raw=T, degree=",x,")")))

models_train <- lapply(forms, function(x)lm(data=dc_train, x))
models_test <- lapply(forms, function(x)lm(data=dc_test, x))
rmse_train <- lapply(models_train, function(x)modelr::rmse(x, data=dc_train))
rmse_test <- lapply(models_test, function(x)modelr::rmse(x, data=dc_test))

points <- tibble(Data=rep("Train", length(models_train)), Error = unlist(rmse_train), Dimensions = 1:20) %>%
  union(tibble(Data=rep("Test", length(models_test)), Error = unlist(rmse_test), Dimensions = 1:20))

ggplot(points) + geom_line(aes(Dimensions, Error, color=Data))
