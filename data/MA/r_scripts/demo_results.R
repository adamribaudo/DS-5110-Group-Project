library(tidyverse)
library(readxl)
demo_name<-c("asian", "black", "disabled", "econ_dis",
             "ell", "female", "hispanic", "high_needs", "male", 
             "multiracial", "non_econ_dis", "white")

#### ELA Passing Percentage  --  by Demographic ####

setwd('~/Desktop/DS-5110-Group-Project/data/MA/PARCC/by_demo/ela/')
demo_list<-map(list.files(path = '.', pattern = "*"), ~ read_excel(., skip = 7))

names(demo_list)<-demo_name

# Summarizing level 4+5 (passing) percentage by each demographic across all schools
test<-map(demo_list, ~ summarise(.,
                              tot_pass = sum(`L4+5 #`, na.rm=T),
                              tot_inc = sum(`Stud. Incl. #`, na.rm =T),
                              pass_per = tot_pass/tot_inc))
    # add demographic name to each tibble in list
for(i in 1:length(test)){
  test[[i]]<-test[[i]]%>%mutate(demo = demo_name[i])
}
  # combine all tibbles into one
sum_demo<-as.tibble(test[[1]])
for(i in 2:length(test)){
  sum_demo<-union(sum_demo, as.tibble(test[[i]]))
}

  # order by highest passing percentage then graph results
sum_demo%>%mutate(demo = reorder(demo, pass_per))%>%ggplot(aes(x=demo, y=pass_per))+
  geom_col()+coord_flip()+labs(y="Percent of Students Passing", title="ELA Passing Percentage by Demographic", x="")

#### MATH Passing Percentage  --  by Demographic####
  #(same steps as above)

setwd('~/Desktop/DS-5110-Group-Project/data/MA/PARCC/by_demo/math/')
demo_math<-map(list.files(path = '.', pattern = "*"), ~ read_excel(., skip = 7))

test2<-map(demo_math, ~ summarise(.,
                                 tot_pass = sum(`L4+5 #`, na.rm=T),
                                 tot_inc = sum(`Stud. Incl. #`, na.rm =T),
                                 pass_per = tot_pass/tot_inc))
for(i in 1:length(test2)){
  test2[[i]]<-test2[[i]]%>%mutate(demo = demo_name[i])
}
sum_math<-as.tibble(test2[[1]])
for(i in 2:length(test2)){
  sum_math<-union(sum_math, as.tibble(test2[[i]]))
}

sum_math%>%mutate(demo = reorder(demo, pass_per))%>%ggplot(aes(x=demo, y=pass_per))+
  geom_col()+coord_flip()+labs(x='', y="Percent of Students Passing", title="Math Passing Percent by Demographics")

