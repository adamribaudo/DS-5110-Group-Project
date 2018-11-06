
## MA PARCC DATA ##
#grades 3-8 together
#parcc_ma_ela<-read_excel('./data/MA/PARCC/ELA.xls', skip = 7)
#parcc_ma_math<-read_excel('./data/MA/PARCC/MATH.xls', skip = 7)
#parcc_ma<-union(parcc_ma_ela, parcc_ma_math)

ma<-list()

#use list.files() function in future
ma$ela3<-read_excel('./data/MA/PARCC/3ELA.xls', skip = 7, na = 'N/A')
ma$math3<-read_excel('./data/MA/PARCC/3MATH.xls', skip = 7,na = 'N/A')
ma$ela4<-read_excel('./data/MA/PARCC/4ELA.xls', skip = 7, na = 'N/A')
ma$math4<-read_excel('./data/MA/PARCC/4MATH.xls', skip = 7, na = 'N/A')
ma$ela5<-read_excel('./data/MA/PARCC/5ELA.xls', skip = 7, na = 'N/A')
ma$math5<-read_excel('./data/MA/PARCC/5MATH.xls', skip = 7, na = 'N/A')
ma$ela6<-read_excel('./data/MA/PARCC/6ELA.xls', skip = 7, na = 'N/A')
ma$math6<-read_excel('./data/MA/PARCC/6MATH.xls', skip = 7, na = 'N/A')
ma$ela7<-read_excel('./data/MA/PARCC/7ELA.xls', skip = 7, na = 'N/A')
ma$math7<-read_excel('./data/MA/PARCC/7MATH.xls', skip = 7, na = 'N/A')
ma$ela8<-read_excel('./data/MA/PARCC/8ELA.xls', skip = 7, na = 'N/A')
ma$math8<-read_excel('./data/MA/PARCC/8MATH.xls', skip = 7, na = 'N/A')
ma$alg8<-read_excel('./data/MA/PARCC/8ALG.xls', skip = 7, na = 'N/A')

ma<-map(ma, ~ filter(.,!is.na(`Grade/Subject`))%>%
          separate(`Grade/Subject`, c('Grade', "Subject"), sep = 7)%>%
          mutate(`SGP #` = as.numeric(`SGP #`),`Trans. SGP Median` = as.numeric(`Trans. SGP Median`)))


ma_tib<-ma[[1]]
for(i in 2:length(ma)){
  ma_tib<-union(ma_tib, ma[[i]])
}
ma_tib<-arrange(ma_tib, `Org Name`, Grade)

ma_math<-ma_tib%>%group_by(Grade)%>%filter(Subject == ' Math')%>%
  summarise(avg_math = mean(`Average Scaled Score`))
ma_ela<-ma_tib%>%group_by(Grade)%>%filter(Subject == ' ELA/L')%>%
  summarise(avg_ela = mean(`Average Scaled Score`))
ggplot(full_join(ma_math, ma_ela, by = 'Grade'), aes(x=avg_ela))+
  geom_point(aes(y=avg_math, color = Grade))+
  labs(title="                      MA avg. PARCC scores", x="ELA", y="MATH")

ggplot(full_join(ma_math, ma_ela, by = 'Grade'))+
  geom_point(aes(x=Grade, y=avg_math, color="Math"))+geom_point(aes(x=Grade, y=avg_ela, color='ELA'))+
  labs(title="Scaled score by Grade", y="Scaled Score")
