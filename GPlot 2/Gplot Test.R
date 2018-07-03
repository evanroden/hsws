head(Cars93)
Cars93 %>%
  filter(MPG.city<36)%>%
  filter(Cylinders!="rotary")%>%
  ggplot(aes(x=Price,y=log(MPG.city,base=exp(.1)),col=Cylinders,size=Horsepower))+
  geom_point(alpha=.6)