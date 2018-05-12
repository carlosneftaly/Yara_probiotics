
library(tidyverse)


datosC<-read.csv('Datos/2018-05-11 datosC.csv',sep=';')


resC<-datosC%>%group_by(Tiempo, patogeno, Probiotics, Muestra)%>%
  summarise(AbsR=mean(Abs), sdR=sd(Abs))


ggplot(resC, aes(Tiempo, AbsR, group=Probiotics,colour=Probiotics))+
  geom_errorbar(aes(ymin=AbsR-sdR/2, ymax=AbsR+sdR/2) ,colour='black', width=.2)+
  geom_point(shape=21, fill="white")+
  geom_line()+facet_grid(patogeno~Muestra) + theme_bw() 



ggplot(resC, aes(Tiempo, AbsR, group=Probiotics,colour=Probiotics))+
  geom_point(shape=21, fill="white")+
  geom_line()+facet_grid(patogeno~Muestra) + theme_bw() 
  

ggplot(resC, aes(Tiempo, AbsR, group=Probiotics,colour=Probiotics))+
  geom_point(shape=21, fill="white")+
  geom_line()+facet_grid(Muestra~patogeno) + theme_bw() 
