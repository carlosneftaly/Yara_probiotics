
library(tidyverse)


datosC<-read.csv('Datos/2018-05-11 datosC.csv',sep=';')


resC<-datosC%>%group_by(Tiempo, patogeno, Probiotics, Muestra)%>%
  summarise(AbsR=mean(Abs), sdR=sd(Abs))


ggplot(resC, aes(Tiempo, AbsR, group=Probiotics,colour=Probiotics))+
  geom_errorbar(aes(ymin=AbsR-sdR/2, ymax=AbsR+sdR/2) ,colour='black', width=.2)+
  geom_point(shape=21, fill="white")+
  geom_line()+facet_grid(patogeno~Muestra) + theme_bw() 



ggplot(resC, aes(Tiempo, AbsR,colour=Probiotics))+
  geom_point(shape=21, alpha=0.2)+
  geom_line()+facet_grid(patogeno~Muestra) + theme_bw() 
  

ggplot(resC, aes(Tiempo, AbsR, group=Probiotics,colour=Probiotics))+
  geom_point(shape=21, fill="white")+
  geom_line()+facet_grid(Muestra~patogeno) + theme_bw() 




ggplot(resC, aes(Tiempo, AbsR, colour=Probiotics))+
  geom_point(shape=21, alpha=.2) + geom_smooth() + facet_wrap(patogeno~Muestra)



################# Inferencia E. Coliiii###############
    
    ## E. coli - Heces
     
      
      dat23<- filter(datosC, Tiempo==23 & patogeno=='E.coli' & Muestra=='Heces')
      
      ##### 
      
      #write.csv(dat23, 'coliHeces.csv')
        datColi<-read.csv('coliHeces.csv',header = T)
        attach(datColi)
      
      
      lapply(as.character(levels(datColi$Probiotics)[-1]), 
             FUN = function(y){t.test(Abs ~ Probiotics,datColi[Probiotics %in% c(y,"Control"),])}
      )
      
      
      
      ## E. coli - Leche
      
      
      dat23.1<- filter(datosC, Tiempo==23 & patogeno=='E.coli' & Muestra=='Leche')
      
      ##### 
      
      write.csv(dat23.1, 'coliLeche.csv')
        datColiL<-read.csv('coliLeche.csv',header = T)
      attach(datColiL)
      
      
      cc<-lapply(as.character(levels(datColiL$Probiotics)[-1]), 
             FUN = function(y){t.test(Abs ~ Probiotics,datColiL[Probiotics %in% c(y,"Control"),])}
      )
      
      
      xx<-do.call(c,lapply(cc,function(x) round(x$p.value,4)) )
      
      xx<-do.call(c,lapply(cc,function(x) x$estimate) )
      
      ################# Inferencia Salmonella###############
      
      ## Salmonella - Heces
      
      
      dat23s<- filter(datosC, Tiempo==23 & patogeno=='Salmonella' & Muestra=='Heces')
      
      ##### 
      
      #write.csv(dat23s, 'SalmoHeces.csv')
      datSalmo<-read.csv('SalmoHeces.csv',header = T)
      attach(datSalmo)
      
      
      lapply(as.character(levels(datSalmo$Probiotics)[-1]), 
             FUN = function(y){t.test(Abs ~ Probiotics,datSalmo[Probiotics %in% c(y,"Control"),])}
      )
      
      
      
      ## Salmo - Leche
      
      
      dat23s.1<- filter(datosC, Tiempo==23 & patogeno=='Salmonella' & Muestra=='Leche')
      
      ##### 
      
      #write.csv(dat23s.1, 'salmoLeche.csv')
      datSalmoL<-read.csv('salmoLeche.csv',header = T)
      attach(datSalmoL)
      
      
      lapply(as.character(levels(datSalmoL$Probiotics)[-1]), 
             FUN = function(y){t.test(Abs ~ Probiotics,datSalmoL[Probiotics %in% c(y,"Control"),])}
      )
      
      
      
      ################# Inferencia H.influenzae###############
      
      ## Salmonella - Heces
      
      
      dat23Hi<- filter(datosC, Tiempo==23 & patogeno=='H.influenzae' & Muestra=='Heces')
      
      ##### 
      
      #write.csv(dat23Hi, 'HiHeces.csv')
      datHiH<-read.csv('HiHeces.csv',header = T)
      attach(datHiH)
      
      
      lapply(as.character(levels(datHiH$Probiotics)[-1]), 
             FUN = function(y){t.test(Abs ~ Probiotics,datHiH[Probiotics %in% c(y,"Control"),])}
      )
      
      
      
      ## Hinhfluenza - Leche
      
      
      dat23H.1<- filter(datosC, Tiempo==23 & patogeno=='H.influenzae' & Muestra=='Leche')
      
      ##### 
      
      #write.csv(dat23H.1, 'HiHLeche.csv')
      datHiHL<-read.csv('HiHLeche.csv',header = T)
      attach(datHiHL)
      
      
      lapply(as.character(levels(datHiHL$Probiotics)[-1]), 
             FUN = function(y){t.test(Abs ~ Probiotics,datHiHL[Probiotics %in% c(y,"Control"),])}
      )
      
      
      ################# Inferencia S.pneumoniae###############
      
      ## S.pneumoniae  - Heces
      
      
      dat23sp<- filter(datosC, Tiempo==23 & patogeno=='S.pneumoniae' & Muestra=='Heces')
      
      ##### 
      
      #write.csv(dat23sp, 'spHeces.csv')
      datspH<-read.csv('spHeces.csv',header = T)
      attach(datspH)
      
      
      lapply(as.character(levels(datspH$Probiotics)[-1]), 
             FUN = function(y){t.test(Abs ~ Probiotics,datspH[Probiotics %in% c(y,"Control"),])}
      )
      
      
      
      ## S.pneumoniae - Leche
      
      
      datsp1<- filter(datosC, Tiempo==23 & patogeno=='S.pneumoniae' & Muestra=='Leche')
      
      ##### 
      
      #write.csv(datsp1, 'spLeche.csv')
      datspL<-read.csv('spLeche.csv',header = T)
      attach(datspL)
      
      
      lapply(as.character(levels(datspL$Probiotics)[-1]), 
             FUN = function(y){t.test(Abs ~ Probiotics,datspL[Probiotics %in% c(y,"Control"),])}
      )
      
      
      
      
      
      
      
      
      
      
