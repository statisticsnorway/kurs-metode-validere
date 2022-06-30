############################ Oppgaver valideing ###################################
# Oppgave 1. Last inn validate pakken og datasettet kirkedata.R
library(validate)
load("kirkedata.RData")
head(kirkedata)

#Oppgave 2. Sett opp noen kontroller for antall døpte

regler<-validator( dopte>0,
                   dopte<=fodde)


#Oppgave 3 Kjør kontrollene på datasettet kirkedata

resultat<-confront(kirkedata,regler,key="region")

#Oppgave 4. Analyser kontrollene

summary(resultat)
aggregate(resultat)
plot(resultat)

# Ekstra oppgave. Last kommunelista ned fra Klass og kontroller at det er gyldige kommuner
library(klassR)
sn <- GetKlass(klass = 131, date = "2019-01-01") 
komliste<-as.vector(sn[,c("code")])
regler<-validator( region %in% komliste)
resultat<-confront(kirkedata,regler,key="region")
summary(resultat)

############################ Oppgaver kontrollere ###################################

# Oppgave 5  Last inn pakken Kostra og datasettet kirkedata_0.R

library(Kostra)
load("kirkedata_0.RData")


# Oppgave 6 Sjekk om det er tusedenfeil funksjoen ThError i rapportering av antall døpte.
#           Lag figurer eller print ut resulatet 

tusres<-ThError(data = kirkedata_0, id = "region", x1 = "dopte", x2 = "dopte_1",
                ll = -3, ul = 3)


#Plotter punktene, med forklaringer på aksene og tittel
plot_ly(data = tusres,x= ~x1, y=~diffLog10, type ="scatter", split = ~outlier,mode ="markers",
                text = paste("<br><br>Id:  ",tusres$id, "<br><br>Verdi i år:  ", tusres$x1,"<br>Verdi i fjor:", tusres$x2 ),            
                hovertemplate = paste( "<b>%{text}<br>", "Logaritmen til differansen: %{y:}<br>", "<extra></extra>" )              )   %>% 
  layout(title = "Tusenfeil", xaxis = list(title = "Døpte"), yaxis = list(title = "logaritmen til differansen"), legend=list(title=list(text='Outlier:'))) %>%
  add_trace(x = ~x1, y=~upperLimit,inherit = FALSE, name = 'Øvre grense' , mode="lines", type="scatter")  %>% 
  add_trace(x = ~x1, y=~lowerLimit,inherit = FALSE, name = 'Nedre grense',mode="lines", type="scatter") 


# Oppgave 7 Sjekk om du finner noen merkelige observasjoner med HB-metoden (Hb), varier parametre C og U.


hbres <- Hb(data = kirkedata_0, id = "region", x1 = "dopte", x2 = "dopte_1",pC=8,pU=0.6,pA=0.05)

#Dataene må være sortert for at grafen skal bli korrekt
hbres<-hbres[order(hbres$maxX),]

#Plotter punktene, med forklaringer på aksene og tittel


plot_ly(data = hbres,x= ~maxX, y=~ratio, type ="scatter",split = ~outlier, mode ="markers",
        text = paste("<br><br>Id:  ",hbres$id, "<br><br>Verdi i år:  ", hbres$x1,"<br>Verdi i fjor:", hbres$x2 ),            
        hovertemplate = paste( "<b>%{text}<br>", "Forholdstallet: %{y:}<br>", "<extra></extra>" )   
       )  %>% 
  layout(title = "HB metoden Døpte i kommunen",
         xaxis = list(title = "Maks antall døpte"),
         yaxis = list(title = "Forholdstallet"),
         legend=list(title=list(text='Outlier:'))
  ) %>% 
  add_trace(x = ~maxX, y=~upperLimit, name = 'Øvre grense' , mode="lines", inherit = FALSE, type="scatter")  %>% 
  add_trace(x = ~maxX, y=~lowerLimit, name = 'Nedre grense',mode="lines", inherit = FALSE, type="scatter") 



# Oppgave 8 Test ut kvartilmetoden Quartilemed  forskjellige grenseverdier på forholdet mellom døpte og fødte i en kommune

qres <- Quartile(data = kirkedata_0, id = "region", x1 = "dopte", y1 = "fodde",pKL=3,pKU=3)
plot_ly(data = qres,x= ~ratio, type ="histogram", name="observasjon")  %>% 
  layout(title = "Kvartilmetode",
         xaxis = list(title = "Andel døpte per fødte"),
         yaxis = list(title = "Antall kommuner"))%>% 
  add_segments(x = ~upperLimit, xend=~upperLimit,y=0, yend=50, name = 'Øvre grense' ) %>% 
  add_segments(x = ~lowerLimit, xend=~lowerLimit,y=0, yend=50,name = 'Øvre grense' ) 

# Oppgave 9 Test ut robust regresjon (OutlierRegressionMicro) med forskjellige modeller og
#           grenser på forholde mellom døpte og fødte

regres <- OutlierRegressionMicro(data= kirkedata_0, idName ="region" , strataName = NULL,
                                 xName ="fodde" , yName ="dopte" ,
                                 method = "ordinary", limitModel = 5, limitIterate = 6)

#Plotter punktene, med forklaringer på aksene og tittel
plot_ly(data = regres,x= ~x, y=~y, type ="scatter", mode ="markers", split = ~outlier,
                text = paste("<br><br>Id:  ",regres$id, "<br><br>fødte :  ", regres$x,"<br>Døpte:", regres$y ),            
                hovertemplate = paste( "<b>%{text}<br>" )  
)  %>% 
  layout(title = "Robust regresjon døpte mot fødte i kommunen",
         xaxis = list(title = "Antall fødte"),
         yaxis = list(title = "Antall døpte"),
         legend=list(title=list(text='Outlier:'))
  ) %>% 
  add_lines(x = ~x, y=~yHat, name="Linje", inherit = FALSE, type="scatter")  




# Oppgave 10 Test ut funksjonen Rank2NumVar for å finne innflytelse på totalen for antall døpte
rankres<-Rank2NumVar(data=kirkedata_0, idVar= "region", xVar="dopte", yVar= "dopte_1", strataVar =NULL, 
                     antall = 10,grense = NULL, identiske = FALSE)
rankres

# Oppgave 11 Test ut funksjonen Diff2NumVar for å finne innflytelse på endringstallet
diffres<- Diff2NumVar(data=kirkedata_0, idVar= "region", xVar="dopte_1", yVar="dopte", strataVar = NULL, antall = 5,
                      grense = NULL, zVar = NULL, kommentarVar = NULL)

diffres[,c("id","x","y","Diff","DiffProsAvTotx") ]

# Oppgave 12 Test ut analyse av aggregat med funksjonen AggrSml2NumVar
aggres<- AggrSml2NumVar(data=kirkedata_0, xVar="dopte_1", yVar="dopte", strataVar = "kostragr", identiske = FALSE)
aggres

plot_ly(data=aggres, x = ~strata,  y = ~Sumx, name = "Døpte forrige år", type = "bar") %>% 
  add_trace(y = ~Sumy, name = 'Døpte i år')


plot_ly(data=aggres, x =~strata ,  y = ~Diff, type = "bar")
