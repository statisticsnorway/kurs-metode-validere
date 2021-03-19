
######## Validering ###########################

#Laster ned pakken med funksjoner
library(validate)

#Hente data
load("kirkedata.RData")

#Skriver de logiske kontrollene
regler<-validator( konfirmanter>0,
              konfirmanter<=personer15, 
              konfirmanter_1<=personer15_1)

#Kjører kontrollene
cf<-confront(kirkedata,regler,key="region")

#Oppsummering av kontrollen
summary(cf)

#Indikatorer for kontrollene
aggregate(cf)

#Figur av kontrollene
plot(cf, main="Konfirmant-datasett, logiske feil")

#Printer ut outlierne
resultat<-as.data.frame(cf)

outlier1<-resultat[resultat$value=="FALSE",]#Selekterer ut de som feilet

outlier <- na.omit(outlier1)#Selekterer ut de som ikke er uoppgitt (NA)

outlier[1:5,]#Printer ut de 5 første outlierne


#Lager indikator-datasett med TRUE/FALSE på kontrollene som er slått ut og setter det sammen med inputdata
kirke_res<-as.data.frame(values(cf))

#Gjør det slik at id-variabel blir en variabel
kirke_res$row_names <- row.names(kirke_res)  
names(kirke_res)[names(kirke_res) == "row_names"] <- "region"

#Setter sammen datasett og kvalitetsindikatorer
kirkedata_kon<-merge(kirkedata,kirke_res,by="region",all.kirkedata = all) 

#Printer ut de 5 første radene
kirkedata_kon[1:5,]

# set properties
names(regler)[1] <- "reg1"
label(regler)[1] <- "konfirmanter positive"
description(regler)[1] <- "Antall konfirmanter må være et positivt tall"
names(regler)[2] <- "reg2"
label(regler)[2] <- "Færre enn 15-åringer"
description(regler)[2] <- "Antall konfirmanter må være færre enn antall 15åringer"
names(regler)[3] <- "reg3"
label(regler)[3] <- "Færre enn 15-åringer"
description(regler)[3] <- "Antall konfirmanter må være færre enn antall 15åringer"

cf<-confront(kirkedata,regler,key="region")

summary(cf)
plot(cf)

# Kontroll av kategoriske verdier med Klass
library(klassR)

#Henter klassifikasjon for kommune med gyldighetsdato
sn <- GetKlass(klass = 131, date = "2019-01-01") 

#Trekker ut selve koden for kommunene og legger det i en vektor 
komliste<-as.vector(sn[,c("code")])

#Oppsett av regler
regler<-validator( region %in% komliste)

#Kjøre kontroller
cf<-confront(kirkedata,regler,key="region")

#Oppsummering av kontrollene
summary(cf)
plot(cf)

##################### Kontroll metoder ##########################################

library(Kostra)
#library(SSBtools)
load("kirkedata_0.RData")
library(plotly)


####Tusenfeil

tusres<-ThError(data = kirkedata_0, id = "region", x1 = "konfirmanter", x2 = "konfirmanter_1", ll = -3, ul = 3)

#Plotter punktene, med forklaringer på aksene og tittel
fig1 <- plot_ly(data = tusres,x= ~x1, y=~diffLog10, type ="scatter", name="observasjon", mode ="markers")   %>% 
  layout(title = "Tusenfeil", xaxis = list(title = "Konfirmanter"), yaxis = list(title = "logaritmen til differansen")) %>%
    add_trace(x = ~x1, y=~upperLimit, name = 'Øvre grense' , mode="lines")  %>% 
  add_trace(x = ~x1, y=~lowerLimit, name = 'Nedre grense',mode="lines") 
fig1


####HB-funksjonen
hbres <- Hb(data = kirkedata_0, id = "region", x1 = "konfirmanter", x2 = "konfirmanter_1",pC=10,pU=0.9,pA=0.05)

#Plotter funksjonen
hbres<-hbres[order(hbres$maxX),]

#Plotter punktene, med forklaringer på aksene og tittel
fig2 <- plot_ly(data = hbres,x= ~maxX, y=~ratio, type ="scatter", name="observasjon", mode ="markers")  %>% 
        layout(title = "HB metoden konfirmanter i kommunen",
                      xaxis = list(title = "Maks antall Konfirmanter"),
                      yaxis = list(title = "Forholdstallet")) %>% 
        add_trace(x = ~maxX, y=~upperLimit, name = 'Øvre grense' , mode="lines")  %>% 
        add_trace(x = ~maxX, y=~lowerLimit, name = 'Nedre grense',mode="lines") 
fig2


####Kvartilmetode
qres <- Quartile(data = kirkedata_0, id = "region", x1 = "konfirmanter", y1 = "personer15",pKL=2,pKU=2)

# ser på outlierne
qres[qres$outlier==1,c("id","x1","y1","ratio","ratioAll")]


#lage grafikk av metoden
fig3 <- plot_ly(data = qres,x= ~ratio, type ="histogram", name="observasjon")  %>% 
        layout(title = "Kvartilmetode andelen konfirmanter i kommunen",
                      xaxis = list(title = "Andel konfirmanter"),
                      yaxis = list(title = "Antall kommuner"))%>% 
      add_segments(x = ~upperLimit, xend=~upperLimit,y=0, yend=50, name = 'Øvre grense' ) %>% 
      add_segments(x = ~lowerLimit, xend=~lowerLimit,y=0, yend=50,name = 'Øvre grense' ) 
      
fig3



############# Robust regresjon ####################################
regres <- OutlierRegressionMicro(data= kirkedata_0, idName ="region" , strataName = NULL,
                                 xName ="personer15" , yName ="konfirmanter" ,
                                 method = "ordinary", limitModel = 6, limitIterate = 6)


#Plotter punktene, med forklaringer på aksene og tittel

fig4 <- plot_ly(data = regres,x= ~x, y=~y, type ="scatter", mode ="markers", color =~ as.character(outlier), colors=c("blue","red"))  %>% 
  layout(title = "Robust regresjon konfirmanter mot 15-åringer i kommunen",
         xaxis = list(title = "Antall 15-åringer"),
         yaxis = list(title = "Antall Konfirmanter")) %>% 
  add_lines(x = ~x, y=~yHat, name="Linje")  
fig4



# ser på outlierne
regres[regres$outlier==1,c("id","x","y","rStud")]

################# Innflytelse på totalen ###################################
rankres<-Rank2NumVar(data=kirkedata_0, idVar= "region", xVar="konfirmanter", yVar= "konfirmanter_1", 
                     strataVar = NULL, antall = 10,grense = NULL, identiske = FALSE)
rankres

################ Innflytelse på endringstallet ###############################
#kjøre metoden med satte parametre
diffres<- Diff2NumVar(data=kirkedata_0, idVar= "region", xVar="konfirmanter_1", yVar="konfirmanter", 
                      strataVar = , antall = 10, grense = NULL, zVar = NULL, kommentarVar = NULL)


diffres[,c("id","x","y","Diff","DiffProsAvTotx") ]

############### Analyse av aggregat #######################################
#kjøre metoden med satte parametre
aggres<- AggrSml2NumVar(data=kirkedata_0, xVar="konfirmanter_1", yVar="konfirmanter", 
                        strataVar = "kostragr", identiske = FALSE)
aggres

fig5 <- plot_ly(data=aggres, x = ~strata,  y = ~Sumx, name = "Konfirmanter forrige år", type = "bar") %>% 
        add_trace(y = ~Sumy, name = 'Konfirmanter i år')
fig5

fig6 <- plot_ly(data=aggres, x =~strata ,  y = ~Diff, type = "bar")
  
fig6

