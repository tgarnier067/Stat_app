com <- read_excel("PFL_E.xlsx", sheet = "Sheet1")


### Upload Covalence indicator

com<-read_excel("G:/Mon Drive/Recherche/Projet greenwashing - Fanny Peter/Data/Covalence/PFL_E.xlsx", sheet = "Sheet1")

com2 <- com %>%
  pivot_longer(
    cols = -idEntreprise,               # Toutes les colonnes sauf 'id'
    names_to = "date",        # Nom de la nouvelle colonne pour les dates
    values_to = "value"       # Nom de la nouvelle colonne pour les valeurs
  )

com2$y<-substr(com2$date,1,4)
com2$m<-substrRight(com2$date,2)
com2$my<-paste(com2$y,com2$m,sep = "")
com2$my<-as.numeric(com2$my)

iden<-read_excel("Universe_Listed_Covalence_31.07.2023_v2_updated.csv", sheet = "Sheet1")

com2$ISIN=iden$ISIN[match(paste(com2$idEntreprise),paste(iden$idEntreprise))]
com2$Company=iden$Company[match(paste(com2$idEntreprise),paste(iden$idEntreprise))]
com2$Sect=iden$`GICS sub-industry`[match(paste(com2$idEntreprise),paste(iden$idEntreprise))]
com2$Country=iden$`Headquarters Country`[match(paste(com2$idEntreprise),paste(iden$idEntreprise))]
com2$Zone=iden$`Headquarters Region`[match(paste(com2$idEntreprise),paste(iden$idEntreprise))]


base$com=com2$value[match(paste(base$ISIN,base$my),paste(com2$ISIN,com2$my))]
base$Country=com2$Country[match(paste(base$ISIN,base$my),paste(com2$ISIN,com2$my))]
base$Zone=com2$Zone[match(paste(base$ISIN,base$my),paste(com2$ISIN,com2$my))]

load("BaseCom.RData")

basecom <- base %>%
  filter(!is.na(com))

basecom <- basecom %>%
  group_by(ISIN) %>%
  mutate(phi = com - lag(com, default = NA)) %>%
  ungroup()  # facultatif, mais c'est une bonne pratique pour ne plus avoir le dataframe groupé.

basecom <- basecom %>%
  group_by(ISIN) %>%
  mutate(phip = (com - lag(com, default = NA))/lag(com, default = NA)) %>%
  ungroup()  # facultatif, mais c'est une bonne pratique pour ne plus avoir le dataframe groupé.

basend <- basecom %>%
  filter(phi!=0)

#Normaliser RRI pour le transformer en note 
#basend$RRIn<-100-basend$current_RRI
basend$RRIn<-basend$current_RRI


basend$RRI<-basend$RRIn
basend$RRIE<-basend$RRI*as.numeric(substr(basend$environmental_percentage,1,(nchar(basend$environmental_percentage)-1)))/100

basend <- basend %>%
  group_by(ISIN) %>%
  mutate(lagphi = lag(phi),
         lag2phi = lag(lagphi),
         lagphip = lag(phip),
         lagRRI = lag(RRI),
         lag2RRI = lag(lag(RRI)),
         lagRRIE = lag(RRIE),
         lag2RRIE = lag(lag(RRIE))) %>%
  ungroup()  # facultatif, mais c'est une bonne pratique pour ne plus avoir le dataframe groupé.

basend$dRRI<-basend$lagRRI-basend$lag2RRI
basend$dRRIE<-basend$lagRRIE-basend$lag2RRIE

basend$dphi<-basend$phi-basend$lagphi
basend$dphip<-basend$phip-basend$lagphip


com<-read_excel("G:/Mon Drive/Recherche/Projet greenwashing - Fanny Peter/Data/Covalence/New/PFL_E.xlsx", sheet = "Sheet1")
comb<-read_excel("G:/Mon Drive/Recherche/Projet greenwashing - Fanny Peter/Data/Covalence/New/NO_E.xlsx", sheet = "Sheet1")
fcomp<-read_excel("G:/Mon Drive/Recherche/Projet greenwashing - Fanny Peter/Data/Covalence/New/FL_positives_E.xlsx", sheet = "Sheet1")
fcomn<-read_excel("G:/Mon Drive/Recherche/Projet greenwashing - Fanny Peter/Data/Covalence/New/FL_negatives_E.xlsx", sheet = "Sheet1")
esge<-read_excel("G:/Mon Drive/Recherche/Projet greenwashing - Fanny Peter/Data/Covalence/New/ESG_E.xlsx", sheet = "Sheet1")
esge1<-read_excel("G:/Mon Drive/Recherche/Projet greenwashing - Fanny Peter/Data/Covalence/New/ESG_scoreenv1.xlsx", sheet = "Sheet1")
esge2<-read_excel("G:/Mon Drive/Recherche/Projet greenwashing - Fanny Peter/Data/Covalence/New/ESG_scoreenv2.xlsx", sheet = "Sheet1")
esge3<-read_excel("G:/Mon Drive/Recherche/Projet greenwashing - Fanny Peter/Data/Covalence/New/ESG_scoreenv3.xlsx", sheet = "Sheet1")
con<-read_excel("G:/Mon Drive/Recherche/Projet greenwashing - Fanny Peter/Data/Covalence/New/CON_E.xlsx", sheet = "Sheet1")
con1<-read_excel("G:/Mon Drive/Recherche/Projet greenwashing - Fanny Peter/Data/Covalence/New/ESGconenv1.xlsx", sheet = "Sheet1")
con2<-read_excel("G:/Mon Drive/Recherche/Projet greenwashing - Fanny Peter/Data/Covalence/New/ESGconenv2.xlsx", sheet = "Sheet1")
con3<-read_excel("G:/Mon Drive/Recherche/Projet greenwashing - Fanny Peter/Data/Covalence/New/ESGconenv3.xlsx", sheet = "Sheet1")
fcon1<-read_excel("G:/Mon Drive/Recherche/Projet greenwashing - Fanny Peter/Data/Covalence/New/conVol_env1.xlsx", sheet = "Sheet1")
fcon2<-read_excel("G:/Mon Drive/Recherche/Projet greenwashing - Fanny Peter/Data/Covalence/New/conVol_env2.xlsx", sheet = "Sheet1")
fcon3<-read_excel("G:/Mon Drive/Recherche/Projet greenwashing - Fanny Peter/Data/Covalence/New/conVol_env3.xlsx", sheet = "Sheet1")

com2 <- com %>%
  pivot_longer(
    cols = -idEntreprise,               # Toutes les colonnes sauf 'id'
    names_to = "date",        # Nom de la nouvelle colonne pour les dates
    values_to = "value"       # Nom de la nouvelle colonne pour les valeurs
  )

com2$y<-substr(com2$date,1,4)
com2$m<-substrRight(com2$date,2)
com2$my<-paste(com2$y,com2$m,sep = "")
com2$my<-as.numeric(com2$my)

comb2 <- comb %>%
  pivot_longer(
    cols = -idEntreprise,               # Toutes les colonnes sauf 'id'
    names_to = "date",        # Nom de la nouvelle colonne pour les dates
    values_to = "value"       # Nom de la nouvelle colonne pour les valeurs
  )

comb2$y<-substr(comb2$date,1,4)
comb2$m<-substrRight(comb2$date,2)
comb2$my<-paste(comb2$y,comb2$m,sep = "")
comb2$my<-as.numeric(comb2$my)

fcomp2 <- fcomp %>%
  pivot_longer(
    cols = -idEntreprise,               # Toutes les colonnes sauf 'id'
    names_to = "date",        # Nom de la nouvelle colonne pour les dates
    values_to = "value"       # Nom de la nouvelle colonne pour les valeurs
  )

fcomp2$y<-substr(fcomp2$date,1,4)
fcomp2$m<-substrRight(fcomp2$date,2)
fcomp2$my<-paste(fcomp2$y,fcomp2$m,sep = "")
fcomp2$my<-as.numeric(fcomp2$my)

fcomn2 <- fcomn %>%
  pivot_longer(
    cols = -idEntreprise,               # Toutes les colonnes sauf 'id'
    names_to = "date",        # Nom de la nouvelle colonne pour les dates
    values_to = "value"       # Nom de la nouvelle colonne pour les valeurs
  )

fcomn2$y<-substr(fcomn2$date,1,4)
fcomn2$m<-substrRight(fcomn2$date,2)
fcomn2$my<-paste(fcomn2$y,fcomn2$m,sep = "")
fcomn2$my<-as.numeric(fcomn2$my)

esgenew <- esge %>%
  pivot_longer(
    cols = -idEntreprise,               # Toutes les colonnes sauf 'id'
    names_to = "date",        # Nom de la nouvelle colonne pour les dates
    values_to = "value"       # Nom de la nouvelle colonne pour les valeurs
  )

esgenew$y<-substr(esgenew$date,1,4)
esgenew$m<-substrRight(esgenew$date,2)
esgenew$my<-paste(esgenew$y,esgenew$m,sep = "")
esgenew$my<-as.numeric(esgenew$my)

esge1new <- esge1 %>%
  pivot_longer(
    cols = -idEntreprise,               # Toutes les colonnes sauf 'id'
    names_to = "date",        # Nom de la nouvelle colonne pour les dates
    values_to = "value"       # Nom de la nouvelle colonne pour les valeurs
  )

esge1new$y<-substr(esge1new$date,1,4)
esge1new$m<-substrRight(esge1new$date,2)
esge1new$my<-paste(esge1new$y,esge1new$m,sep = "")
esge1new$my<-as.numeric(esge1new$my)

esge2new <- esge2 %>%
  pivot_longer(
    cols = -idEntreprise,               # Toutes les colonnes sauf 'id'
    names_to = "date",        # Nom de la nouvelle colonne pour les dates
    values_to = "value"       # Nom de la nouvelle colonne pour les valeurs
  )

esge2new$y<-substr(esge2new$date,1,4)
esge2new$m<-substrRight(esge2new$date,2)
esge2new$my<-paste(esge2new$y,esge2new$m,sep = "")
esge2new$my<-as.numeric(esge2new$my)

esge3new <- esge3 %>%
  pivot_longer(
    cols = -idEntreprise,               # Toutes les colonnes sauf 'id'
    names_to = "date",        # Nom de la nouvelle colonne pour les dates
    values_to = "value"       # Nom de la nouvelle colonne pour les valeurs
  )

esge3new$y<-substr(esge3new$date,1,4)
esge3new$m<-substrRight(esge3new$date,2)
esge3new$my<-paste(esge3new$y,esge3new$m,sep = "")
esge3new$my<-as.numeric(esge3new$my)

connew <- con %>%
  pivot_longer(
    cols = -idEntreprise,               # Toutes les colonnes sauf 'id'
    names_to = "date",        # Nom de la nouvelle colonne pour les dates
    values_to = "value"       # Nom de la nouvelle colonne pour les valeurs
  )

connew$y<-substr(connew$date,1,4)
connew$m<-substrRight(connew$date,2)
connew$my<-paste(connew$y,connew$m,sep = "")
connew$my<-as.numeric(connew$my)

con1new <- con1 %>%
  pivot_longer(
    cols = -idEntreprise,               # Toutes les colonnes sauf 'id'
    names_to = "date",        # Nom de la nouvelle colonne pour les dates
    values_to = "value"       # Nom de la nouvelle colonne pour les valeurs
  )

con1new$y<-substr(con1new$date,1,4)
con1new$m<-substrRight(con1new$date,2)
con1new$my<-paste(con1new$y,con1new$m,sep = "")
con1new$my<-as.numeric(con1new$my)

con2new <- con2 %>%
  pivot_longer(
    cols = -idEntreprise,               # Toutes les colonnes sauf 'id'
    names_to = "date",        # Nom de la nouvelle colonne pour les dates
    values_to = "value"       # Nom de la nouvelle colonne pour les valeurs
  )

con2new$y<-substr(con2new$date,1,4)
con2new$m<-substrRight(con2new$date,2)
con2new$my<-paste(con2new$y,con2new$m,sep = "")
con2new$my<-as.numeric(con2new$my)

con3new <- con3 %>%
  pivot_longer(
    cols = -idEntreprise,               # Toutes les colonnes sauf 'id'
    names_to = "date",        # Nom de la nouvelle colonne pour les dates
    values_to = "value"       # Nom de la nouvelle colonne pour les valeurs
  )

con3new$y<-substr(con3new$date,1,4)
con3new$m<-substrRight(con3new$date,2)
con3new$my<-paste(con3new$y,con3new$m,sep = "")
con3new$my<-as.numeric(con3new$my)

fcon1new <- fcon1 %>%
  pivot_longer(
    cols = -idEntreprise,               # Toutes les colonnes sauf 'id'
    names_to = "date",        # Nom de la nouvelle colonne pour les dates
    values_to = "value"       # Nom de la nouvelle colonne pour les valeurs
  )

fcon1new$y<-substr(fcon1new$date,1,4)
fcon1new$m<-substrRight(fcon1new$date,2)
fcon1new$my<-paste(fcon1new$y,fcon1new$m,sep = "")
fcon1new$my<-as.numeric(fcon1new$my)

fcon2new <- fcon2 %>%
  pivot_longer(
    cols = -idEntreprise,               # Toutes les colonnes sauf 'id'
    names_to = "date",        # Nom de la nouvelle colonne pour les dates
    values_to = "value"       # Nom de la nouvelle colonne pour les valeurs
  )

fcon2new$y<-substr(fcon2new$date,1,4)
fcon2new$m<-substrRight(fcon2new$date,2)
fcon2new$my<-paste(fcon2new$y,fcon2new$m,sep = "")
fcon2new$my<-as.numeric(fcon2new$my)

fcon3new <- fcon3 %>%
  pivot_longer(
    cols = -idEntreprise,               # Toutes les colonnes sauf 'id'
    names_to = "date",        # Nom de la nouvelle colonne pour les dates
    values_to = "value"       # Nom de la nouvelle colonne pour les valeurs
  )

fcon3new$y<-substr(fcon3new$date,1,4)
fcon3new$m<-substrRight(fcon3new$date,2)
fcon3new$my<-paste(fcon3new$y,fcon3new$m,sep = "")
fcon3new$my<-as.numeric(fcon3new$my)

# iden<-read_excel("G:/Mon Drive/Recherche/Projet greenwashing - Fanny Peter/Data/Covalence/Universe_Listed_Covalence_31.07.2023 v2.xlsx", sheet = "Sheet1")
iden<-read_excel("G:/Mon Drive/Recherche/Projet greenwashing - Fanny Peter/Data/Covalence/Universe_Listed_Covalence_31.07.2023_v2_updated.xlsx", sheet = "Sheet1")

com2$ISIN=iden$ISIN[match(paste(com2$idEntreprise),paste(iden$idEntreprise))]
com2$Company=iden$Company[match(paste(com2$idEntreprise),paste(iden$idEntreprise))]
com2$Sect=iden$`GICS sub-industry`[match(paste(com2$idEntreprise),paste(iden$idEntreprise))]
com2$Country=iden$`Headquarters Country`[match(paste(com2$idEntreprise),paste(iden$idEntreprise))]
com2$Zone=iden$`Headquarters Region`[match(paste(com2$idEntreprise),paste(iden$idEntreprise))]

com2$fcomp=fcomp2$value[match(paste(com2$idEntreprise,com2$date),paste(fcomp2$idEntreprise,fcomp2$date))]
com2$fcomn=fcomn2$value[match(paste(com2$idEntreprise,com2$date),paste(fcomn2$idEntreprise,fcomn2$date))]
com2$esge=esgenew$value[match(paste(com2$idEntreprise,com2$date),paste(esgenew$idEntreprise,esgenew$date))]
com2$esge1=esge1new$value[match(paste(com2$idEntreprise,com2$date),paste(esge1new$idEntreprise,esge1new$date))]
com2$esge2=esge2new$value[match(paste(com2$idEntreprise,com2$date),paste(esge2new$idEntreprise,esge2new$date))]
com2$esge3=esge3new$value[match(paste(com2$idEntreprise,com2$date),paste(esge3new$idEntreprise,esge3new$date))]
com2$con=connew$value[match(paste(com2$idEntreprise,com2$date),paste(connew$idEntreprise,connew$date))]
com2$con1=con1new$value[match(paste(com2$idEntreprise,com2$date),paste(con1new$idEntreprise,con1new$date))]
com2$con2=con2new$value[match(paste(com2$idEntreprise,com2$date),paste(con2new$idEntreprise,con2new$date))]
com2$con3=con3new$value[match(paste(com2$idEntreprise,com2$date),paste(con3new$idEntreprise,con3new$date))]
com2$fcon1=fcon1new$value[match(paste(com2$idEntreprise,com2$date),paste(fcon1new$idEntreprise,fcon1new$date))]
com2$fcon2=fcon2new$value[match(paste(com2$idEntreprise,com2$date),paste(fcon2new$idEntreprise,fcon2new$date))]
com2$fcon3=fcon3new$value[match(paste(com2$idEntreprise,com2$date),paste(fcon3new$idEntreprise,fcon3new$date))]
com2$comb=comb2$value[match(paste(com2$idEntreprise,com2$date),paste(comb2$idEntreprise,comb2$date))]


names(com2)[names(com2) == "value"] <- "com"
#com2 <- cbind(subset(com2, select = -com), com = com2$com)

com <- com2
#rm(com2,fcomn,fcomn2,fcomp,fcomp2)

crsp <- read.csv("G:/Mon Drive/Recherche/Data/CRSP/CRSP - Dec 2022.csv", header = TRUE, sep = ",")

#Constuire market return et beta ici
crsp$weight<-crsp$SHROUT*crsp$PRC
crsp$RET<-as.numeric(crsp$RET)
crsp$date <- ymd(crsp$date)
crspmkt <- crsp %>%
  group_by(date) %>%
  summarise(RETm = sum(RET * weight, na.rm=TRUE) / sum(weight,na.rm=TRUE))
crsp$RETm<-crspmkt$RETm[match(paste(crsp$date),paste(crspmkt$date))]

crsp <- crsp %>%
  arrange(CUSIP, date) %>%
  group_by(CUSIP) %>%
  mutate(rolling_beta_fast = rollapply(data = data.frame(RET, RETm), width = 12, FUN = function(x) {
    cov_xy <- cov(x[, 1], x[, 2], use = "pairwise.complete.obs")
    var_y <- var(x[, 2], na.rm = TRUE)
    cov_xy / var_y
  }, by.column = FALSE, align = "right", fill = NA))


#

crsp2<-crsp[,c("PERMNO","CUSIP","date","PRC","SHROUT","RET","rolling_beta_fast")]

indicfin <- read.csv("G:/Mon Drive/Recherche/Data/WRDS Financial ratios/Financial ratios WRDS - Sep 2023.csv", header = TRUE, sep = ",")
indicfin2<-indicfin[,c("permno","qdate","bm","de_ratio","roe")]

crsp2$bm=indicfin2$bm[match(paste(crsp2$PERMNO,crsp2$date),paste(indicfin2$permno,indicfin2$qdate))]
crsp2$de_ratio=indicfin2$de_ratio[match(paste(crsp2$PERMNO,crsp2$date),paste(indicfin2$permno,indicfin2$qdate))]
crsp2$roe=indicfin2$roe[match(paste(crsp2$PERMNO,crsp2$date),paste(indicfin2$permno,indicfin2$qdate))]
crsp2$MV<-ifelse(crsp2$SHROUT*crsp2$PRC>=0,crsp2$SHROUT*crsp2$PRC,NA)

crsp2$m<-substr(crsp2$date,6,7)
crsp2$y <- substr(crsp2$date,1,4)
crsp2$my<-paste(crsp2$y,crsp2$m,sep = "")
crsp2$my<-as.numeric(crsp2$my)

com$CUSIP<-substr(com$ISIN, 3, 10)
com$ret=crsp2$RET[match(paste(com$CUSIP,com$my),paste(crsp2$CUSIP,crsp2$my))]
com$beta12=crsp2$rolling_beta_fast[match(paste(com$CUSIP,com$my),paste(crsp2$CUSIP,crsp2$my))]

com$mv=crsp2$MV[match(paste(com$CUSIP,com$my),paste(crsp2$CUSIP,crsp2$my))]
com$bm=crsp2$bm[match(paste(com$CUSIP,com$my),paste(crsp2$CUSIP,crsp2$my))]
com$de_ratio=crsp2$de_ratio[match(paste(com$CUSIP,com$my),paste(crsp2$CUSIP,crsp2$my))]
com$roe=crsp2$roe[match(paste(com$CUSIP,com$my),paste(crsp2$CUSIP,crsp2$my))]

# save(com, file="G:/Mon Drive/Recherche/Projet greenwashing - Fanny Peter/Code/BaseCom.RData")
# save(com, file="G:/Mon Drive/Recherche/Projet greenwashing - Fanny Peter/Code/BaseComb.RData")
# save(com, file="G:/Mon Drive/Recherche/Projet greenwashing - Fanny Peter/Code/BaseCombUpdated.RData")
# save(com, file="G:/Mon Drive/Recherche/Projet greenwashing - Fanny Peter/Code/BaseCom9Nov23.RData")



### Estimations ----

load("G:/Mon Drive/Recherche/Projet greenwashing - Fanny Peter/Code/BaseCom9Nov23.RData") 


## Data: Remove ISINs=NA

com <- com[!is.na(com$ISIN), ]


## Création de variables

com <- com %>%
  group_by(ISIN) %>%
  mutate(lagesge = lag(esge),
         lag2esge=lag(lag(esge)),
         lagesge1 = lag(esge1),
         lagesge2 = lag(esge2),
         lagesge3 = lag(esge3),
         lag2esge1 = lag(lag(esge1)),
         lag2esge2 = lag(lag(esge2)),
         lag2esge3 = lag(lag(esge3)),
         leadesge = lead(esge),
         lead2esge = lead(leadesge),
         lead3esge = lead(lead2esge),
         lagcon = lag(con),
         lagcon1 = lag(con1),
         lagcon2 = lag(con2),
         lagcon3 = lag(con3),
         leadcom=lead(com),
         #leadcomb=lead(comb), #lead com backward
         lead2com=lead(leadcom),
         lead3com=lead(lead2com),
         lead4com=lead(lead3com),
         lead5com=lead(lead4com),
         lead6com=lead(lead5com),
         lead12com=lead(com,12),
         lagcom=lag(com),
         lagbeta12=lag(beta12),
         lagret=lag(ret)) %>%
  ungroup()  # facultatif, mais c'est une bonne pratique pour ne plus avoir le dataframe groupé.
com$desge<-com$esge-com$lagesge
com$leaddesge<-com$leadesge-com$esge
com$lagdesge<-com$lagesge-com$lag2esge
com$desge1<-com$esge1-com$lagesge1
com$desge2<-com$esge2-com$lagesge2
com$desge3<-com$esge3-com$lagesge3
com$lagdesge1<-com$lagesge1-com$lag2esge1
com$lagdesge2<-com$lagesge2-com$lag2esge2
com$lagdesge3<-com$lagesge3-com$lag2esge3
com$dcon<-com$con-com$lagcon
com$dcon1<-com$con1-com$lagcon1
com$dcon2<-com$con2-com$lagcon2
com$dcon3<-com$con3-com$lagcon3
com$dcom<-com$leadcom-com$com
com$d2com<-com$lead2com-com$com
com$d3com<-com$lead3com-com$com
com$d4com<-com$lead4com-com$com
com$d5com<-com$lead5com-com$com
com$d6com<-com$lead6com-com$com
com$d12com<-com$lead12com-com$com
com$dcomlag<-com$dcom-com$lagcom
com$desgelead<-com$leadesge-com$esge
com$desgelead2<-com$lead2esge-com$esge
com$desgelead3<-com$lead3esge-com$esge
com$desgeup<-ifelse(com$desge>0,com$desge,0)
com$desgedown<-ifelse(com$desge<0,com$desge,0)
com$dbeta<-com$beta12-com$lagbeta12
com$dbetap<-(com$beta12-com$lagbeta12)/com$lagbeta12
com$dret<-com$ret-com$lagret

com <- com %>%
  group_by(ISIN) %>%
  mutate(lagdcom = lag(dcom),
         laglagdcom = lag(lag(dcom)),
         leaddcom=lead(dcom),
         lead2dcom=lead(leaddcom),
         lead3dcom=lead(dcom,3),
         lead4dcom=lead(dcom,4),
         lead5dcom=lead(dcom,5),
         lead6dcom=lead(dcom,6),
         lead7dcom=lead(dcom,7),
         lead8dcom=lead(dcom,8),
         lead9dcom=lead(dcom,9),
         lead10dcom=lead(dcom,10),
         lead11dcom=lead(dcom,11)
         ) %>%
  ungroup() 
com$ddcom<-com$dcom-com$lagdcom
com$ddcom2m<-com$leaddcom+com$dcom-com$lagdcom
com$ddcom3m<-com$lead2dcom+com$leaddcom+com$dcom-com$lagdcom
com$ddcom4m<-com$lead3dcom+com$lead2dcom+com$leaddcom+com$dcom-com$lagdcom
com$ddcom5m<-com$lead4dcom+com$lead3dcom+com$lead2dcom+com$leaddcom+com$dcom-com$lagdcom
com$ddcom6m<-com$lead5dcom+com$lead4dcom+com$lead3dcom+com$lead2dcom+com$leaddcom+com$dcom-com$lagdcom
com$ddcom12m<-com$lead11dcom+com$lead10dcom+com$lead9dcom+com$lead8dcom+com$lead7dcom+com$lead6dcom+com$lead5dcom+com$lead4dcom+com$lead3dcom+com$lead2dcom+com$leaddcom+com$dcom-com$lagdcom
com$ddcomlag<-com$lagdcom-com$laglagdcom



#Focus on data up to Dec22 to finish on a "round date" instead of April23

com$ret<-as.numeric(com$ret)
com<-com[com$my<=202212,]


## Retirer les controverses à la réputation + faire les régressions

#Créer la variable de chgt de com ddres
comreg<-com[com$my>=201512,]
pdata <- pdata.frame(comreg, index = c("ISIN","date"))

#Combien d'entreprises:
length(unique(pdata$ISIN))
nrow((pdata[!is.na(pdata$com)&!is.na(pdata$con),]))
#

sls <- plm(com ~ con | lagcon, data = pdata, model = "within",effect="twoways")
summary(sls)
crob <- coeftest(sls, vcov = vcovHC(sls, type = "HC3"))
print(crob)

summary(sls,vcov = vcovHC(sls, type = "HC3"),diagnostics = TRUE)
stargazer(sls)
dep_var_name <- "Dependent variable: Rep$_t$"
var_labels <- c("Con$_t^*$")
stargazer(crob, covariate.labels = var_labels, dep.var.caption = dep_var_name)


res<-data.frame(attr(residuals(sls), "index"),as.numeric(residuals(sls)))
colnames(res)<-c("ISIN","date","res")
com$res=res$res[match(paste(com$ISIN,com$date),paste(res$ISIN,res$date))]

com <- com %>%
  group_by(ISIN) %>%
  mutate(leadres=lead(res)) %>%
  ungroup()
com$dres<-com$leadres-com$res
com <- com %>%
  group_by(ISIN) %>%
  mutate(lagdres = lag(dres)) %>%
  ungroup() 
com$ddres<-com$dres-com$lagdres

comreg<-com[com$my>=201512,]
pdata <- pdata.frame(comreg, index = c("ISIN","date"))

#Graphs res et dres
commu <- comreg %>% 
  group_by(date) %>% 
  summarise(
    comch = mean(dres, na.rm = TRUE),
    com = mean(res, na.rm = TRUE),
    comsd=sd(res, na.rm = TRUE))

commu$date<-as.Date(paste0(commu$date,"-01"), format = "%Y-%m-%d")
plot(commu$date, commu$com, type = "l", 
     xlab = "Year", ylab = "Communication score", 
     main = "")
lines(commu$date, commu$com, type = "l", col = "blue")
years <- as.numeric(format(commu$date, "%Y"))
axis_dates <- as.Date(paste0(years, "-01-01"))
axis(1, at = axis_dates, labels = years)

#Avec google trend
googletrend <- read_excel("G:/Mon Drive/Recherche/Projet greenwashing - Fanny Peter/Data/Calibration/googletrend.xlsx")
googletrend$Date <- as.Date(paste0(googletrend$Date, "-01"), format = "%Y-%m-%d")
merged_data <- merge(commu, googletrend, by.x = "date", by.y = "Date", all.x = TRUE)

par(mar = c(5, 4, 4, 4) + 0.3)
plot(merged_data$date, merged_data$com, type = "l", 
     xlab = "Year", ylab = "Communication score", 
     main = "",col="blue")
par(new = TRUE)
plot(merged_data$date, merged_data$Nb, type = "l", axes="FALSE",col = "gray", lty = 2,bty="n",xlab = "", ylab = "")
axis(4, at = pretty(range(merged_data$Nb,na.rm=TRUE)), labels = pretty(range(merged_data$Nb,na.rm=TRUE)))
mtext("Score of \"net zero\" occurences (Google)", side=4, line=3) #Worldwide finance
axis(1, at = axis_dates, labels = years)
legend("bottomleft", legend = c("Communication Score", "Google score \"net zero\""), 
       col = c("blue", "gray"), lty = c(1, 2),cex=0.95)
#

plot(commu$date, commu$comch, type = "l", 
     xlab = "Year", ylab = "Monthly communication", 
     main = "")
lines(commu$date, commu$comch, type = "l", col = "blue")
years <- as.numeric(format(commu$date, "%Y"))
axis_dates <- as.Date(paste0(years, "-01-01"))
axis(1, at = axis_dates, labels = years)
abline(h = 0, col = "black", lty = 2)

hist(comreg$dres,breaks=100000,xlim=c(-1,1))

#Régression

#Pblm Endogeneity
# model_fe <- plm(ddres ~ desge, data = pdata, model = "within",effect="twoways")
# model_fe2 <- plm(ddres ~ desge+ret, data = pdata, model = "within",effect="twoways")
# model_fe3 <- plm(ddres ~ desge+ret+dbeta, data = pdata, model = "within",effect="twoways")
# model_fe3sls <- plm(ddres ~ desgelead+ret+dbeta | desge+ret+dbeta, data = pdata, model = "within",effect="twoways")
# 
# #Deal with endogeneity: weak signif
# comreg<-com[com$my>=200901,]
# #comreg<-comreg[comreg$my<=201912,]
# pdata <- pdata.frame(comreg, index = c("ISIN","date"))
# model_fe3sls <- plm(ddres ~ desge | lagdesge, data = pdata, model = "within",effect="twoways")
# summary(model_fe3sls)
# coefs_robust3sls <- coeftest(model_fe3sls, vcov = vcovHC(model_fe3sls, type = "HC3"))
# print(coefs_robust3sls)

#Pour info: lien entre proxys et lags:
model_fe <- plm(esge ~ lagesge, data = pdata, model = "within",effect="individual")
model_pool <- plm(esge ~ lagesge, data = pdata, model = "pooling")
model_fe <- plm(esge ~ desge, data = pdata, model = "within",effect="twoways")
summary(model_fe)
coefs_robust <- coeftest(model_fe, vcov = vcovHC(model_fe, type = "HC3"))
print(coefs_robust)

model_fe <- plm(con ~ lagcon, data = pdata, model = "within",effect="twoways")
model_pool <- plm(con ~ lagcon, data = pdata, model = "pooling")
summary(model_fe)
coefs_robust <- coeftest(model_fe, vcov = vcovHC(model_fe, type = "HC3"))
print(coefs_robust)

hist(fixef(model_fe),breaks=10000)
summary(fixef(model_fe, type='level'))[,1]
summary(fixef(model_fe, type='level'))

#Significantly different from zero
pFtest(model_fe,model_pool)


# IV Regression
comreg<-com[com$my>=201512,]
pdata <- pdata.frame(comreg, index = c("ISIN","date"))
model_fe1 <- plm(dres ~ esge|lagdesge, data = pdata, model = "within",effect="individual")
model_fe2 <- plm(dres ~ esge|lagesge, data = pdata, model = "within",effect="twoways")
model_fe3 <- plm(dres ~ esge+ret+beta12 | lagesge+ret+beta12, data = pdata, model = "within",effect="twoways")
#model_fe <- plm(dres ~ esge+ret+dbeta | desge+ret+dbeta, data = pdata, model = "within",effect="twoways")
#summary(model_fe1)
cr1 <- coeftest(model_fe1, vcov = vcovHC(model_fe1, type = "HC3"))
cr2 <- coeftest(model_fe2, vcov = vcovHC(model_fe2, type = "HC3"))
cr3 <- coeftest(model_fe3, vcov = vcovHC(model_fe3, type = "HC3"))
#print(cr1)
#summary(model_fe2,vcov = vcovHC(model_fe2, type = "HC3"),diagnostics = TRUE)

stargazer(model_fe1,model_fe2,model_fe3)
dep_var_name <- "Dependent variable: $\\phi_{t}$"
var_labels <- c("$E^*_{t}$","$R_{t-1}$","$\\beta_{t-1}$")
stargazer(cr1,cr2,cr3, covariate.labels = var_labels, dep.var.caption = dep_var_name)


hist(fixef(model_fe1),breaks=1000,main="",xlab="Firm fixed effect")
mean(fixef(model_fe1))
sum(fixef(model_fe1)>0)/length(fixef(model_fe1))


#=> 1) je peux regarder les effets fixes et 2) l'effet contra-cyclique + j'ai les contrôles et je peux faire les analyses ci-dessous ; suppose que ça met du temps à être paru la note, ça marche avec lagesge comme var explic ; montrer que l'effet est atténué qd je ne corrige pas des controverses


## Reg de note sur note passée (conseil Xavier)
# 
# comreg<-com[!is.na(com$ddcom),] #Inconditionnellement à une communication
# comreg<-comreg[comreg$my>=200901,]
# 
# pdata <- pdata.frame(comreg, index = c("ISIN","date"))
# model_fe <- plm(esge ~ lagesge, data = pdata, model = "within",effect="twoways")
# summary(model_fe)
# 
# pdata <- pdata.frame(comreg, index = c("ISIN","date"))
# model_fe <- plm(ddcom ~ desge+lagdesge+ret+dbeta, data = pdata, model = "within",effect="twoways")
# summary(model_fe)
# 
# #Test avec les résidus
# pdata <- pdata.frame(comreg, index = c("ISIN","date"))
# model_fe <- plm(ddres ~ desge+lagdesge+ret+dbeta, data = pdata, model = "within",effect="twoways")
# summary(model_fe)



## Etude de dcom
#Finding 1: dcom est très souvent positive

#Nombre de titres avec dcom non nuls
# Filtrer les lignes avec dcom différent de zéro
com_filtered <- com %>% filter(dcom != 0)
# Regrouper les données par année et compter le nombre d'ISINs uniques
result <- com_filtered %>%
  group_by(year = y) %>%
  summarise(number_of_ISINs = n_distinct(ISIN))
# Afficher le résultat
print(result)

#Stats
min(com$com,na.rm=TRUE)
max(com$com,na.rm=TRUE)
summary(com$com)
summary(com_filtered$com)
hist(com$dcom,breaks=100000,xlim=c(-1,1))
summary(com$dcom)
hist(com_filtered$dcom,breaks=100000,xlim=c(-1,1))
summary(com_filtered$dcom)
length(unique(com$ISIN))

#% de données où dcom non nul
sum(com$dcom!=0,na.rm=TRUE)/sum(com$dcom,na.rm=TRUE)

#% de mois où dcom moyen positif:
mean_data_uncond <- com %>% 
  group_by(date) %>% 
  summarise(mean_dcom = mean(dcom, na.rm = TRUE))
sum(mean_data_uncond$mean_dcom>0,na.rm=TRUE)/length(mean_data_uncond$mean_dcom)

com_filtered$date<-as.Date(paste0(com_filtered$date,"-01"), format = "%Y-%m-%d")
mean_data <- com_filtered %>% 
  group_by(date) %>% 
  summarise(
    mean_dcom = mean(dcom, na.rm = TRUE),
    sd_dcom = sd(dcom, na.rm = TRUE))
sum(mean_data$mean_dcom>0)/length(mean_data$mean_dcom)

# Com moyen dans le temps
combis<-com
combis$date<-as.Date(paste0(combis$date,"-01"), format = "%Y-%m-%d")
test <- combis %>% 
  group_by(date) %>% 
  summarise(
    mean_com = mean(com, na.rm = TRUE))

plot(test$date, test$mean_com, type = "l", 
     xlab = "Year", ylab = "Reputation score", 
     main = "")
lines(test$date, test$mean_com, type = "l", col = "blue")
years <- as.numeric(format(test$date, "%Y"))
axis_dates <- as.Date(paste0(years, "-01-01"))
axis(1, at = axis_dates, labels = years)


#Graph de dcom sans les dcom=0
# Tracer le graphique avec les années en abscisses
plot(mean_data$date, mean_data$mean_dcom, type = "l", 
     xlab = "Year", ylab = "Change in reputation", 
     main = "", ylim = c(-20, 25))
# Tracer la moyenne
lines(mean_data$date, mean_data$mean_dcom, type = "l", col = "blue")
# Tracer la moyenne + 1 écart type
lines(mean_data$date, mean_data$mean_dcom + mean_data$sd_dcom, type = "l", col = "grey")
# Tracer la moyenne - 1 écart type
lines(mean_data$date, mean_data$mean_dcom - mean_data$sd_dcom, type = "l", col = "grey")
# Griser la zone entre la moyenne + 1 écart type et la moyenne - 1 écart type
polygon(
  x = c(mean_data$date, rev(mean_data$date)),
  y = c(mean_data$mean_dcom + mean_data$sd_dcom, rev(mean_data$mean_dcom - mean_data$sd_dcom)),
  col = rgb(0.8, 0.8, 0.8, 0.4),
  border = NA
)
# Obtenir les positions d'axe pour les années
years <- as.numeric(format(mean_data$date, "%Y"))
axis_dates <- as.Date(paste0(years, "-01-01"))
# Ajouter les étiquettes des années sur l'axe des x
axis(1, at = axis_dates, labels = years)
abline(h=0)

barplot(height = mean_data$mean_dcom, names.arg = years, 
        xlab = "Year", ylab = "Change in reputation", 
        col = "blue", ylim = c(-20, 25))
arrows(years, mean_data$mean_dcom - mean_data$sd_dcom, 
       years, mean_data$mean_dcom + mean_data$sd_dcom, 
       angle = 90, code = 3, length = 0.1, col = "grey")
abline(h=0)

# Calcul de corrélation entre dcom et E

listannees<-c(200901,201001,201101,201201,201301,201401,201501,201601,201701,201801,201901,202001,202101,202201)
a <- data.frame(deb = rep(NA, 14),
                pos = rep(NA, 14),
                neg = rep(NA, 14))
indic=1
for (i in 1:14){
ann<-listannees[i]
comcor<-com[!is.na(com$dcom)&!is.na(com$esge),]
comcor<-comcor[comcor$my>=ann,]

info <- comcor %>%
  group_by(ISIN) %>%
  summarize(cor=cor(dcom,esge,use="complete.obs"))

a$deb[indic]=ann
a$pos[indic]=sum(info$cor>0,na.rm=TRUE)/nrow(info[!is.na(info$cor),])
a$neg[indic]=sum(info$cor<0,na.rm=TRUE)/nrow(info[!is.na(info$cor),])
indic<-indic+1
}
a$year<-substr(a$deb,1,4)
astock<-a

red_color <- rgb(230, 70, 70, maxColorValue = 255)
green_color <- rgb(144, 202, 120, maxColorValue = 255)
ggplot(a, aes(x = factor(year))) +
  geom_bar(aes(y = 1, fill = "Positive"), stat = "identity", position = "stack") +
  geom_bar(aes(y = neg, fill = "Negative"), stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Positive" = green_color, "Negative" = red_color)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  ylim(0, 1) +
  labs(title = "",
       x = "Starting year",
       y = "% of positive and negative correlations",
       fill="") +
  theme_minimal()
  #theme(legend.position = "bottom")  



## Estimations complémentaires sur dres

# Par dates avec firm FE

comreg<-com[com$my>=201212,]
pdata <- pdata.frame(comreg, index = c("ISIN","date"))
model_fe1 <- plm(dres ~ esge|lagesge, data = pdata, model = "within",effect="individual")

pdata<-pdata[pdata$my>=201712,]
model_fe2 <- plm(dres ~ esge|lagesge, data = pdata, model = "within",effect="individual")

pdata<-pdata[pdata$my>=201912,]
model_fe3 <- plm(dres ~ esge|lagesge, data = pdata, model = "within",effect="individual")

pdata<-pdata[pdata$my>=202112,]
model_fe4 <- plm(dres ~ esge|lagesge, data = pdata, model = "within",effect="individual")


#model_fe <- plm(dres ~ esge+ret+dbeta | desge+ret+dbeta, data = pdata, model = "within",effect="twoways")
#summary(model_fe1)
cr1 <- coeftest(model_fe1, vcov = vcovHC(model_fe1, type = "HC3"))
cr2 <- coeftest(model_fe2, vcov = vcovHC(model_fe2, type = "HC3"))
cr3 <- coeftest(model_fe3, vcov = vcovHC(model_fe3, type = "HC3"))
cr4 <- coeftest(model_fe4, vcov = vcovHC(model_fe4, type = "HC3"))

# print(cr3)
# summary(model_fe2,vcov = vcovHC(model_fe2, type = "HC3"),diagnostics = TRUE)

stargazer(model_fe1,model_fe2,model_fe3,model_fe4)
dep_var_name <- "Dependent variable: $\\phi_{t}$"
var_labels <- c("$E^*_{t}$","$R_{t-1}$","$\\beta_{t-1}$")
stargazer(cr1,cr2,cr3,cr4, covariate.labels = var_labels, dep.var.caption = dep_var_name)


# Par dates avec time FE

comreg<-com[com$my>=201212,]
pdata <- pdata.frame(comreg, index = c("ISIN","date"))
model_fe1 <- plm(dres ~ esge|lagesge, data = pdata, model = "within",effect="twoways")

pdata<-pdata[pdata$my>=201712,]
model_fe2 <- plm(dres ~ esge|lagesge, data = pdata, model = "within",effect="twoways")

pdata<-pdata[pdata$my>=201912,]
model_fe3 <- plm(dres ~ esge|lagesge, data = pdata, model = "within",effect="twoways")

pdata<-pdata[pdata$my>=202112,]
model_fe4 <- plm(dres ~ esge|lagesge, data = pdata, model = "within",effect="twoways")


#model_fe <- plm(dres ~ esge+ret+dbeta | desge+ret+dbeta, data = pdata, model = "within",effect="twoways")
#summary(model_fe1)
cr1 <- coeftest(model_fe1, vcov = vcovHC(model_fe1, type = "HC3"))
cr2 <- coeftest(model_fe2, vcov = vcovHC(model_fe2, type = "HC3"))
cr3 <- coeftest(model_fe3, vcov = vcovHC(model_fe3, type = "HC3"))
cr4 <- coeftest(model_fe4, vcov = vcovHC(model_fe4, type = "HC3"))

# print(cr3)
# summary(model_fe2,vcov = vcovHC(model_fe2, type = "HC3"),diagnostics = TRUE)

stargazer(model_fe1,model_fe2,model_fe3,model_fe4)
dep_var_name <- "Dependent variable: $\\phi_{t}$"
var_labels <- c("$E^*_{t}$","$R_{t-1}$","$\\beta_{t-1}$")
stargazer(cr1,cr2,cr3,cr4, covariate.labels = var_labels, dep.var.caption = dep_var_name)


# Par dates avec time FE and controls

comreg<-com[com$my>=201212,]
pdata <- pdata.frame(comreg, index = c("ISIN","date"))
model_fe1 <- plm(dres ~ esge+ret+beta12|lagesge+ret+beta12, data = pdata, model = "within",effect="twoways")

pdata<-pdata[pdata$my>=201712,]
model_fe2 <- plm(dres ~ esge+ret+beta12|lagesge+ret+beta12, data = pdata, model = "within",effect="twoways")

pdata<-pdata[pdata$my>=201912,]
model_fe3 <- plm(dres ~ esge+ret+beta12|lagesge+ret+beta12, data = pdata, model = "within",effect="twoways")

pdata<-pdata[pdata$my>=202112,]
model_fe4 <- plm(dres ~ esge+ret+beta12|lagesge+ret+beta12, data = pdata, model = "within",effect="twoways")


#model_fe <- plm(dres ~ esge+ret+dbeta | desge+ret+dbeta, data = pdata, model = "within",effect="twoways")
#summary(model_fe1)
cr1 <- coeftest(model_fe1, vcov = vcovHC(model_fe1, type = "HC3"))
cr2 <- coeftest(model_fe2, vcov = vcovHC(model_fe2, type = "HC3"))
cr3 <- coeftest(model_fe3, vcov = vcovHC(model_fe3, type = "HC3"))
cr4 <- coeftest(model_fe4, vcov = vcovHC(model_fe4, type = "HC3"))

# print(cr3)
# summary(model_fe2,vcov = vcovHC(model_fe2, type = "HC3"),diagnostics = TRUE)

stargazer(model_fe1,model_fe2,model_fe3,model_fe4)
dep_var_name <- "Dependent variable: $\\phi_{t}$"
var_labels <- c("$E^*_{t}$","$R_{t-1}$","$\\beta_{t-1}$")
stargazer(cr1,cr2,cr3,cr4, covariate.labels = var_labels, dep.var.caption = dep_var_name)




# Différentes sous-notes environnementales

comreg<-com[com$my>=201512,]

pdata <- pdata.frame(comreg, index = c("ISIN","date"))
model_fe1 <- plm(dres ~ esge1|lagesge1, data = pdata, model = "within",effect="individual")
summary(model_fe1)
coefs_robust1 <- coeftest(model_fe1, vcov = vcovHC(model_fe1, type = "HC3"))
print(coefs_robust1)

model_fe2 <- plm(dres ~ esge2|lagesge2, data = pdata, model = "within",effect="individual")
summary(model_fe2)
coefs_robust2 <- coeftest(model_fe2, vcov = vcovHC(model_fe2, type = "HC3"))
print(coefs_robust2)

model_fe3 <- plm(dres ~ esge3|lagesge3, data = pdata, model = "within",effect="individual")
summary(model_fe3)
coefs_robust3 <- coeftest(model_fe3, vcov = vcovHC(model_fe3, type = "HC3"))
print(coefs_robust3)

model_fe4<- plm(dres ~ esge1|lagesge1, data = pdata, model = "within",effect="twoways")
coefs_robust4 <- coeftest(model_fe4, vcov = vcovHC(model_fe4, type = "HC3"))

model_fe5<- plm(dres ~ esge2|lagesge2, data = pdata, model = "within",effect="twoways")
coefs_robust5 <- coeftest(model_fe5, vcov = vcovHC(model_fe5, type = "HC3"))

model_fe6 <- plm(dres ~ esge3|lagesge3, data = pdata, model = "within",effect="twoways")
coefs_robust6 <- coeftest(model_fe6, vcov = vcovHC(model_fe6, type = "HC3"))

model_fe7<- plm(dres ~ esge1+ret+beta12|lagesge1+ret+beta12, data = pdata, model = "within",effect="twoways")
coefs_robust7 <- coeftest(model_fe7, vcov = vcovHC(model_fe7, type = "HC3"))

model_fe8<- plm(dres ~ esge2+ret+beta12|lagesge2+ret+beta12, data = pdata, model = "within",effect="twoways")
coefs_robust8 <- coeftest(model_fe8, vcov = vcovHC(model_fe8, type = "HC3"))

model_fe9 <- plm(dres ~ esge3+ret+beta12|lagesge3+ret+beta12, data = pdata, model = "within",effect="twoways")
coefs_robust9 <- coeftest(model_fe9, vcov = vcovHC(model_fe9, type = "HC3"))

stargazer(model_fe1,model_fe2,model_fe3)
dep_var_name <- "Dependent variable: $\\phi_{t}$"
var_labels <- c("$E^{Imp}_{t}$", "$E^{Res}_{t}$", "$E^{Emi}_{t}$", "$R_{t}$", "$\\beta_{t}$")
stargazer(coefs_robust1, coefs_robust2, coefs_robust3,  covariate.labels = var_labels, dep.var.caption = dep_var_name)

stargazer(model_fe4,model_fe5,model_fe6)
dep_var_name <- "Dependent variable: $\\phi_{t}$"
var_labels <- c("$E^{Imp}_{t}$", "$E^{Res}_{t}$", "$E^{Emi}_{t}$", "$R_{t}$", "$\\beta_{t}$")
stargazer(coefs_robust4, coefs_robust5, coefs_robust6,  covariate.labels = var_labels, dep.var.caption = dep_var_name)

stargazer(model_fe7,model_fe8,model_fe9)
dep_var_name <- "Dependent variable: $\\phi_{t}$"
var_labels <- c("$E^{Imp}_{t}$", "$E^{Res}_{t}$", "$E^{Emi}_{t}$", "$R_{t}$", "$\\beta_{t}$")
stargazer(coefs_robust7, coefs_robust8, coefs_robust9,  covariate.labels = var_labels, dep.var.caption = dep_var_name)

dep_var_name <- "Dependent variable: $\\phi_{t}$"
var_labels <- c("$E^{Imp}_{t}$", "$E^{Res}_{t}$", "$E^{Emi}_{t}$", "$R_{t}$", "$\\beta_{t}$")
stargazer(model_fe1,model_fe1,model_fe2,model_fe2,model_fe3,model_fe3,  covariate.labels = var_labels, dep.var.caption = dep_var_name)



# Indic Up and down
comreg<-com[com$my>=201512,]
comreg$down<-ifelse(comreg$desge<0,1,0)
comreg$up<-ifelse(comreg$desge>0,1,0)
pdata <- pdata.frame(comreg, index = c("ISIN","date"))
model_fe1 <- plm(dres ~ esge*down+esge*up|lagesge*down+lagesge*up, data = pdata, model = "within",effect="individual")
model_fe2 <- plm(dres ~ esge*down+esge*up|lagesge*down+lagesge*up, data = pdata, model = "within",effect="twoways")

summary(model_fe)
coefs_robust1 <- coeftest(model_fe, vcov = vcovHC(model_fe, type = "HC3"))
coefs_robust2 <- coeftest(model_fe2, vcov = vcovHC(model_fe2, type = "HC3"))

print(coefs_robust)
stargazer(model_fe1,model_fe2)
dep_var_name <- "Dependent variable: $\\phi_{t}$"
var_labels <- c("$E^*_{t}$", "Down","Up", "$E^*_{t}\\times Down$" , "$E^*_{t}\\times Up$")
stargazer(coefs_robust1,coefs_robust2,  covariate.labels = var_labels, dep.var.caption = dep_var_name)


# Indic |desge|>5
# hist(comreg$desge,breaks=1000)
# mean(abs(com$desge) > 1,na.rm=TRUE)
# comreg<-comreg[comreg$my>=201512,]
# comreg$indic<-ifelse(abs(comreg$desge)>3,1,0)
# pdata <- pdata.frame(comreg, index = c("ISIN","date"))
# model_fe <- plm(dres ~ desge*indic, data = pdata, model = "within",effect="individual")
# summary(model_fe)
# coefs_robust <- coeftest(model_fe, vcov = vcovHC(model_fe, type = "HC3"))
# print(coefs_robust)

# Indic controversies
# hist(comreg$con,breaks=1000)
# comreg<-com[!is.na(com$ddcom),] 
# comreg<-comreg[comreg$my>=200901,]
# comreg$indic<-ifelse(comreg$con>10,1,0)
# pdata <- pdata.frame(comreg, index = c("ISIN","date"))
# model_fe <- plm(ddcom ~ desge*indic+ret+dbeta, data = pdata, model = "within",effect="twoways")
# summary(model_fe)
# coefs_robust <- coeftest(model_fe, vcov = vcovHC(model_fe, type = "HC3"))
# print(coefs_robust)

#Par zone géographique -- les facteurs estimés par Zone n'apparaissent pas. Mais apparemment peu intéressant.
# comreg<-com[!is.na(com$ddcom),] #Inconditionnellement à une communication
# comreg<-comreg[comreg$my>=200901,]
# pdata <- pdata.frame(comreg, index = c("ISIN","date"))
# model_fe <- plm(ddcom ~ desge + ret + dbeta +factor(Zone), data = pdata, model = "within", effect = "twoways")
# summary(model_fe)
# coefs_robust <- coeftest(model_fe, vcov = vcovHC(model_fe, type = "HC3"))
# print(coefs_robust)


#Conclusions générales
#Effet significatif
#Marche pour les 3 sous-notes+
#Se renforce dans le temps.
#Ups and down ne marche pas comme souhaité: ok ups, sens inverse down

#Big move: ne marche pas quand je teste |desge|>5 (ok à 1)
#Avec indic de controverse, marche, par ex, pour com$con>10 mais je ne veux pas le faire car ddcom peut contenir de la controverse
#Par Zone géo: bug à l'affichage mais semble peu intéressant
#Pas essayé: Effet persistent à 2,3,4,5,6,12 mois. 


## Tests sur Régression

# Hausman test

comreg<-com[!is.na(com$ddcom),] 
comreg<-comreg[comreg$my>=200901,]

pdata <- pdata.frame(comreg, index = c("ISIN","date"))
model_fe <- plm(ddcom ~ desge+ret+dbeta, data = pdata, model = "within",effect="twoways")
summary(model_fe)
coefs_robust <- coeftest(model_fe, vcov = vcovHC(model_fe, type = "HC3"))
print(coefs_robust)

model_re <- plm(ddcom ~ desge+ret+dbeta, data = pdata, model = "random",effect="twoways")
summary(model_re)

hausman_test <- phtest(model_re,model_fe)
print(hausman_test)
#=> P-Value 0.13: pas de problème à utiliser un random effect model (pas nécessaire de le reporter)

# F test

model_pooling <- plm(ddcom ~ desge+ret+dbeta, data = pdata, model = "pooling",effect="twoways")
summary(model_pooling)
pFtest(model_fe, model_pooling)
#=> Pas d'apport majeur des effets fixes

# Wooldridge test

wooldridge_test <- pbgtest(model_fe)
print(wooldridge_test)
#=> P-Value faible: indique serial correlation

# Test de Breusch Pagan 

bp_fe <- bptest(model_fe)
print(bp_fe)
#P-Value faible: je rejette l'hyp d'homoscedasticité

# # Test de Honda 
# 
# honda_fe <- hbgtest(model_fe)
# print(honda_fe)

# Test de Durbin-Watson

dw_test <- pdwtest(model_fe)
print(dw_test)
#=> P-Value faible: pas de serial correlation (weird)

# Test de Wooldridge pour AR(1)

wooldridge_ar1 <- pwartest(model_fe, data=pdata)
print(wooldridge_ar1)
#=> High P-Value: Serial correlation

# Test de Breusch Pagan

bp_het <- bptest(model_fe, studentize = FALSE)
print(bp_het)
#=> P-Value faible: je rejette l'hyp d'homoscedasticité

# FEGLS

comreg<-com[!is.na(com$ddcom),] 
comreg<-comreg[comreg$my>=200901,]
pdata <- pdata.frame(comreg, index = c("ISIN","date"))
model_fegls <- pggls(ddcom ~ desge+ret+dbeta, data = pdata, model = "within", effect="individual")
summary(model_gls)
#=> OUI, still highly significant


### Calibration ----

# Calibration of lambda

freq <- com %>%
  group_by(ISIN) %>%
  summarize(freq = sum(con>0, na.rm = TRUE) / n())

mean(freq$freq)

mean(freq[freq$freq>0,]$freq)

hist(com$con,breaks=1000)

length(unique(com$ISIN))

#Calibration of A

#comreg<-com[!is.na(com$ddcom)&(com$ddcom!=0),] #Conditionnellement à une communication
comreg<-com[!is.na(com$ddcom),] #Inconditionnellement à une communication
comreg<-comreg[comreg$my>=200901,]

#Finding 1: la communication environnementale réagit au changement de note pour induire un retour à la moyenne
pdata <- pdata.frame(comreg, index = c("ISIN","date"))
#model_fe <- plm(dcom ~ desge+ret+dbeta, data = pdata, model = "within",effect="twoways")
#ret ou dret?
model_fe <- plm(ddcom ~ desge+ret+dbeta, data = pdata, model = "within",effect="twoways")
summary(model_fe)
coefs_robust <- coeftest(model_fe, vcov = vcovHC(model_fe, type = "HC3"))
print(coefs_robust)
mean(com$ddcom,na.rm=TRUE)


test <- com %>%
  group_by(ISIN) %>%
  summarize(var = sum(ddcom, na.rm = TRUE) / n())
mean(test$var)
(mean(test$var)*120)

-0.197*0.41/(mean(test$var)*120)

