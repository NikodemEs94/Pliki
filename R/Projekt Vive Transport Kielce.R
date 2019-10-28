library(dplyr) 
library(ggplot2)
library(animation)
library(clusterSim)
library(readr)
library(tibble)

TD <- read.csv(file.choose(),sep = ";",header = TRUE)
options(OutDec = ".")
marzec_czynnosci <- read.csv(file.choose(),sep = ";", dec = ".", header = TRUE)
kwiecien_czynnosci <- read.csv(file.choose(),sep = ";", dec = ".", header = TRUE)
marzec_tacho <- read.csv(file.choose(),sep = ";",header = TRUE)
kwiecien_tacho <- read.csv(file.choose(),sep = ";",header = TRUE)
zlecenia <- read.csv(file.choose(),sep = ";",dec=".",header = TRUE)

czynnosci <- rbind(marzec_czynnosci,kwiecien_czynnosci)

View(czynnosci)
View(zlecenia)

czynnosci <- czynnosci %>%
  filter(czynnosci$Kierowca!=unique(czynnosci$Kierowca)[2]) %>%
  filter(CzynnoćŤ=="Jazda")

jazda <- czynnosci %>%
  filter(Czynno??=="Jazda")  #%>%
#  filter(Trasa==unique(czynnosci$Trasa)[1]) %>%
 # arrange(Data.i.godzina.rozpocz.cia)
jazda

#trasy_marzec <- unique(marzec_czynnosci$Trasa)
#trasy_kwiecien <- unique(kwiecien_czynnosci$Trasa)


zlecenia <- zlecenia %>% filter(NR_ZLECENIA %in% unique(czynnosci$Trasa)) 

tab <- data.frame(NR_ZLECENIA=zlecenia$NR_ZLECENIA,ZA_MIASTO=zlecenia$ZA_MIASTO,WY_MIASTO=zlecenia$WY_MIASTO,TRASA_ODL_DOJAZD=zlecenia$TRASA_ODL_DOJAZD,TRASA_ODL=zlecenia$TRASA_ODL,TRASA_ODL_GL=zlecenia$TRASA_ODL_GL,WAGA_LADUNKU_RZ=zlecenia$WAGA_LADUNKU_RZ)
tab <- tab %>%
  order_by(NR_ZLECENIA)


tab2 <- data.frame(NR_ZLECENIA=czynnosci$Trasa, Kierowca1=czynnosci$Kierowca, Kierowca2=czynnosci$Drugi.kierowca, Dystans=czynnosci$Dystans..km. ,Laczna_odleglosc=czynnosci$ü.czna.odleg.oćŤ, Dojazd=czynnosci$ü.czna.odleg.oćŤ-czynnosci$Dystans..km.,Spalanie_litry=as.numeric(chartr(",",".",  czynnosci$litry)),Spalanie_100_km=czynnosci$l.100.km)
colSums(na.omit(tab2))

tab2 <- tab2 %>%
  filter(Spalanie_litry!=0) %>%
  filter(Spalanie_100_km!="0,00")













View(marzec_czynnosci)
View(zlecenia)


marzec_cz <- marzec_czynnosci[which(marzec_czynnosci$Kod.pracy=="Jazda"),] 
kwiecien_cz <- kwiecien_czynnosci[which(kwiecien_czynnosci$Kod.pracy=="Jazda"),] 

require(maps)
map("world", regions = c("Poland","Germany","Czech Republic","Norway"))

#######   MARZEC    #############

likw <- marzec_czynnosci %>% 
  filter(Kierowca!=unique(marzec_czynnosci$Kierowca)[1])
  
jazda <- marzec_czynnosci %>%
  filter(CzynnoćŤ=="Jazda") %>%
  filter(Kierowca!=unique(marzec_czynnosci$Kierowca)[2]) %>%
  select(Trasa, Dystans..km.) 

tabela <- matrix(0,length(unique(marzec_czynnosci$Trasa)),2)
tabela <- data.frame(0,0)

for (i in 1:length(unique(marzec_czynnosci$Trasa))) {
  
  trasa <- likw %>% 
    filter(Kod.pracy=="Jazda") %>%
    filter(Trasa==unique(likw$Trasa)[i]) %>%

    select(Trasa, Dystans..km.) 
  
  tabela[i,1] <- as.character(unique(likw$Trasa)[i])
  tabela[i,2] <- sum(trasa$Dystans..km.)
  
}
colnames(tabela) <- c("NR_ZLECENIA","Laczna dlugosc")
View(tabela)


jakasTrasa <- marzec_czynnosci %>%
  filter(marzec_czynnosci$Trasa=="T/226/11/03/2015")


#######   KWIECIEN    #############


likw2 <- kwiecien_czynnosci %>% 
  filter(Kierowca!=unique(kwiecien_czynnosci$Kierowca)[1])

jazda2 <- kwiecien_czynnosci %>%
  filter(Czynno??=="Jazda") %>%
  filter(Kierowca!=unique(kwiecien_czynnosci$Kierowca)[2]) %>%
  select(Trasa, Dystans..km.) 

tabela2 <- matrix(0,length(unique(kwiecien_czynnosci$Trasa)),2)
tabela2 <- data.frame(0,0)


for (i in 1:length(unique(kwiecien_czynnosci$Trasa))) {
  
  trasa2 <- likw2 %>% 
    filter(Kod.pracy=="Jazda") %>%
    filter(Trasa==unique(likw2$Trasa)[i]) %>%
   
    select(Trasa, Dystans..km.) 
  
  tabela2[i,1] <- as.character(unique(likw2$Trasa)[i])
  tabela2[i,2] <- sum(trasa2$Dystans..km.)
  #tabela2[i,3] <- jazda2 %>% filter(jazda2$Trasa==tabela2[i,1]) %>% nrow()
}
colnames(tabela2) <- c("NR_ZLECENIA","Laczna dlugosc")
View(tabela2)
################################

km <- rbind(tabela,tabela2)
km <- km %>%
  filter(`Laczna dlugosc`!=0) %>%
  group_by(NR_ZLECENIA) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
  
trasyDobre <- km[which(km$n==1),]
trasyPodwojne <- km[which(km$n==2),]
trasyPodwojne$NR_ZLECENIA

a <- matrix(0,length(trasyPodwojne$NR_ZLECENIA),1)

for(i in 1:length(trasyPodwojne$NR_ZLECENIA)) {
  tttt <- czynnosci %>% filter(Trasa==trasyPodwojne$NR_ZLECENIA[i])
  a[i] <- sum(tttt$Dystans..km.)
}

tabela3 <- data.frame(trasyPodwojne$NR_ZLECENIA,a)
colnames(tabela3) <- c("NR_ZLECENIA","Laczna dlugosc")

km <- rbind(tabela,tabela2)
km <- km %>%
  filter(NR_ZLECENIA %in% trasyDobre$NR_ZLECENIA) %>%
  filter(NR_ZLECENIA!=0)
colnames(km) <- c("NR_ZLECENIA","Laczna dlugosc")


tabela4 <- rbind(km,tabela3)
colnames(tabela4) <- c("NR_ZLECENIA","Laczna dlugosc")
tabela4 <- tabela4[which(nchar(as.character(tabela4$NR_ZLECENIA))>14),]
View(tabela4)

merge()  # łączenie dwóch  tabel. Nazwy kolumn muszą byc takie same
tab <- merge(tabela4,tab,by="NR_ZLECENIA")


tabela4 <- data.frame(trasyDobre$NR_ZLECENIA,km$`Laczna dlugosc`)

dim(tab[!duplicated(tab),])


tt <- czynnosci %>% filter(Trasa=="T/579/08/02/2015")
sum(tt$Dystans..km.)

t2 <- czynnosci %>%
  group_by(Trasa)

tt <- zlecenia %>% filter(NR_ZLECENIA=="T/579/08/02/2015")
sum(tt$Dystans..km.)

zespoly <- paste(czynnosci$Kierowca,",",czynnosci$Drugi.kierowca)

#0.75 <- km_zmierzone / zlecenia$TRASA_ODL

czynnosci$Dystans..km. / zlecenia$TRASA_ODL

kmZmierzone <- data.frame(czynnosci$Trasa,czynnosci$Dystans..km.)
kmZlecenia <- data.frame(NR_ZLECENIA=zlecenia$NR_ZLECENIA,as.numeric(zlecenia$TRASA_ODL))
################################################



sprKM <- merge(tabela4,kmZlecenia,by="NR_ZLECENIA")

sprKM$zlecenia.TRASA_ODL / sprKM$`Laczna dlugosc`

data.frame(sprKM,as.numeric(sprKM$zlecenia.TRASA_ODL) / as.numeric(sprKM$`Laczna dlugosc`))

tb <- sprKM[which(sprKM$NR_ZLECENIA=="T/264/10/03/2015"),]

as.numeric(tb$zlecenia.TRASA_ODL)/as.numeric(tb$`Laczna dlugosc`)

tab3 <- merge(tab,tab2,by="NR_ZLECENIA")


#######   WSZYSTKO    #############

likw3 <- rbind(likw,likw2)

tabela5 <- matrix(0,length(unique(tab2$NR_ZLECENIA)),5)
#tabela5 <- data.frame(0,0)


for (i in 1:length(unique(tab2$NR_ZLECENIA))) {
  
  trasa2 <- tab2 %>% 
    filter(NR_ZLECENIA==unique(tab2$NR_ZLECENIA)[i])
  
  tabela5[i,1] <- as.character(unique(tab2$NR_ZLECENIA)[i])
  tabela5[i,2] <- sum(trasa2$Dystans)
  tabela5[i,3] <- sum(trasa2$Spalanie_litry)
  tabela5[i,4] <- sum(as.numeric(chartr(",",".",trasa2$Spalanie_100_km)))/nrow(trasa2)
  tabela5[i,5] <- 
}
colnames(tabela5) <- c("NR_ZLECENIA","Laczna_dlugosc","Spalanie_litry","Spalanie_100_km","Spalanie_100_kg")
View(tabela5)

zleceniaGotowe <- merge(tab,tabela5,by="NR_ZLECENIA")
 
zleceniaGotowe <- na.omit(zleceniaGotowe)

zleceniaGotowe[,4] <- as.numeric(chartr(",",".",zleceniaGotowe[,4]))
zleceniaGotowe[,5] <- as.numeric(chartr(",",".",zleceniaGotowe[,5]))
zleceniaGotowe[,6] <- as.numeric(chartr(",",".",zleceniaGotowe[,6]))

zleceniaGotowe[,8] <- as.numeric(chartr(",",".",zleceniaGotowe[,8]))
zleceniaGotowe[,9] <- as.numeric(chartr(",",".",zleceniaGotowe[,9]))
zleceniaGotowe[,10] <- as.numeric(chartr(",",".",zleceniaGotowe[,10]))

zleceniaGotowe <- zleceniaGotowe %>%
  filter(zleceniaGotowe$Laczna_dlugosc/zleceniaGotowe$TRASA_ODL>0.75)

ilosciowe <- data.frame(zleceniaGotowe[,4:10])

#s <- scale(ilosciowe)
s <- data.Normalization(ilosciowe,type = "n5",normalization = "column")
#s <- ilosciowe
hc <- hclust(dist(s,method = "euclidean"),method = "median")  # metody: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid" 
plot(hc,hang=1)
rhc <- rect.hclust(hc,k=15)
length(rhc[[1]])
length(rhc[[2]])
length(rhc[[3]])
length(rhc[[4]])
length(rhc[[5]])
length(rhc[[6]])
length(rhc[[7]])
length(rhc[[8]])
length(rhc[[9]])
length(rhc[[10]])
length(rhc[[11]])
length(rhc[[12]])
length(rhc[[13]])
length(rhc[[14]])
length(rhc[[15]])
length(rhc[[16]])
length(rhc[[17]])
length(rhc[[18]])
length(rhc[[19]])
length(rhc[[20]])



grupa1 <- rhc[[1]]
grupa2 <- rhc[[2]]
grupa3 <- rhc[[3]]
grupa4 <- rhc[[4]]
grupa5 <- rhc[[5]]
grupa6 <- rhc[[6]]
grupa7 <- rhc[[7]]
grupa8 <- rhc[[8]]
grupa9 <- rhc[[9]]
grupa10 <- rhc[[10]]
grupa11 <- rhc[[11]]
grupa12 <- rhc[[12]]
grupa13 <- rhc[[13]]
grupa14 <- rhc[[14]]
grupa15 <- rhc[[15]]
grupa16 <- rhc[[16]]
grupa17 <- rhc[[17]]
grupa18 <- rhc[[18]]
grupa19 <- rhc[[19]]
grupa20 <- rhc[[20]]

summary(zleceniaGotowe[grupa7,c(1,4:10)])

zleceniaGotowe[grupa1,]




# brakuj?ce dane
winedata <- as.tibble(zleceniaGotowe)
winedata[is.na(winedata)] <- 0
col_index <- ncol(winedata)
winedata_t <- t(winedata[,8:col_index])
winedata_t <- as.tibble(winedata_t)
# etykieta wiersza poprzez nazwisko klienta
row.names(winedata_t) <- colnames(winedata[,8:col_index])
# grupowanie danych
winedata_cluster <- kmeans(winedata_t,5)
str(winedata_cluster)
library(factoextra)
fviz_cluster(winedata_cluster, data = winedata_t)
# zsumowanie ofert w grupach
aggregate(winedata_t, by = list(winedata_cluster$cluster), sum)
winedata_clusterCount <- t(aggregate(winedata_t, by = list(winedata_cluster$cluster), sum)[,2:33])
winedata_clusterCount
rowSums(t(winedata_clusterCount))
winedata_group <- data.frame(winedata[,1:7],winedata_clusterCount)

# sortowanie danych wzgl?dem 1 grupy
sort_group1 <- winedata_group[order(winedata_group[,8], decreasing = TRUE),]
View(sort_group1)

colSums(sort_group1[8:12])

df <- winedata_t

k2 <- kmeans(df,centers = 2,nstart = 25)
k3 <- kmeans(df,centers = 3,nstart = 25)
k4 <- kmeans(df,centers = 4,nstart = 25)
k5 <- kmeans(df,centers = 5,nstart = 25)

p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point", data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point", data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point", data = df) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow=2)

library(cluster)
plot(pam(df,5))

























TD$TRASA_ODL <- as.numeric(chartr(",",".",TD$TRASA_ODL))

table(is.na(TD$TRASA_ODL))
TD[which(is.na(TD$TRASA_ODL)==TRUE),]

TD$TRASA_ODL_DOJAZD <- as.numeric(chartr(",",".",TD$TRASA_ODL_DOJAZD))
table(is.na(TD$TRASA_ODL_DOJAZD))

TD$TRASA_ODL_GL <- as.numeric(chartr(",",".",TD$TRASA_ODL_GL))
table(is.na(TD$TRASA_ODL_GL))

TD$Km.Pocz.tek <- as.numeric(chartr(",",".",TD$Km.Pocz.tek))

kmPoczatek <- TD[which(is.na(TD$Km.Pocz.tek)==FALSE),]$Km.Pocz.tek
kmKoniec <- TD[which(is.na(TD$Km.Koniec)==FALSE),]$Km.Koniec

# -----------------------------------------
TD <- TD[which(TD$Pojazd=="BI 0079K"),]
TD$D.ugo??.geograficzna <- as.numeric(chartr(",",".",TD$D.ugo??.geograficzna))
TD$Szeroko??.geograficzna <- as.numeric(chartr(",",".",TD$Szeroko??.geograficzna))

dlugosc <- TD[which(is.na(TD$D.ugo??.geograficzna)==FALSE),]$D.ugo??.geograficzna
szerokosc <- TD[which(is.na(TD$Szeroko??.geograficzna)==FALSE),]$Szeroko??.geograficzna
TD <- TD[-c(which(szerokosc==0)),]

plot(dlugosc,szerokosc,type = "l")

#TD <- TD[-nrow(TD),]
dim(TD)


#p <- plot(TD2$Latitude,TD2$Longitude,type = "l")

saveGIF({
  
  ani.options(interval = 0.01, nmax = 50)
  
  #t = seq(0,pi,.01)
  
  x = dlugosc[1:length(dlugosc)]
  
  y = szerokosc[1:length(szerokosc)]
  
  idx = seq(1/4,length(x),5/4)
  
  for (i in seq_along(idx)) {
    
    plot(x,y,type="l",col="white")
    
    points(x[seq(idx[i])],
           
           y[seq(idx[i])], pch=16,cex=1.3, col="red",type = "l")
    
    ani.pause() }
  
}, movie.name = "trasa.gif",
ani.width = 600, ani.height = 600)




#---------------------------------------------
# t = seq(0,1.1*pi,.01)
# 
# 
# x = cos(2*t) * sin(2*t)
# 
# 
# y = tan(2*t) * cos(2*t)
# 
# 
# while (i <= length(x)) {
#   
#   
#   frame = 100 + i
#   
#   
#   filename <- paste("test", frame, ".png", sep="")
#   
#   
#   png(file=filename, width=550, height=550)
#   
#   
#   plot(x,y,type='n')
#   
#   
#   points(x[1:i], y[1:i], pch=15,col='dark green')
#   
#   
#   i <- i+1
#   
#   dev.off() }
# 
# 
# system("convert -delay 40 *.png example_1.gif")



# nak?ad na map? ?wiata:

library(ggmap)
library(get_map)

#plot the roads Google Maps basemap
map <- qmap('Liverpool', zoom = 12, maptype = 'roadmap')
#plot the density map
map + stat_density2d(
  aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..*2), 
  size = 2, bins = 5, data = crimes, geom = "polygon") +
  scale_fill_gradient(low = "black", high = "red")

