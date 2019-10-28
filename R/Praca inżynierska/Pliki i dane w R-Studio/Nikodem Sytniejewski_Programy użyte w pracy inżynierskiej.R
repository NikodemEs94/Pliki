library(GGally)
library(gpairs)

T <- read.table("E:/6 semestr/Seminarium dyplomowe/Pliki i dane w R-Studio/Dane_do_programu.csv", header=T, sep=';')   # należy podać ścieżkę dostępu do pliku z danymi

T2013 <- T[1:12,]
T2014 <- T[13:24,]
T2015 <- T[25:36,]
T2016 <- T[37:48,]

T2013 <- T2013[-2]
T2014 <- T2014[-2]
T2015 <- T2015[-2]
T2016 <- T2016[-2]

tDzien <- T$Temp_Dzien
tNoc <- T$Temp_Noc
opad <- T$Opady
ziemniak <- T$Cena_ziemniakow
mleko <- T$Cena_mleka

tDzien13 <- T2013$Temp_Dzien
tNoc13 <- T2013$Temp_Noc
opad13 <- T2013$Opady
ziemniak13 <- T2013$Cena_ziemniakow
mleko13 <- T2013$Cena_mleka

tDzien14 <- T2014$Temp_Dzien
tNoc14 <- T2014$Temp_Noc
opad14 <- T2014$Opady
ziemniak14 <- T2014$Cena_ziemniakow
mleko14 <- T2014$Cena_mleka

tDzien15 <- T2015$Temp_Dzien
tNoc15 <- T2015$Temp_Noc
opad15 <- T2015$Opady
ziemniak15 <- T2015$Cena_ziemniakow
mleko15 <- T2015$Cena_mleka

tDzien16 <- T2016$Temp_Dzien
tNoc16 <- T2016$Temp_Noc
opad16 <- T2016$Opady
ziemniak16 <- T2016$Cena_ziemniakow
mleko16 <- T2016$Cena_mleka

dim(T)
summary(T) # podsumowanie danych    #kwartyl górny - 3rd Qu.
pairs(T) # wyświetlenie i wstępne podsumowanie danych

gpairs(T)  # gpairs
ggpairs(T)  #GGally

# Funkcja ggpairs, która pokazuje korelację oraz funkcję regresji pomiędzy poszczególnymi cenami i pogodą
ggpairs(T2013,column = c("Cena_ziemniakow","Temp_Dzien"))
ggpairs(T2014,column = c("Cena_ziemniakow","Temp_Dzien"))
ggpairs(T2015,column = c("Cena_ziemniakow","Temp_Dzien"))
ggpairs(T2016,column = c("Cena_ziemniakow","Temp_Dzien"))

ggpairs(T2013,column = c("Cena_mleka","Temp_Dzien"))
ggpairs(T2014,column = c("Cena_mleka","Temp_Dzien"))
ggpairs(T2015,column = c("Cena_mleka","Temp_Dzien"))
ggpairs(T2016,column = c("Cena_mleka","Temp_Dzien"))

ggpairs(T2013,column = c("Cena_ziemniakow","Opady"))
ggpairs(T2014,column = c("Cena_ziemniakow","Opady"))
ggpairs(T2015,column = c("Cena_ziemniakow","Opady"))
ggpairs(T2016,column = c("Cena_ziemniakow","Opady"))

ggpairs(T2013,column = c("Cena_mleka","Opady"))
ggpairs(T2014,column = c("Cena_mleka","Opady"))
ggpairs(T2015,column = c("Cena_mleka","Opady"))
ggpairs(T2016,column = c("Cena_mleka","Opady"))

#Przykładowe wykresy pudełkowe:

boxplot(ziemniak~tDzien)
boxplot(ziemniak~tNoc)
boxplot(ziemniak~opad)
boxplot(mleko~tDzien)
boxplot(mleko~tNoc)
boxplot(mleko~opad)

#Badanie korelacji pomiędzy ceną a pogodą:

cor(ziemniak13,tDzien13)
cor(ziemniak14,tDzien14)
cor(ziemniak15,tDzien15)
cor(ziemniak16,tDzien16)

cor(mleko13,tDzien13)
cor(mleko14,tDzien14)
cor(mleko15,tDzien15)
cor(mleko16,tDzien16)

cor(ziemniak13,tNoc13)
cor(ziemniak14,tNoc14)
cor(ziemniak15,tNoc15)
cor(ziemniak16,tNoc16)

cor(mleko13,tNoc13)
cor(mleko14,tNoc14)
cor(mleko15,tNoc15)
cor(mleko16,tNoc16)

cor(ziemniak13,opad13)
cor(ziemniak14,opad14)
cor(ziemniak15,opad15)
cor(ziemniak16,opad16)

cor(mleko13,opad13)
cor(mleko14,opad14)
cor(mleko15,opad15)
cor(mleko16,opad16)

# Szukanie funkcji regresji, wpasowywanie linii w chmurę punktów:
Cena_mleka_2013 <- mleko13  # można podmieniać i wpisywać np mleko15, ziemniak14 itp
Temperatura_2013 <- tDzien13    # można podmieniać i wpisywać np tDzien14, opad16 itp
plot(Cena_mleka_2013~Temperatura_2013, pch=19, cex=1)
par(new=TRUE)
plot(Cena_mleka_2013~Temperatura_2013, type="l")

par(new=TRUE)

Cena_mleka_2013 <- mleko13  # można podmieniać i wpisywać np mleko15, ziemniak14 itp
Temperatura_2013 <- tDzien13^8    # można podmieniać i wpisywać np tDzien14, opad16 itp
plot(Cena_mleka_2013~Temperatura_2013, pch=19, cex=1,col="red")
par(new=TRUE)
plot(Cena_mleka_2013~Temperatura_2013, type="l",col="red")

par(new=TRUE)

Cena_mleka_2013 <- mleko13^16  # można podmieniać i wpisywać np mleko15, ziemniak14 itp. Można też przeskalować wpisując np mleko13^2
Temperatura_2013 <- tDzien13    # można podmieniać i wpisywać np tDzien14, opad16 itp
plot(Temperatura_2013~Cena_mleka_2013, pch=19, cex=1,col="green")
par(new=TRUE)
plot(Temperatura_2013~Cena_mleka_2013, type="l",col="green")

par(new=TRUE)

Cena_mleka_2013 <- mleko13^-2  # można podmieniać i wpisywać np mleko15, ziemniak14 itp. Można też przeskalować wpisując np mleko13^2
Temperatura_2013 <- tDzien13    # można podmieniać i wpisywać np tDzien14, opad16 itp
plot(Temperatura_2013~Cena_mleka_2013, pch=19, cex=1,col="brown")
par(new=TRUE)
plot(Temperatura_2013~Cena_mleka_2013, type="l",col="brown")

par(new=TRUE)

Cena_mleka_2013 <- mleko13^-6  # można podmieniać i wpisywać np mleko15, ziemniak14 itp. Można też przeskalować wpisując np mleko13^2
Temperatura_2013 <- tDzien13    # można podmieniać i wpisywać np tDzien14, opad16 itp
plot(Temperatura_2013~Cena_mleka_2013, pch=19, cex=1,col="blue")
par(new=TRUE)
plot(Temperatura_2013~Cena_mleka_2013, type="l",col="blue")

par(new=TRUE)

Cena_mleka_2013 <- mleko13^-10  # można podmieniać i wpisywać np mleko15, ziemniak14 itp. Można też przeskalować wpisując np mleko13^2
Temperatura_2013 <- tDzien13    # można podmieniać i wpisywać np tDzien14, opad16 itp
plot(Temperatura_2013~Cena_mleka_2013, pch=19, cex=1,col="orange")
par(new=TRUE)
plot(Temperatura_2013~Cena_mleka_2013, type="l",col="orange")

par(new=TRUE)

Cena_mleka_2013 <- mleko13^-14  # można podmieniać i wpisywać np mleko15, ziemniak14 itp. Można też przeskalować wpisując np mleko13^2
Temperatura_2013 <- tDzien13    # można podmieniać i wpisywać np tDzien14, opad16 itp
plot(Temperatura_2013~Cena_mleka_2013, pch=19, cex=1,col="yellow")
par(new=TRUE)
plot(Temperatura_2013~Cena_mleka_2013, type="l",col="yellow")

legend("topright",pch=19,c("Standard","Cena^8","Cena^16","Cena^-2","Cena^-6","Cena^-10","Cena^-14"),col=c("black","red","green","brown","blue","orange","yellow"))

#-----------------------------------------------------

Cena_mleka_2014 <- mleko14  # można podmieniać i wpisywać np mleko15, ziemniak14 itp. Można też przeskalować wpisując np mleko13^2
Temperatura_2014 <- tDzien14    # można podmieniać i wpisywać np tDzien14, opad16 itp
plot(Cena_mleka_2014~Temperatura_2014, pch=19, cex=1)
par(new=TRUE)
plot(Cena_mleka_2014~Temperatura_2014, type="l")

par(new=TRUE)

Cena_mleka_2014 <- mleko14  # można podmieniać i wpisywać np mleko15, ziemniak14 itp. Można też przeskalować wpisując np mleko13^2
Temperatura_2014 <- tDzien14^-3    # można podmieniać i wpisywać np tDzien14, opad16 itp
plot(Cena_mleka_2014~Temperatura_2014, pch=19, cex=1,col="red")
par(new=TRUE)
plot(Cena_mleka_2014~Temperatura_2014, type="l",col="red")

#------------------------------------------------------

Cena_mleka_2016 <- mleko16  # można podmieniać i wpisywać np mleko15, ziemniak14 itp. Można też przeskalować wpisując np mleko13^2
Temperatura_2016 <- tDzien16^70    # można podmieniać i wpisywać np tDzien14, opad16 itp
plot(Cena_mleka_2016~Temperatura_2016, pch=19, cex=1,col="red")
par(new=TRUE)
plot(Cena_mleka_2016~Temperatura_2016, type="l",col="red")

#-----------------------------------------------------

Cena_mleka_2015 <- mleko15  # można podmieniać i wpisywać np mleko15, ziemniak14 itp. Można też przeskalować wpisując np mleko13^2
Temperatura_2015 <- tDzien15    # można podmieniać i wpisywać np tDzien14, opad16 itp
plot(Cena_mleka_2015~Temperatura_2015, pch=19, cex=1)
par(new=TRUE)
plot(Cena_mleka_2015~Temperatura_2015, type="l")

par(new=TRUE)

Cena_mleka_2015 <- mleko15  # można podmieniać i wpisywać np mleko15, ziemniak14 itp. Można też przeskalować wpisując np mleko13^2
Temperatura_2015 <- tDzien15^-4    # można podmieniać i wpisywać np tDzien14, opad16 itp
plot(Cena_mleka_2015~Temperatura_2015, pch=19, cex=1,col="red")
par(new=TRUE)
plot(Cena_mleka_2015~Temperatura_2015, type="l",col="red")

#--------------------------------------------------

x <- opad14
y <- ziemniak14
Opady_2014 <- x
Cena_ziemniaków_2014 <- y
plot(-sin(Opady_2014)^14,Cena_ziemniaków_2014,pch=19)
#plot(x,y,pch=19)
x <- -sin(x)^14
y <- y
model <- lm(y~x)
lx <- seq(min(x)-10,max(x)+10,len=100)
ly <- predict(model,list(x=lx))
lines(lx,ly,col='red',main='Tytul')
cor(x,y)
summary(model)

x <- opad14
y <- mleko14
Opady_2014 <- x
Cena_mleka_2014 <- y
plot(-sin(Opady_2014)^8,Cena_mleka_2014,pch=19)
#plot(x,y,pch=19)
x <- -sin(x)^8
y <- y
model <- lm(y~x)
lx <- seq(min(x)-10,max(x)+10,len=100)
ly <- predict(model,list(x=lx))
lines(lx,ly,col='red',main='Tytul')
cor(x,y)
summary(model)

par(new=TRUE)
plot(-(seq(min(x),max(x),len=80))^8+max(y),type = "l",col="green",lwd=3)

# Przyda się:
plot(-(seq(min(x),max(x),len=100)))
plot(-(seq(min(x),max(x),len=100))^2)
plot(-(seq(min(x),max(x),len=100))^2+10)
plot(-(seq(min(x),max(x),len=100))^4+10)
plot(-(seq(min(x),max(x),len=80))^4+10)
plot(-(seq(min(x),max(x),len=60))^4+10)

gpairs(T2013,lower.pars=list(scatter='stats'),upper.pars = list(scatter='loess'))
