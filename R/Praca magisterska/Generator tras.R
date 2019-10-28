#TD3 <- read.csv(paste0(getwd(),"/Odleg?o?ci/Szwecja.csv"), sep=",",header = TRUE)
TD3 <- read.csv(file.choose(),sep = ";",header = TRUE)
rownames(TD3) <- c(names(TD3),"Koszty","Wysokość n.p.m", "Opinie", "% dróg rowerowych")
TD3
TD <- TD3[1:length(names(TD3)),]
TD2 <- TD3[(nrow(TD3)-3):nrow(TD3),]
options(digits = 2)
print("Nazwy miejscowosci wpisuj w cudzyslowie !")
# https://www.wysokosciomierz.pl/#
odleglosc <- function(miejscowosc1,miejscowosc2) {
  o=TD[miejscowosc1,miejscowosc2]
  cat(paste(c("Odleglosc miedzy ",miejscowosc1," a ", miejscowosc2," wynosi ",o," km")))
}

generujTrasy <- function(ileMiejscowosci,ileTras) {
  a=matrix(0,nrow = ileTras,ncol = ileMiejscowosci)
  for (i in 1:ileTras) {
    for (j in 1:(ileMiejscowosci-1)) {
      a[i,] <- sample(rownames(TD),ileMiejscowosci,replace = F)
    }
  }
  d <<- a
  e <<- i
  f <<- j
  
  l <- matrix(0,e,f)
  for(x in 1:e) {
    for(y in 1:f) {
      l[x,y] <- TD[d[x,seq(1,(f+1))],d[x,seq(1,(f+1))]][2:(f+1),1:f][y,y]
    }
  }
  b <<- l
}
#generujTrasy(12,1000000)  # generuj(liczbaMiejscowosci,liczbaTras)
#generujTrasy(6,3000)
braki <- nrow(b[!complete.cases(b),]) # liczba wierszy z brakami, czyli błędnie wyznaczone trasy
popr <- e-nrow(b[!complete.cases(b),]) # liczba poprawnych tras
popr
(e-nrow(b[!complete.cases(b),]))/(braki+popr)*100    # procent tras poprawnych

# Które wiersze mają poprawną trasę?
na_rows <- rowSums(is.na(b))
which(na_rows==0) 

tab <- d[which(na_rows==0),] # Tworzymy tabelę z poprawnie wygenerowanymi trasami
tab <- tab[!duplicated(tab),]  # Eliminujemy powtarzające się trasy
dim(tab)[1]

# Tworzymy listę długości wszystkich tras:
a <- dim(tab)[1]
km2 <- matrix(0,a,f)
for(j in 1:a) {
  for(i in 1:f) {
    km2[j,i] <- TD[tab[j,i],tab[j,i+1]]  # sumujemy łączną długość wszystkich tras
  }
}
dlugosciTras <- rowSums(km2)

koszty <- TD2[1,]
zl <- matrix(0,a,1)
for(j in 1:a) {
    zl[j] <- sum(koszty[tab[j,1:(f+1)]])  # wyliczamy koszty poszczególnych wycieczek
}

wysokosc <- TD2[2,]

wys2 <- matrix(0,a,f)
for(j in 1:a) {
  for(i in 1:f) {
    wys2[j,i] <- sum(abs(wysokosc[tab[j,(i+1)]]-wysokosc[tab[j,i]]))  # wyliczamy srednia wysokosc
  }
  wys3 <- rowSums(wys2)
}


ciag <- matrix(0,a,f+1)
wys <- matrix(0,a,f)
for(j in 1:a) {
  for(i in 1:(f+1)) {
    ciag[j,i] <- wysokosc[,tab[j,i]]
    wys[j,] <- diff(ciag[j,])
  }
}

gora <- matrix(0,a,1)
dol <- matrix(0,a,1)
for(j in 1:a) {
  gora1 <- which(wys[j,]>0)
  dol1 <- which(wys[j,]<0)
  gora[j,] <- sum(wys[j,gora1])
  dol[j,] <- sum(wys[j,dol1])
}

opinie <- TD2[3,]
op <- matrix(0,a,1)
for(j in 1:a) {
  op[j] <- sum(opinie[tab[j,1:(f+1)]])/i  # wyliczamy srednia opinie
}

drogi <- TD2[4,]
dr <- matrix(0,a,1)
for(j in 1:a) {
  dr[j] <- sum(drogi[tab[j,1:(f+1)]])/i  # wyliczamy sredni % drog rowerowych
}

kraj <- rep("  ",dim(tab)[1])

tab <- data.frame(tab,kraj,dlugosciTras,zl,gora,dol,op,dr)
colnames(tab) <- c(1:i,"Kraj","Laczna dlugosc trasy [km]","Srednie koszty wycieczki [zl]","W gore [m]","W dol [m]","Opinie","Drogi asfaltowe [%]")

# tab[order(tab$`Laczna dlugosc trasy [km]`),]           # sortowanie wycieczek od najkrótszej do najdłuższej
# tab[order(-tab$`Srednie koszty wycieczki [zl]`),]         # sortowanie wycieczek od najdroższej do najtańszej
# tab[order(-tab$'Atrakcyjnosc'),]         # sortowanie wycieczek od najbardziej atrakcyjnej do najmniej atrakcyjnej

tab2 <- data.frame(tab$`Laczna dlugosc trasy [km]`,tab$`Srednie koszty wycieczki [zl]`,tab$`W gore [m]`,tab$`W dol [m]`,tab$Opinie,tab$`Drogi asfaltowe [%]`)
sc <- scale(tab2)
hc <- hclust(dist(sc,method = "euclidean"),method = "average")
plot(hc,hang=1)
rhc <- rect.hclust(hc,k=3) # tutaj k=x, czyli jest x grup
a <- rhc[[1]]
b <- rhc[[2]]
c <- rhc[[3]]
#d <- rhc[[4]]
#e <- rhc[[5]]


# Co chcemy grupowac:
k <- tab$`W gore [m]`
l <- tab$`W dol [m]`
plot(k, abs(l), xlab = "W gore", ylab = "W dol")
points(1:max(k,abs(l)),1:max(k,abs(l)),type = "l",col="red")   # dla wykresu z przewyzszeniami

# Metody:
# the agglomeration method to be used. This should be (an unambiguous abbreviation of)
# one of "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), 
# "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).

tt <- data.frame(k,abs(l))
hc <- hclust(dist(tt,method = "euclidean"),method = "complete")
rhc <- rect.hclust(hc,k=5) # tutaj k=x, czyli jest x grup
a <- rhc[[1]]
b <- rhc[[2]]
c <- rhc[[3]]
d <- rhc[[4]]
e <- rhc[[5]]
#f <- rhc[[6]]
plot(tt,xlab="W gore [m]",ylab="W dol [m]",main = "Trasy rowerowe w Szwajcarii (5 miejscowości)",
        sub = "Metoda complete, 5 grup")
points(1:max(k,abs(l)),1:max(k,abs(l)),type = "l",col="red")   # dla wykresu z przewyzszeniami
points(tt[a,],col="red")
points(tt[b,],col="blue")
points(tt[c,],col="green")
points(tt[d,],col="orange")
points(tt[e,],col="yellow")
#points(tt[f,],col="brown")

#write.table(tab[1:200,],"Trasy rowerowe w Niemczech, 6.csv",sep = ";",row.names = FALSE)

#which(tab[,1]=="Ystad") # chcemy wystartować z Ystad. Które wiersze to spełniają? (numer wiersza)
#which(tab[,1]=="Ystad" & tab[,2]=="Trelleborg") # chcemy wystartować z Ystad, a druga miejscowość to Trelleborg

#which(dlugosciTras==min(dlugosciTras)) # numer najkrótszej trasy (podgląd trasy - tab[which(dlugosciTras==min(dlugosciTras)),] )
#which(dlugosciTras==max(dlugosciTras)) # numer najdłuższej trasy (podgląd trasy - tab[which(dlugosciTras==max(dlugosciTras)),] )