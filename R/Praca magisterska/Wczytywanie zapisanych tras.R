library(clusterSim)

TD <- read.csv(file.choose(),sep = ",",header = TRUE)
TD$Opinie <- as.numeric(chartr(",",".",TD$Opinie))
TD$Drogi.asfaltowe.... <- as.numeric(chartr(",",".",TD$Drogi.asfaltowe....))

colnames(TD) <- c(1:6,"Kraj","Laczna dlugosc trasy [km]","Srednie koszty wycieczki [zl]","W gore [m]","W dol [m]","Opinie","Drogi asfaltowe [%]")
tab <- TD[sample(1:500,500),]

tab[1,]
dim(tab)
#write.table(TD[c(1:100,201:300,401:500,601:700,801:900),],"Trasy Rowerowe oficjalne.csv",sep = ",",row.names = FALSE)
tab <- tab[which(tab$`Srednie koszty wycieczki [zl]`<mean(tab$`Srednie koszty wycieczki [zl]`)),]
ilosciowe <- data.frame(tab$`Laczna dlugosc trasy [km]`,tab$`Srednie koszty wycieczki [zl]`,tab$`W gore [m]`,tab$`W dol [m]`,tab$Opinie,tab$`Drogi asfaltowe [%]`)
#s <- scale(ilosciowe)
s <- data.Normalization(ilosciowe,type = "n4",normalization = "column")
#s <- ilosciowe
hc <- hclust(dist(s,method = "euclidean"),method = "ward.D")  # metody: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid" 
plot(hc,hang=1)
rhc <- rect.hclust(hc,k=20)
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

table(tab[grupa1,]$Kraj)

summary(tab[grupa5,7:13])

kol <- tab$`Srednie koszty wycieczki [zl]`
mean(kol[grupa1])
mean(kol[grupa2])
mean(kol[grupa3])
mean(kol[grupa4])
mean(kol[grupa5])
mean(kol[grupa6])
mean(kol[grupa7])
mean(kol[grupa8])
mean(kol[grupa9])
mean(kol[grupa10])
mean(kol[grupa11])
mean(kol[grupa12])
mean(kol[grupa13])
mean(kol[grupa14])
mean(kol[grupa15])
mean(kol[grupa16])
mean(kol[grupa17])
mean(kol[grupa18])
mean(kol[grupa19])
mean(kol[grupa20])

tab[grupa5,]

#srednie <- kmeans(s,10,20)
#table(srednie$cluster)
#srednie$centers  # jak jest na minusie to znaczy, że jest poniżej średniej, a na plusie - powyżej średniej

write.table(tab[grupa1,],"Grupa 1.csv",sep = ",",row.names = FALSE)
write.table(tab[grupa2,],"Grupa 2.csv",sep = ",",row.names = FALSE)
write.table(tab[grupa3,],"Grupa 3.csv",sep = ",",row.names = FALSE)
write.table(tab[grupa4,],"Grupa 4.csv",sep = ",",row.names = FALSE)
write.table(tab[grupa5,],"Grupa 5.csv",sep = ",",row.names = FALSE)
write.table(tab[grupa6,],"Grupa 6.csv",sep = ",",row.names = FALSE)
write.table(tab[grupa7,],"Grupa 7.csv",sep = ",",row.names = FALSE)
write.table(tab[grupa8,],"Grupa 8.csv",sep = ",",row.names = FALSE)
write.table(tab[grupa9,],"Grupa 9.csv",sep = ",",row.names = FALSE)
write.table(tab[grupa10,],"Grupa 10.csv",sep = ",",row.names = FALSE)
write.table(tab[grupa11,],"Grupa 11.csv",sep = ",",row.names = FALSE)
write.table(tab[grupa12,],"Grupa 12.csv",sep = ",",row.names = FALSE)
write.table(tab[grupa13,],"Grupa 13.csv",sep = ",",row.names = FALSE)
write.table(tab[grupa14,],"Grupa 14.csv",sep = ",",row.names = FALSE)
write.table(tab[grupa15,],"Grupa 15.csv",sep = ",",row.names = FALSE)
write.table(tab[grupa16,],"Grupa 16.csv",sep = ",",row.names = FALSE)
write.table(tab[grupa17,],"Grupa 17.csv",sep = ",",row.names = FALSE)
write.table(tab[grupa18,],"Grupa 18.csv",sep = ",",row.names = FALSE)
write.table(tab[grupa19,],"Grupa 19.csv",sep = ",",row.names = FALSE)
write.table(tab[grupa20,],"Grupa 20.csv",sep = ",",row.names = FALSE)

tab1 <- tab[grupa1,]  
ilosciowe1 <- data.frame(tab1$`Laczna dlugosc trasy [km]`,tab1$`Srednie koszty wycieczki [zl]`,tab1$`W gore [m]`,tab1$`W dol [m]`,tab1$Opinie,tab1$`Drogi asfaltowe [%]`)
#s1 <- scale(ilosciowe1)
s1 <- data.Normalization(ilosciowe1,type = "n9",normalization = "column")
#s1 <- ilosciowe
hc1 <- hclust(dist(s1,method = "euclidean"),method = "average")  # metody: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid" 
plot(hc1,hang=1)
rhc1 <- rect.hclust(hc1,k=10)

length(rhc1[[1]])
length(rhc1[[2]])
length(rhc1[[3]])
length(rhc1[[4]])
length(rhc1[[5]])
length(rhc1[[6]])
length(rhc1[[7]])
length(rhc1[[8]])
length(rhc1[[9]])
length(rhc1[[10]])
grupa11 <- rhc1[[1]]
grupa12 <- rhc1[[2]]
grupa13 <- rhc1[[3]]
grupa14 <- rhc1[[4]]
grupa15 <- rhc1[[5]]
grupa16 <- rhc1[[6]]
grupa17 <- rhc1[[7]]
grupa18 <- rhc1[[8]]
grupa19 <- rhc1[[9]]
grupa110 <- rhc1[[10]]

table(tab1[grupa11,]$Kraj)

summary(tab1[grupa110,7:13])

kol <- tab1$`W gore [m]`
mean(kol[grupa11])
mean(kol[grupa12])
mean(kol[grupa13])
mean(kol[grupa14])
mean(kol[grupa15])

mean(kol[grupa16])
mean(kol[grupa17])
mean(kol[grupa18])
mean(kol[grupa19])
mean(kol[grupa110])




kol <- tab1$`W dol [m]`
mean(kol[grupa11])
mean(kol[grupa12])
mean(kol[grupa13])
mean(kol[grupa14])
mean(kol[grupa15])

mean(kol[grupa16])
mean(kol[grupa17])
mean(kol[grupa18])
mean(kol[grupa19])
mean(kol[grupa110])

tab1[grupa13,]

write.table(tab1[grupa11,],"Grupa 11.csv",sep = ",",row.names = FALSE)
write.table(tab1[grupa12,],"Grupa 12.csv",sep = ",",row.names = FALSE)
write.table(tab1[grupa13,],"Grupa 13.csv",sep = ",",row.names = FALSE)
write.table(tab1[grupa14,],"Grupa 14.csv",sep = ",",row.names = FALSE)
write.table(tab1[grupa15,],"Grupa 15.csv",sep = ",",row.names = FALSE)
write.table(tab1[grupa16,],"Grupa 16.csv",sep = ",",row.names = FALSE)
write.table(tab1[grupa17,],"Grupa 17.csv",sep = ",",row.names = FALSE)
write.table(tab1[grupa18,],"Grupa 18.csv",sep = ",",row.names = FALSE)
write.table(tab1[grupa19,],"Grupa 19.csv",sep = ",",row.names = FALSE)
write.table(tab1[grupa110,],"Grupa 110.csv",sep = ",",row.names = FALSE)








# Decydent: student. Chce tanio i mieć dużo podjazdów.
student <- data.frame(tab$`Srednie koszty wycieczki [zl]`,tab$`W dol [m]`,tab$`W gore [m]`)

s <- scale(student)
hc <- hclust(dist(s,method = "euclidean"),method = "complete")  # metody: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid" 
plot(hc,hang=1)
rhc <- rect.hclust(hc,k=10)
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

kol <- tab$`Srednie koszty wycieczki [zl]`
mean(kol[grupa1])
mean(kol[grupa2])
mean(kol[grupa3])
mean(kol[grupa4])
mean(kol[grupa5])
mean(kol[grupa6])
mean(kol[grupa7])
mean(kol[grupa8])
mean(kol[grupa9])
mean(kol[grupa10])



#write.table(tab[which(tab$`W gore [m]`>abs(tab$`W dol [m]`) & tab$`W gore [m]`>500 & tab$`Srednie koszty wycieczki [zl]`<mean(tab$`Srednie koszty wycieczki [zl]`)),],"Trasy dla studenta.csv",sep = ",",row.names = FALSE)


student2 <- which(tab$`W gore [m]`>abs(tab$`W dol [m]`) & tab$`W gore [m]`>500)
hist(tab[student2,]$`Srednie koszty wycieczki [zl]`)

turysta <- which(tab$`Srednie koszty wycieczki [zl]`<mean(tab$`Srednie koszty wycieczki [zl]`) & tab$Opinie >= 8)
write.table(tab[turysta,],"Trasy dla turystów.csv",sep = ",",row.names = FALSE)

emeryt <- which(tab$`W gore [m]` < 100 & abs(tab$`W dol [m]`) < 100 & tab$`Laczna dlugosc trasy [km]` < 100)
write.table(tab[emeryt,],"Trasy dla emerytów.csv",sep = ",",row.names = FALSE)
