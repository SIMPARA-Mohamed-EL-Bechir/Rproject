#-----------Etude de cas statistique/probabiliste avec R-----------#
#------L’objectif est de faire une analyse statistique de Covid-19 au Maroc.-----#

#------------------------------1-Comprehension des donnees---------------------------------------#
#a. Etablissez des statistiques descriptives, e.g. des moyennes, des icarts-types, des valeurs minimales et maximales pour chaque variable 
monDataset<-read.csv2("open_stats_coronavirus.csv")
monDataset<-monDataset[monDataset$nom=='maroc',]
View(monDataset)

max(monDataset$cas)

round(c(summary(monDataset$cas), σ=sd(monDataset$cas, na.rm = TRUE)), 2)
round(c(summary(monDataset$deces), σ=sd(monDataset$deces, na.rm = TRUE)), 2)
round(c(summary(monDataset$guerisons), σ=sd(monDataset$guerisons, na.rm = TRUE)), 2)

#b. Etablissez des graphiques, e.g. des histogrammes ou des boites i moustaches qui montrent la distribution
library(ggplot2)

plot(monDataset$cas,monDataset$deces, col = rgb(1, 0, 0, 0.1))
plot(monDataset$cas,monDataset$guerisons, col = rgb(1, 0, 0, 0.1))
plot(monDataset$guerisons,monDataset$deces, col = rgb(1, 0, 0, 0.1))

hist(monDataset$cas,main="Histogramme de cas", xlab="nbr de cas",border="blue",col="white")
hist(monDataset$deces,main="Histogramme de deces", xlab="nbr de deces",border="blue",col="white")
hist(monDataset$guerisons,main="Histogramme de guerisons", xlab="nbr de guerisons",border="blue",col="white",breaks =6)

boxplot(monDataset$cas, xlab="cas") 
boxplot(monDataset$deces, xlab="deces")
boxplot(monDataset$guerisons, xlab="guerisons")
boxplot(monDataset$cas,monDataset$deces,monDataset$guerisons,names=c("cas","deces","guerisons"),col=c("#FF8000","#FF8000","#FF8000"))

plot(x,monDataset$cas,col="red",type="l",xlab = 'jours',ylab = 'nbr cas')
plot(x,monDataset$deces,col="red",type="l",xlab = 'jours',ylab = 'nbr deces')
plot(x,monDataset$guerisons,col="red",type="l",xlab = 'jours',ylab = 'nbr guerisons')

#------------------------------2-Examination la distribution---------------------------------------#
#a. Calculer le coefficient de correlation lineaire 
c1 <- cor(monDataset$cas,monDataset$deces,use = "complete.obs")
print(c1)
c2 <- cor(monDataset$cas,monDataset$guerisons,use = "complete.obs")
print(c2)
c3 <- cor(monDataset$deces,monDataset$guerisons,use = "complete.obs")
print(c3)

#b. Faire la regression lineaire
reg1 <-lm(monDataset$cas~monDataset$deces, data=monDataset)
plot(monDataset$cas~monDataset$deces,pch=20,data=monDataset,col= rgb(0.968, 0.494, 0.094,0.1))
reg1
abline(reg1,col=rgb(0.349, 0.670, 0.211),lwd=2)

reg2 <-lm(monDataset$cas~monDataset$guerisons, data=monDataset)
plot(monDataset$cas~monDataset$guerisons,pch=20,data=monDataset,col= rgb(0.968, 0.494, 0.094,0.1))
reg2
abline(reg2,col=rgb(0.349, 0.670, 0.211),lwd=2)

reg3 <-lm(monDataset$deces~monDataset$guerisons,data=monDataset)
plot(monDataset$deces~monDataset$guerisons,pch=20,data=monDataset,col= rgb(0.968, 0.494, 0.094,0.1))
reg3
abline(reg3,col=rgb(0.349, 0.670, 0.211),lwd=2)

#----------------------------------------3-Faire une analyse probabiliste ----------------------#




