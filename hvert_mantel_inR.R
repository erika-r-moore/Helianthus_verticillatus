### Mantel Test in Hvert
## Geographic distance vs. Genetic distance

#setwd("C:/Users/Erika/OneDrive - The University of Memphis/H. vert/Data results/R_geo_distance")
setwd("C:/Users/Erika/OneDrive - The University of Memphis/H. vert/Data results/R_Mantel_Test")


#Genetic distance matrix generated with R:
geo_matrix<-read.csv("hvert_geo_distance_matrix_KM.csv")

##the package MASS that is used later masks dplyr. So, if MASS was ran prior, this detach command will allow you to run dplyr now.
#detach("package:MASS", unload=TRUE)

library(dplyr)
rownames(geo_matrix) <- geo_matrix[,1]
geo_matrix <- geo_matrix %>%
  select(-X)
geo_matrix
class(geo_matrix)


#Genetic distance matrix generated from GenAlEx:
genetic_matrix<-read.csv("hvert_genetic_distance_matrix.csv")
rownames(genetic_matrix) <- genetic_matrix[,1]
genetic_matrix<- genetic_matrix %>%
  select(-ï..)
genetic_matrix
class(genetic_matrix)


##Now that we have the distance matrices, it is time to do the Mantel test which looks for isolation by distance

#install.packages("vegan")
#install.packages("geosphere")
library(vegan)
library(geosphere)

geo_vs_gen<- mantel(geo_matrix, genetic_matrix, method = "spearman", permutations = 999, na.rm = TRUE)
geo_vs_gen

##IBD
#install.packages("adegenet")
library(adegenet)
geo<-data.matrix(geo_matrix)
gen<-data.matrix(genetic_matrix)

geo_m<- as.dist(geo)
geo_m
class(geo_m)

gen_m<- as.dist(gen)
gen_m
class(gen_m)


ibd<- mantel.rtest(geo_m, gen_m, nrepet = 999)
ibd
plot(ibd)


## Better plot of IBD:
library(MASS)
dens <- kde2d(geo_m, gen_m, n=300)
myPal <- colorRampPalette(c("white","blue","gold", "orange", "red"))
plot(geo_m, gen_m, pch=20,cex=.5)
image(dens, col=transp(myPal(300),.7), add=TRUE)
abline(lm(gen_m~geo_m))
title("Isolation by distance plot")