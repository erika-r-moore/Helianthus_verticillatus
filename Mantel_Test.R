### Performing a Mantel Test in R
## Geographic distance vs. Genetic distance

#### Set your working directory


#### Upload your genetic distance matrix. Named it "geo_matrix".


##the package MASS that is used later masks dplyr. So, if MASS was ran prior, this detach command will allow you to run dplyr now.
#detach("package:MASS", unload=TRUE)

#install.packages("dplyr")
library(dplyr)
rownames(geo_matrix) <- geo_matrix[,1]
geo_matrix <- geo_matrix %>%
  select(-X)
geo_matrix
class(geo_matrix)


##### Upload genetic distance matrix. I generated mine from GenAlEx in Excel:
#Named the genetic distance matrix "genetic_matrix"

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
