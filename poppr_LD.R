####### Using poppr to measure multi-locus LD #########

## Set your Working directory

## upload your allelic data for each population. 

### The first population I ran was MS, so I named my file "hvert_12MS"

### Transforming data frame to genind file for poppr
library(adegenet)
library(dplyr)
obj2<-df2genind(hvert_12MS, ploidy = 2, sep = "/")
obj2



#### Finding ia ####
## ia() calculates the index of association over all loci in the data set
#install.packages("poppr")
library(poppr)
ia<- ia(gid = obj2, sample = 999, quiet = TRUE, valuereturn = TRUE)
ia


# Pairwise over all loci
res<- pair.ia(obj2, sample = 999)

plot(res, index = "Ia")
plot(res, index = "rbarD")


### poppr analysis
pop_12MS<- poppr(dat=obj2, total = TRUE, sample = 999, plot = TRUE)

#Values are same, histograms are different:
pop_ia<- poppr(dat=obj2, total = TRUE, sample = 1000, index = "Ia", plot = TRUE)
pop_rbarD<- poppr(dat=obj2, total = TRUE, sample = 1000, index = "rbarD", plot = TRUE)




### Now I will run my VA population. Named that file "hvert_8VA"
### Transforming df to genind file for poppr
library(adegenet)
library(dplyr)

obj3<-df2genind(hvert_8VA, ploidy = 2, sep = "/")
obj3

#### Finding ia ####
## ia() calculates the index of association over all loci in the data set
#install.packages("poppr")
library(poppr)
ia<- ia(gid = obj3, sample = 999, quiet = TRUE, valuereturn = TRUE)
ia

# Pairwise over all loci
res<- pair.ia(obj3, sample = 999)

plot(res, index = "Ia")
plot(res, index = "rbarD")


### poppr analysis
pop_8VA<- poppr(dat=obj3, total = TRUE, sample = 999, plot = TRUE)

#Values are same, histograms are different:
pop_ia<- poppr(dat=obj2, total = TRUE, sample = 1000, index = "Ia", plot = TRUE)
pop_rbarD<- poppr(dat=obj2, total = TRUE, sample = 1000, index = "rbarD", plot = TRUE)
