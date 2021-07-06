
####### poppr for LD #########



setwd("C:/Users/Erika/OneDrive - The University of Memphis/H. vert/Data results/R_LD_poppr")

################## 10MS individuals ##############

### Transforming df to genind file for poppr
library(adegenet)
library(dplyr)

hvert_12MS<-read.csv("12MS_multi_LD.csv")
rownames(hvert_12MS) <- hvert_12MS[,1]
hvert_12MS<- hvert_12MS %>%
  select(-ï..) 

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


################## 2VA individuals ##############
### Transforming df to genind file for poppr
library(adegenet)
library(dplyr)

hvert_8VA<-read.csv("8VA_multi_LD.csv")
rownames(hvert_8VA) <- hvert_8VA[,1]
hvert_8VA<- hvert_8VA %>%
  select(-ï..) 

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