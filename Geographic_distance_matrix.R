ReplaceLowerOrUpperTriangle <- function(m, triangle.to.replace){
  # If triangle.to.replace="lower", replaces the lower triangle of a square matrix with its upper triangle.
  # If triangle.to.replace="upper", replaces the upper triangle of a square matrix with its lower triangle.
  
  if (nrow(m) != ncol(m)) stop("Supplied matrix must be square.")
  if      (tolower(triangle.to.replace) == "lower") tri <- lower.tri(m)
  else if (tolower(triangle.to.replace) == "upper") tri <- upper.tri(m)
  else stop("triangle.to.replace must be set to 'lower' or 'upper'.")
  m[tri] <- t(m)[tri]
  return(m)
}

GeoDistanceInKMMatrix <- function(df.geopoints){
  # Returns a matrix (M) of distances between geographic points.
  # M[i,j] = M[j,i] = Distance between (df.geopoints$lat[i], df.geopoints$lon[i]) and
  # (df.geopoints$lat[j], df.geopoints$lon[j]).
  # The row and column names are given by df.geopoints$name.
  
  GeoDistanceInKM <- function(g1, g2){
    # Returns a vector of distances. (But if g1$index > g2$index, returns zero.)
    # The 1st value in the returned vector is the distance between g1[[1]] and g2[[1]].
    # The 2nd value in the returned vector is the distance between g1[[2]] and g2[[2]]. Etc.
    # Each g1[[x]] or g2[[x]] must be a list with named elements "index", "lat" and "lon".
    # E.g. g1 <- list(list("index"=1, "lat"=12.1, "lon"=10.1), list("index"=3, "lat"=12.1, "lon"=13.2))
    DistM <- function(g1, g2){
      require("Imap")
      return(ifelse(g1$index > g2$index, 0, gdist(lat.1=g1$lat, lon.1=g1$lon, lat.2=g2$lat, lon.2=g2$lon, units="km")))
    }
    return(mapply(DistM, g1, g2))
  }
  
  n.geopoints <- nrow(df.geopoints)
  
  # The index column is used to ensure we only do calculations for the upper triangle of points
  df.geopoints$index <- 1:n.geopoints
  
  # Create a list of lists
  list.geopoints <- by(df.geopoints[,c("index", "lat", "lon")], 1:n.geopoints, function(x){return(list(x))})
  
  # Get a matrix of distances (in KM)
  mat.distances <- ReplaceLowerOrUpperTriangle(outer(list.geopoints, list.geopoints, GeoDistanceInKM), "lower")
  
  # Set the row and column names
  rownames(mat.distances) <- df.geopoints$name
  colnames(mat.distances) <- df.geopoints$name
  
  return(mat.distances)
}


#install.packages("Imap")
library(Imap)

## this is where you will input your data:
df.cities<- data.frame(name=c("McNairy Co., TN", "Madison Co., TN", "Georgia", "Alabama", "Mississippi", "Virginia"),
        lat = c(35.058301, 35.491029, 34.135823, 34.134977, 34.947011, 36.92859),
        lon = c(-88.601807, -88.719902, -85.404075, -85.443063, -89.415152, -80.18808))
geo.distance.km <-round(GeoDistanceInKMMatrix(df.cities), 2)

geo.distance.km

##Creating .csv sheet:
setwd("C:/Users/Erika/OneDrive - The University of Memphis/H. vert/Data results/R_geo_distance")
write.csv(geo.distance.km, "hvert_geo_distance_matrix_KM.csv")




#### New locations from September collection

#install.packages("Imap")
library(Imap)

## this is where you will input your data:
df.cities<- data.frame(name=c("Mississippi", "South72", "River"),
                       lat = c(34.947011, 34.9457, 34.93089),
                       lon = c(-89.415152, -89.4154, -89.41313))
geo.distance.km <-round(GeoDistanceInKMMatrix(df.cities), 2)

geo.distance.km

