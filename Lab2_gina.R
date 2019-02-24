## ------------------------------------------------------------------------
library(sf)
library(ggspatial)
library(tidyverse)
library(purrr)

getwd()

#test
#read_sf("data/ME-GIS-master/Coastline2.shp")

## ------------------------------------------------------------------------
p <- ggplot() +
  geom_sf(data = read_sf("data/ME-GIS-master/Coastline2.shp"),
          colour="grey10", fill="grey90") +
  geom_sf(data = read_sf("data/ME-GIS-master/Rivers19.shp"),
          colour="steelblue", size=0.3) +
  geom_sf(data = read_sf("data/ME-GIS-master/PrimaryRoads.shp"),
          size = 0.7, colour="grey30") +
  geom_sf(data = read_sf("data/ME-GIS-master/Cities.shp")) +
  theme_bw()

p

## ------------------------------------------------------------------------
# add cities, orientation w/annotation_scale and annotation_north_arrow

p + 
  geom_sf_text(data = read_sf("data/ME-GIS-master/Cities.shp"), 
               aes(label = Name)) + 
  annotation_scale() + 
  annotation_north_arrow(which_north = "true", location = "true", style = north_arrow_nautical())

## ------------------------------------------------------------------------

# Now do Australia

ozbig <- read_sf("data/Australia/gadm36_AUS_1.shp")

# Monkey coded, to reduce number of points, takes forever.
oz_st <- maptools::thinnedSpatialPoly(
  as(ozbig, "Spatial"), tolerance = 0.1, 
  minarea = 0.001, topologyPreserve = TRUE)

library(rgeos)
oz <- st_as_sf(oz_st)

## ------------------------------------------------------------------------

# Extract geometries

head(oz$geometry[[1]][[1]][[1]])

head(oz)

dat <- unlist(oz$geometry, recursive = F) #--Not sure what recursive = F does, 
# but it's nice? It makes it a list again?
dat %>% flatten() 

# dissecting what they did
head(oz$geometry)



myl<- dat[[1]][[1]] %>% nrow()
cbind(dat[[1]][[1]], seq(1, myl, by = 1))

# I don't love this approach bc they don't name things as they add them. 

helper <- function(d){
  d <- unlist(d,recursive = FALSE)
  d <- purrr::map(d,.f=add_order)
  d <- add_layer(d)
  return(d)
}

add_order <- function(d){
  l <- nrow(d)
  return(cbind(d,seq(1,l,by=1)))
}
add_layer <- function(d){
  ll <- unlist(lapply(d,nrow))
  d <- do.call(rbind,d)
  d <- cbind(d,rep(c(1:length(ll)),time=ll))
  return(d)
}

purrr::map(oz$geometry, .f=helper) -> res

res <- add_layer(res)
colnames(res) <- c('long','lat','order','group','geo')
res <- as.data.frame(res)

ggplot(data=res)+geom_path(aes(x=long,y=lat,group=paste(res$geo,res$group,sep='.')))

## ------------------------------------------------------------------------
CANbig <- read_sf("data/gadm36_CAN_shp/gadm36_CAN_0.shp")

CAN_st <- maptools::thinnedSpatialPoly(
  as(CANbig, "Spatial"), tolerance = 0.1, 
  minarea = .001, topologyPreserve = TRUE)
CAN <- st_as_sf(CAN_st)

purrr::map(CAN$geometry, .f=helper) -> res

res <- add_layer(res)
colnames(res) <- c('long','lat','order','group','geo')

res <- as.data.frame(res)

ggplot(data=res)+geom_path(aes(x=long,y=lat,group=paste(res$geo,res$group,sep='.')))

