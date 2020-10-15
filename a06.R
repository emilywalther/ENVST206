install.packages(c("sp","rgdal","dplyr"))
#package for vector data
library(sp)
#package for reading in spatial data
library(rgdal)
#data manangement package
library(dplyr)

g1966 <- readOGR ("/Users/KaetheWalther/Desktop/a06/GNPglaciers/GNPglaciers_1966.shp")

g2015 <- readOGR ("/Users/KaetheWalther/Desktop/a06/GNPglaciers/GNPglaciers_2015.shp")

#investigating format of this data
str(g2015)

#map the glaciers filling in the polygons with light blue and making the borders grey
plot(g1966, col = "lightblue2", border="grey50")

#data stores all accompanying info/measurements for each spatial object
head(g2015@data)

g1966@proj4string

#check glacier names
g1966@data$GLACNAME

g2015@data$GLACNAME

#fix glacier name so that it is consistent with the entire time period
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))

#lets combine area, first work with a smaller data frame
gdf66 <- data.frame(GLACNAME = g1966@data$GLACNAME,
                    area66 = g1966@data$Area1966)

gdf15 <- data.frame(GLACNAME = g2015@data$GLACNAME,
                    area15 = g2015@data$Area2015)

#join all data tables by glacier name
gAll <- full_join(gdf66,gdf15, by="GLACNAME")

#calculate the % change in area from 1966 to 2015
gAll$gdiff <- ((gAll$area66-gAll$area15)/gAll$area66)*100

plot(gAll$area66,gAll$gdiff,xlab="area of glaciers in 1966 (Km squared)",ylab="percent glacial loss")

#calculate mean 
mean(gAll$gdiff)

sd(gAll$gdiff)

#find the glacier with largest and smallest percent loss
max(gAll$gdiff)
maxglacier <- max(gAll$gdiff)
maxindex <- which(gAll$gdiff==maxglacier)
maxindex
gAll$GLACNAME

min(gAll$gdiff)
minglacier <- min(gAll$gdiff)
minindex <- which(gAll$gdiff==minglacier)
minindex

#max and min area time!
max(gAll$area66)
maxglacierarea <- max(gAll$area66)
maxindexarea <- which(gAll$area66==maxglacierarea)
maxindexarea
#percent change 
maxindexareapercent <- gAll$gdiff[maxindexarea]
maxindexareapercent

min(gAll$area66)
minglacierarea <- min(gAll$area66)
minindexarea <- which(gAll$area66==minglacierarea)
minindexarea

minindexareapercent <- gAll$gdiff[minindexarea]
minindexareapercent

g1966@data <- left_join(g1966@data, gAll, by="GLACNAME")
spplot(g1966, "gdiff", main="% change in area", col="transparent")
vulture66 <- g1966[g1966@data$GLACNAME == "Vulture Glacier",]
plot(vulture66, main = "Vulture Glacier in 1966", col="slategray")

Boulder66 <- g1966[g1966@data$GLACNAME == "Boulder Glacier",]
plot(Boulder66, main = "Boulder Glacier Loss", col="slategray")
Boulder15 <- g2015[g2015@data$GLACNAME == "Boulder Glacier",]
plot(Boulder15, col="orange", add=TRUE)
legend("bottomleft",c("1966","2015"), fill = c("slategray", "orange"))


Pumpelly66 <- g1966[g1966@data$GLACNAME == "Pumpelly Glacier",]
plot(Pumpelly66, main = "Pumpelly Glacier Loss", col="purple")
Pumpelly15 <- g2015[g2015@data$GLACNAME == "Pumpelly Glacier",]
plot(Pumpelly15, col="yellow", add=TRUE)
legend("bottomleft",c("1966","2015"), fill = c("purple", "yellow"))



