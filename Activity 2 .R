#activity 2

heights <- c(3,2,3)

datW <-read.csv ("/Users/KaetheWalther/Documents/ES DATA /a02/noaa2011124.csv")

datW$PRCP_cm <- datW$PRCP/10

mean(datW$PRCP_cm, na.rm=TRUE)

str (datW)
datW$NAME <- as.factor(datW$NAME)
myvector <- c(1,4,7,9,11)
numericvector <- c(-1.5,.25,.75,1.25,1.5)
charactervector <- c("hello","R","world","what","up")
factordatavector <- c("coffee","latte","coffee","cappucchino","mocha")
myfactor <- as.factor(factordatavector)

#find out all unique site names 
levels(datW$NAME)
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"])
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)
sd(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp
datW$siteN <- as.numeric(datW$NAME)
hist(datW$TAVE[datW$siteN == 3],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[3]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey75",
     border="white")
AberdeenMean <-mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"])
qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
1-pnorm(18.51026,4+mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

hist(datW$PRCP[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Precipitation (mm)", 
     ylab="Relative frequency",
     col="grey75",
     border="white")

averagePRCP <- aggregate(datW$PRCP, by=list(datW$NAME,datW$year), FUN="sum",na.rm=TRUE)
averagePRCP
averagePRCP$Group.1 <- as.factor(averagePRCP$Group.1)
averagePRCP$Group.1 <- as.numeric(averagePRCP$Group.1)
hist(averagePRCP$x[averagePRCP$Group.1 == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Annual Precipitation (mm)", 
     ylab="Relative frequency",
     col="grey75",
     border="white")

hist(averagePRCP$x[averagePRCP$Group.1 == 3],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[3]),
     xlab = "Annual Precipitation (mm)", 
     ylab="Relative frequency",
     col="grey75",
     border="white")

pnorm(700,mean(averagePRCP$x[averagePRCP$Group.1 == 1],na.rm=TRUE),
      sd(averagePRCP$x[averagePRCP$Group.1 == 1],na.rm=TRUE))

pnorm(700,mean(averagePRCP$x[averagePRCP$Group.1 == 3],na.rm=TRUE),
      sd(averagePRCP$x[averagePRCP$Group.1 == 3],na.rm=TRUE))