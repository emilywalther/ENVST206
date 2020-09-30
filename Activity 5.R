datW <- read.csv("/Users/KaetheWalther/Desktop/a05/noaa2011124.csv")

datW$NAME<- as.factor(datW$NAME)
#create vector
nameS <- levels(datW$NAME)
nameS

nameS[2]

#make data frame
datP <- na.omit (data.frame(PRCP = datW$PRCP, NAME = datW$NAME, year = datW$year ) )

#total annual precipitation for each site
pr <- aggregate(datP$PRCP, by=list(datP$NAME, datP$year), FUN="sum")
View(pr)
colnames(precip) <- c("NAME","year","totalP")
colnames(pr) <- c("NAME","year","totalP")
pr$ncount <- aggregate(datP$PRCP, by=list(datP$NAME,datP$year), FUN="length")$x
pr <- pr[pr$ncount >=364, ]

ca <- pr[pr$NAME == nameS[2], ]
ny <- pr[pr$NAME == nameS[5], ]

install.packages("ggplot2")
library(ggplot2)

plot(pr$year, pr$totalP)
ggplot(data = pr, aes(x = year, y = totalP, color = NAME))+
  geom_point()+
  geom_path()

ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+ #data for plot
  geom_point()+ #make points at data point
  geom_path()+ #use lines to connect data points
  labs(x="year", y="Annual Precipitation")+ #make axis labels
  theme_classic() #change plot theme

ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+
  geom_point(alpha=0.5)+
  geom_path(alpha=0.5)+
  labs(x="year", y="Annual Precipitation")+
  theme_classic()+
  scale_color_manual(values = c("#7FB3D5","#34495E", "#E7B800", "#FC4E07","#26A69A"))

#starting over bc i got confused what I have already done
datW <- read.csv("/Users/KaetheWalther/Desktop/a05/noaa2011124.csv")

datW$NAME<- as.factor(datW$NAME)
nameS <- levels(datW$NAME)
nameS

datP <- na.omit(data.frame(NAME=datW$NAME,
                           year=datW$year,
                           PRCP=datW$PRCP))
precip <- aggregate(datW$PRCP, by=list(datW$NAME,datW$year), FUN="sum", na.rm=TRUE)
precip <- aggregate(datP$PRCP, by=list(datP$NAME,datP$year), FUN="sum", na.rm=TRUE)

colnames(precip) <- c("NAME","year","totalP")
precip$ncount <- aggregate(datP$PRCP, by=list(datP$NAME,datP$year), FUN="length")$x

pr <- precip[precip$ncount >=364, ]
ca <- pr[pr$NAME == nameS[2], ]
ny <- pr[pr$NAME == nameS[5], ]

#cali plot
plot(ca$year, ca$totalP)
#make a better plot
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year")
#fix some things
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year", 
     yaxt = "n")
axis(2, seq(200,800, by=200), las=2 )

plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year", 
     yaxt = "n")

axis(2, seq(200,800, by=200), las=2 )
#add NY
points(ny$year, ny$totalP,
       type = "b",
       pch = 19,
       col="tomato3")

plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year", 
     yaxt = "n",
     ylim =c(0, 1600))

axis(2, seq(0,1600, by=400), las=2 )
#fix the scope of the graph
points(ny$year, ny$totalP,
       type = "b",
       pch = 19,
       col="tomato3")

plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year", 
     yaxt = "n",
     ylim =c(0, 1600))

axis(2, seq(0,1600, by=400), las=2 )
points(ny$year, ny$totalP,
       type = "b",
       pch = 19,
       col="tomato3")
legend("topleft", #position
       c("California", "New York"), 
       col= c("black", "tomato3"), 
       pch=19,
       lwd=1,
       bty="n") 

#plot of annual mean max temp for morrisville and Mandan
datT <- na.omit(data.frame(NAME=datW$NAME,
                           year=datW$year,
                           TMAX=datW$TMAX))
TMAX <- aggregate(datW$TMAX, by=list(datW$NAME,datW$year), FUN="mean", na.rm=TRUE)

colnames(TMAX) <- c("NAME","year","Max Temp")
TMAX$ncount <- aggregate(datT$TMAX, by=list(datT$NAME,datT$year), FUN="length")$x
TMAX

TMAX <- na.omit(TMAX)
TMAX$ncount <- aggregate(datT$TMAX, by=list(datT$NAME,datT$year), FUN="length")$x

#ran some things in the wrong order, one column had omitted values and the other didnt
Temp <- TMAX[TMAX$ncount >=364, ]
Mo <- Temp[Temp$NAME == nameS[5], ]
Mandan <- Temp[Temp$NAME == nameS[3], ]
nameS
plot( Mo$year, Mo$`Max Temp`)
#make plot
plot(Mo$year, Mo$`Max Temp`,
     type = "b",
     pch = 19,
     ylab = "Annual Temperature (Celcius)",
     xlab = "Year")

points(Mandan$year, Mandan$`Max Temp`,
       type = "b",
       pch = 19,
       col="tomato3")

#make axis nicer
plot(Mo$year, Mo$`Max Temp`,
     type = "b",
     pch = 19,
     ylab = "Annual Temperature (Celcius)",
     xlab = "Year",
     xaxt = "n",
     yaxt = "n" ,
     ylim = c(8,16),
     xlim = c(1930,2020))

points(Mandan$year, Mandan$`Max Temp`,
       type = "b",
       pch = 19,
       col="tomato3")

axis(2, seq(8,16, by=1),las=2)

axis(1, seq(1930,2020, by=5), las=1)

install.packages("ggplot2")
library(ggplot2)

plot(pr$year, pr$totalP)
ggplot(data = pr, aes(x = year, y = totalP, color = NAME))+
  geom_point()+
  geom_path()

ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+ #data for plot
  geom_point()+ #make points at data point
  geom_path()+ #use lines to connect data points
  labs(x="year", y="Annual Precipitation")+ #make axis labels
  theme_classic() #change plot theme

ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+
  geom_point(alpha=0.5)+
  geom_path(alpha=0.5)+
  labs(x="year", y="Annual Precipitation")+
  theme_classic()+
  scale_color_manual(values = c("firebrick4","limegreen", "goldenrod3", "navyblue","chocolate3"))

ggplot(data = datW, aes(x=NAME, y=TMIN))+ #look at daily tmin
  geom_violin(fill=rgb(0.933,0.953,0.98))+ #add a violin plot with blue color
  geom_boxplot(width=0.2,size=0.25, fill="grey90")+ #add grey boxplots and make them about 20% smaller than normal with 25% thinner lines than normal
  theme_classic() #git rid of ugly gridlines

sub <- datW[datW$NAME == nameS[4] & datW$ year == 1974,]
sub$DATE <- as.Date(sub$DATE,"%Y-%m-%d")

ggplot(data=sub, aes(x=DATE, y=TMAX))+
  geom_point()+
  geom_path()+
  theme_classic()+
  labs(x="year", y="Maximimum temperature (C)")

ggplot(data=sub, aes(x=DATE, y=PRCP))+
  geom_col(fill="royalblue3")+
  theme_classic()+
  labs(x="year", y="Daily precipitation (mm)")

#ARIZONA
#make a dataframe with just precipitation, year, and site name
#remove NA using na.omit
datP <- na.omit(data.frame(NAME=datW$NAME,
                           year=datW$year,
                           PRCP=datW$PRCP))
#total annual precipitation (mm)
precip <- aggregate(datW$PRCP, by=list(datW$NAME,datW$year), FUN="sum", na.rm=TRUE)
#use aggregate to get total annual precipitation
precip <- aggregate(datP$PRCP, by=list(datP$NAME,datP$year), FUN="sum", na.rm=TRUE)
#rename columns
colnames(precip) <- c("NAME","year","totalP")
#add the x column from aggregate looking at the length of observations in each year
precip$ncount <- aggregate(datP$PRCP, by=list(datP$NAME,datP$year), FUN="length")$x
pr <- precip[precip$ncount >=364, ]
AZ <- pr[pr$NAME == nameS[4], ]
nameS
plot(AZ$year, AZ$totalP)
plot(AZ$year, AZ$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year")

datT <- na.omit(data.frame(NAME=datW$NAME,
                           year=datW$year,
                           TMAX=datW$TMAX))
TMAX <- aggregate(datW$TMAX, by=list(datW$NAME,datW$year), FUN="mean", na.rm=TRUE)

colnames(TMAX) <- c("NAME","year","Max Temp")
TMAX$ncount <- aggregate(datT$TMAX, by=list(datT$NAME,datT$year), FUN="length")$x
TMAX

TMAX <- na.omit(TMAX)
TMAX$ncount <- aggregate(datT$TMAX, by=list(datT$NAME,datT$year), FUN="length")$x

Temp <- TMAX[TMAX$ncount >=364, ]
AZ <- Temp[Temp$NAME == nameS[4], ]
plot( AZ$year, AZ$`Max Temp`)
#make plot
plot(AZ$year, AZ$`Max Temp`,
     type = "b",
     pch = 19,
     ylab = "Annual Temperature (Celcius)",
     xlab = "Year")
#REDO 1974 LOL
sub <- datW[datW$NAME == nameS[1] & datW$ year == 1974,]
sub$DATE <- as.Date(sub$DATE,"%Y-%m-%d")
ggplot(data=sub, aes(x=DATE, y=PRCP))+
  geom_col(fill="royalblue3")+
  theme_classic()+
  labs(x="year", y="Daily precipitation (mm)")
sub <- datW[datW$NAME == nameS[1] & datW$ year == 1974,]
sub$DATE <- as.Date(sub$DATE,"%Y-%m-%d")
ggplot(data=sub, aes(x=DATE, y=TMAX))+
  geom_point()+
  geom_path()+
  theme_classic()+
  labs(x="year", y="Maximimum temperature (C)")

#Violin plot

ggplot(data = datW, aes(x=NAME, y=TMIN))+ #look at daily tmin
  geom_violin(fill=rgb(0.933,0.953,0.98))+ #add a violin plot with blue color
  geom_boxplot(width=0.2,size=0.25, fill="grey90")+ #add grey boxplots and make them about 20% smaller than normal with 25% thinner lines than normal
  theme_classic() #git rid of ugly gridlines
dev.off()
sub <- datW[datW$NAME == nameS[1] & datW$ year > 1999,]
sub$year <- as.factor(sub$year)
#make violin plot
ggplot(data = sub, aes(x=year, y=TMIN))+ #look at daily tmin
  geom_violin(fill=rgb(0.933,0.953,0.98))+ #add a violin plot with blue color
  geom_boxplot(width=0.2,size=0.25, fill="grey90")+ #add grey boxplots and make them about 20% smaller than normal with 25% thinner lines than normal
  theme_classic()+ #git rid of ugly gridlines
  labs(x="year", y="TMIN (degrees celcius)")

