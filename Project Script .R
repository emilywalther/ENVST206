NSLR <- read.csv("/Users/KaetheWalther/Desktop/ENVST206/NJnorthernSLR/NJ_Northern_slr_final_dist.gdb")
ACslr <- read.csv("/Users/KaetheWalther/Desktop/ENVST 206/AC_meantrend.csv")
read.table("/Users/KaetheWalther/Desktop/ENVST 206/AC_meantrend.csv", sep = ",", row.names=TRUE)

#fixed the period problem in the table heading
ACslr <- read.csv("/Users/KaetheWalther/Desktop/ENVST 206/Course Project/AC_meantrend.csv")
SandyHslr <- read.csv("/Users/KaetheWalther/Desktop/ENVST 206/Course Project/SandyHook_meantrend.csv")
CapeMayslr <- read.csv("/Users/KaetheWalther/Desktop/ENVST 206/Course Project/CapeMay_meantrend.csv")

mean(ACslr$Monthly_MSL)
datAC <- na.omit(data.frame(Monthly_MSL=ACslr$Monthly_MSL,
                            year=ACslr$Year,
                            month=ACslr$Month))
mean(datAC$Monthly_MSL)
sd(datAC$Monthly_MSL)
plot(datAC$year,datAC$Monthly_MSL, pch =19)


#make it look prettier
plot(datAC$year,datAC$Monthly_MSL, pch =19)
install.packages("ggplot2")
library(ggplot2)
ggplot(data = datAC, aes(x = year, y= Monthly_MSL, color= "red"))+ #data for plot
  geom_point()+ #make points at data point
  geom_path()+ #use lines to connect data points
  labs(x="year", y="Monthly Sea Level (m)")+
  theme_classic()+
  ggtitle("Sea Level (m) in Atlantic City, NJ")

#Same thing with Cape May

mean(CapeMayslr$Monthly_MSL)
datCapeMay <- na.omit(data.frame(Monthly_MSL=CapeMayslr$Monthly_MSL,
                            year=CapeMayslr$Year,
                            month=CapeMayslr$Month))

mean(datCapeMay$Monthly_MSL)
sd(CapeMayslr$Monthly_MSL)
plot(datCapeMay$year,datCapeMay$Monthly_MSL, pch =19)

#make nice looking
plot(datCapeMay$year,datCapeMay$Monthly_MSL, pch =19, col=c("Blue"))
library(ggplot2)
ggplot(data = datCapeMay, aes(x = year, y= Monthly_MSL, color= "Blue"))+ #data for plot
  geom_point(color="royalblue3")+ #make points at data point
  geom_path(color="royalblue")+ #use lines to connect data points
  labs(x="year", y="Monthly Sea Level (m)")+
  theme_classic()+
  ggtitle("Sea Level (m) in Cape May, NJ")

#Same thing with Sandy Hook

mean(SandyHslr$Monthly_MSL)
datSandyH <- na.omit(data.frame(Monthly_MSL=SandyHslr$Monthly_MSL,
                                 year=SandyHslr$Year,
                                 month=SandyHslr$Month))

mean(datSandyH$Monthly_MSL)
sd(SandyHslr$Monthly_MSL)
plot(datSandyH$year,datSandyH$Monthly_MSL, pch =19)

#make nice looking
plot(datSandyH$year,datSandyH$Monthly_MSL, pch =19, col=c("Green"))
library(ggplot2)
ggplot(data = datSandyH, aes(x = year, y= Monthly_MSL, color= "Green"))+ #data for plot
  geom_point(color="green")+ #make points at data point
  geom_path(color="green")+ #use lines to connect data points
  labs(x="year", y="Monthly Sea Level (m)")+
  theme_classic()+
  ggtitle("Sea Level (m) in Sandy Hook, NJ")

#question for class--> how to plot all of these on one plot. 
#make new data frame with the three data sets
plot(datSandyH$year, datSandyH$Monthly_MSL, main= "Sea Level Rise in New Jersey Shore Towns",
type = "b",
pch = 19,
ylab = "Monthly Sea Level (m)",
xlab = "Year", 
yaxt = "n")
points(datAC$year,datAC$Monthly_MSL,
       type = "b",
       pch = 19,
       col="tomato3")
points(datCapeMay$year,datCapeMay$Monthly_MSL,
       type = "b",
       pch = 19,
       col="green")
legend("topleft", 
       c("Sandy Hook", "Atlantic City", "Cape May"), 
       col= c("black", "tomato3", "green"), 
       pch=19, 
       lwd=1, 
       bty="n") 
axis(2, seq(-4,4, by=.25), las=1 )
#add title  
main
#lets try a regression assessment
SLR.mod <- lm(datAC$year ~ datAC$Monthly_MSL)
SLR.res <- rstandard(SLR.mod)

#set up qqplot
qqnorm(SLR.res)
#set up qq line
qqline(SLR.res)

#shapriowilks normality test
shapiro.test(SLR.res)
#make residual plot
plot(datAC$Monthly_MSL, SLR.res, 
     xlab = "Sea Level Rise (m)", 
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)

#interpret results
summary(SLR.mod)

#make plot of year and SLR
plot(datAC$year, datAC$Monthly_MSL, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Monthly Sea Level Rise (m)",
     xlab =  "Year")
#add regression line
#make line width thicker
abline(SLR.mod, lwd=2)

