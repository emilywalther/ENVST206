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
  
