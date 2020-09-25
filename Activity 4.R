datb <- read.csv("/Users/KaetheWalther/Desktop/a04/beaver_dam.csv")

plot(datb$dams.n, datb$area.ha, pch = 19)

dam.mod <- lm(datb$area.ha ~ datb$dams.n)

dam.res <- rstandard(dam.mod)

qqnorm(dam.res)

qqline(dam.res)

shapiro.test(dam.res)

#consensusthedataisnormallydistributed

plot(datb$dams.n, dam.res, pch=19)

head(datb)

plot(datb$dams.n, datb$area.h, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Surface water area (ha)",
     xlab =  "Number of beaver dams")

#set up regression
dam.mod <- lm(datb$area.ha ~ datb$dams.n)
#get standardized residuals
dam.res <- rstandard(dam.mod)
#set up qqplot
qqnorm(dam.res)
#add qq line
qqline(dam.res)
abline(h= 0)
dev.off()

#set up qq plot part 2 
qqnorm(dam.res)
#addline
qqline(dam.res)

#now the SW test
shapiro.test(dam.res)
#make residual plot
plot(datb$dams.n, dam.res, 
     xlab = "beaver damns", 
     ylab = "standardized residual")
#add horizantal line
abline(h=0)

summary(dam.mod)

#start interpreting at the intercept
#look at slope of line
#look at R squared to tell us the linear relationship bw x and y
#are beavers flooding the arctic?
#Yes! 

plot(datb$dams.n, datb$area.h, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Surface water area (ha)",
     xlab =  "Number of beaver dams")
abline(dam.mod, lwd=2)

#phenotime
pheno <- read.csv("/Users/KaetheWalther/Desktop/a04/red_maple_pheno.csv")

#set up panel plots
par(mfrow=c(1,2))
plot(pheno$Tmax,pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Maximum temperature (C)")
plot(pheno$Prcp,pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Precipitation (mm)")

plot(pheno$Lat,pheno$doy, 
    pch = 19,
    col = "purple",
    ylab = "Day of leaf out",
    xlab = "Latitude (degrees)")

plot(pheno$elev,pheno$doy, 
     pch = 19,
     col = "red",
     ylab = "Day of leaf out",
     xlab = "Elevation (m)")

#box plot to show rural/urb
sitefactor <- as.factor(pheno$siteDesc)
plot(sitefactor,pheno$doy, 
     pch = 19,
     col = "dark green",
     ylab = "Day of leaf out",
     xlab = "")

plot( ~  pheno$Lat + pheno$Tmax+ pheno$Tmin +pheno$Prcp + pheno$elev + pheno$siteDesc)

#ifelse
pheno$urID <- ifelse(pheno$siteDesc == "Urban",1,0)

mlr <- lm(pheno$doy ~  pheno$Tmax  + pheno$Prcp + pheno$elev + pheno$urID)
mlFitted <- fitted(mlr)

#qqnorm
pheno.res <- rstandard(mlr)
qqnorm(pheno.res)
qqline(pheno.res)

plot(mlFitted,pheno.res)

summary(mlr)
