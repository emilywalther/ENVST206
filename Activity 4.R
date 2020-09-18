datb <- read.csv("/Users/KaetheWalther/Desktop/a04/beaver_dam.csv")

plot(datb$dams.n, datb$area.ha, pch = 19)

dam.mod <- lm(datb$area.ha ~ datb$dams.n)

dam.res <- rstandard(dam.mod)

qqnorm(dam.res)

qqline(dam.res)

shapiro.test(dam.res)

#consensusthedataisnormallydistributed

plot(datb$dams.n, dam.res, pch=19)

abline(h= 0)

summary(dam.mod)

#start interpreting at the intercept
#look at slope of line
#look at R squared to tell us the linear relationship bw x and y
#are beavers flooding the arctic?
#Yes! 