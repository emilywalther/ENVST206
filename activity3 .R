ch4 <-read.csv("/Users/KaetheWalther/Desktop/ao3/lemming_herbivory.csv")

ch4$herbivory <- as.factor(ch4$herbivory)

plot(ch4$CH4_Flux ~ ch4$herbivory, xlab ="Treatment", 
     ylab="CH4 fluxes (mgC m –2 day–1) ")

shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ctl"])

shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ex"])

bartlett.test(ch4$CH4_Flux ~ ch4$herbivory)

t.test(ch4$CH4_Flux ~ ch4$herbivory)

#insect data time!
datI <- read.csv("/Users/KaetheWalther/Desktop/ao3/insect_richness.csv")
#ANOVA
datI$urbanName <- as.factor(datI$urbanName)

#Shapiro Wilk Normality Test
shapiro.test(datI$Richness[datI$urbanName == "Developed"])
shapiro.test(datI$Richness[datI$urbanName == "Dense"])
shapiro.test(datI$Richness[datI$urbanName == "Suburban"])
shapiro.test(datI$Richness[datI$urbanName == "Natural"])

#Bartlett Test

in.mod <- lm(datI$Richness ~ datI$urbanName)
in.aov <- aov(in.mod)
summary(in.aov)

tukeyT <- TukeyHSD(in.aov)
tukeyT
plot(tukeyT, cex.axis=0.75)

tapply(datI$Richness, datI$urbanName, "mean")

species <- matrix(c(18,8,15,32), ncol=2, byrow = TRUE) 
colnames(species) <- c("Not protected", "Protected")
rownames(species) <- c("Declining", "Stable/Increase")

mosaicplot(species, xlab="population status", ylab="legal protection",
           main="Legal protection impacts on populations")

chisq.test(species)

species