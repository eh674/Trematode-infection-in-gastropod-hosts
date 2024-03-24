all<-read.table("all.txt", header=TRUE)
summary(all)
allnew<-read.table("allnew.txt", header=TRUE)
summary(allnew)
rates<-read.table("rates.txt", header=TRUE)  
summary(rates)
ssh1<-read.table("newshell.txt", header=TRUE)
summary(ssh1)

#prediction 1

#graph to look at the prevalence of parasitism 

library(stringr)
ggplot(rates, aes(x = str_to_title(beach), y = rate, fill = species)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  labs(x = "Study Site", y = "Prevalence of Trematode Infection (%)",
       size = 14) +
  scale_fill_manual(values = c("pink", "yellow", "lightblue"), 
                    name = "Species",
                    labels = c("Common Periwinkle", "Dog Whelk", "Flat Periwinkle")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5, vjust = 0.5, color = "black"),  
        axis.text.y = element_text(size = 14, angle = 0, hjust = 0.5, vjust = 0.5, color = "black"), 
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        title = element_text(size = 14),
        legend.text = element_text(size = 14),  
        legend.title = element_text(size = 14)) +
  geom_vline(xintercept = -Inf, linetype = "solid", color = "black", size = 1) +
  geom_hline(yintercept = -Inf, linetype = "solid", color = "black", size = 1)  
rates$beach <- str_replace(rates$beach, "gylly", "Gyllyngvase")

library(stringr)
library(ggplot2)

ggplot(rates, aes(x = str_to_title(beach), y = rate, fill = species)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  labs(x = "Study Site", y = "Trematode Infection Prevalence (%)", size = 14) +
  scale_fill_manual(values = c("pink", "yellow", "lightblue"), 
                    name = "Host Species",  # Changed legend title
                    labels = c("Common Periwinkle", "Dog Whelk", "Flat Periwinkle")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5, vjust = 0.5, color = "black"),  
        axis.text.y = element_text(size = 14, angle = 0, hjust = 0.5, vjust = 0.5, color = "black"), 
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        title = element_text(size = 14),
        legend.text = element_text(size = 14),  
        legend.title = element_text(size = 14)) +
  geom_vline(xintercept = -Inf, linetype = "solid", color = "black", size = 1) +
  geom_hline(yintercept = -Inf, linetype = "solid", color = "black", size = 1)  

rates$beach <- str_replace(rates$beach, "gylly", "Gyllyngvase")

summary(rates$rate[rates$beach=="flushing"])
sd(rates$rate[rates$beach=="flushing"])
summary(rates$rate[rates$beach=="Gyllyngvase"])
sd(rates$rate[rates$beach=="Gyllyngvase"])
summary(rates$rate[rates$beach=="castle"])
sd(rates$rate[rates$beach=="castle"])

summary((rates$rate[rates$species=="common"]))
sd(rates$rate[rates$species=="common"])
summary((rates$rate[rates$species=="flat"]))
sd(rates$rate[rates$species=="flat"])
summary((rates$rate[rates$species=="dog"]))
sd(rates$rate[rates$species=="dog"])

#binomial generalized linear model
library(tidyverse)
m1<-glm(trematode_presence~beach*species, data=allnew, family=binomial(link="logit"))
coef(m1)
summary(m1)
anova(m1, test="Chi")
par(mfrow=c(2,2))
plot(m1)
m2<-glm(trematode_presence~beach+species, data=allnew, family=binomial(link="logit"))
anova(m1,m2, test="Chi")
summary(m2)
m3<-glm(trematode_presence~beach, data=allnew, family=binomial(link="logit"))
anova(m1,m3, test="Chi")
m4<-glm(trematode_presence~beach, data=allnew, family=binomial(link="logit"))
anova(m2,m4, test="Chi")
m5<-glm(trematode_presence~species, data=allnew, family=binomial(link="logit"))
anova(m2,m5, test="Chi")


#prediction 2

summary(ssh1)

#shell height graph

par(cex.lab = 1, cex.axis = 1, cex.main = 1, cex.sub = 1)
boxplot(sh ~ trematode_presence + species, data = all,
        col = c("lightgreen", "orange"),
        xlab = "Species of Marine Gastropod", ylab = "Shell Height (mm)", xaxt = "n",
        outline = FALSE)
legend("topright", legend = c("Uninfected", "Infected"), col = c("light green", "orange"), cex= 1, pt.cex= 2, pch = 16, bty = "n")
axis(1, at = c(1.5, 3.5, 5.5), labels = c("Common Periwinkle", "Dog Whelk", "Flat Periwinkle"), tick = TRUE)

#shell height statistical analysis
hist(all$sh)

hist(all$sh[all$species=="common"]) 
hist(all$sh[all$species=="flat"], breaks=7) 
hist(all$sh[all$species=="dog_whelk"]) 
leveneTest(sh~trematode_presence, data=subset(all, species=="common")) 
leveneTest(sh~trematode_presence, data=subset(all, species=="flat")) 
leveneTest(sh~trematode_presence, data=subset(all, species=="dog_whelk")) 

sd(all$sh[all$trematode_presence=="yes"])
sd(all$sh[all$trematode_presence=="no"])

summary(all$sh[all$species=="common" & all$trematode_presence=="yes"])
summary(all$sh[all$species=="common" & all$trematode_presence=="no"])
sd(all$sh[all$species=="common" & all$trematode_presence=="yes"])
sd(all$sh[all$species=="common" & all$trematode_presence=="no"])

summary(all$sh[all$species=="flat" & all$trematode_presence=="yes"])
summary(all$sh[all$species=="flat" & all$trematode_presence=="no"])
sd(all$sh[all$species=="flat" & all$trematode_presence=="yes"])
sd(all$sh[all$species=="flat" & all$trematode_presence=="no"])

summary(all$sh[all$species=="dog_whelk" & all$trematode_presence=="yes"])
summary(all$sh[all$species=="dog_whelk" & all$trematode_presence=="no"])
sd(all$sh[all$species=="dog_whelk" & all$trematode_presence=="yes"])
sd(all$sh[all$species=="dog_whelk" & all$trematode_presence=="no"])

m1<-glm(sh~trematode_presence + species + trematode_presence*species, data=all)
par(mfrow=c(2,2))
plot(m1) k
anova(m1, test="Chi") 
boxplot(sh~trematode_presence+species,data=all) 
m2<-glm(sh~trematode_presence+species, data=all)
anova(m1,m2, test="Chi") 
summary(m1) 
summary(m2)
m3<-glm(sh~trematode_presence, data=all)
anova(m1, m3, test="Chi") 
m4 <- glm(sh ~ trematode_presence, data = all)  
anova(m2,m4, test="Chi")
m5<- glm(sh~species, data=all) 
anova(m2,m5, test="Chi")

TukeyHSD(aov(sh~trematode_presence*species, data=all))

t.test(sh~trematode_presence, data=subset(all, species=="common"),
       var.equal = TRUE)
library(effsize)
cohen.d(sh~trematode_presence, data=subset(all, species=="common"))

t.test(sh~trematode_presence, data=subset(all, species=="dog_whelk"),
       var.equal = TRUE)

t.test(sh~trematode_presence, data=subset(all, species=="flat"))
cohen.d(sh~trematode_presence, data=subset(all, species=="flat"))

#shell width

hist(all$sw)
hist(all$sw[all$species=="common"]) 
hist(all$sw[all$species=="flat"], breaks=7) 
hist(all$sw[all$species=="dog_whelk"]) 
leveneTest(sw~trematode_presence, data=subset(all, species=="common")) 
leveneTest(sw~trematode_presence, data=subset(all, species=="flat")) 
leveneTest(sw~trematode_presence, data=subset(all, species=="dog_whelk")) 

summary(all$sw[all$species=="common" & all$trematode_presence=="yes"])
summary(all$sw[all$species=="common" & all$trematode_presence=="no"])
sd(all$sw[all$species=="common" & all$trematode_presence=="yes"])
sd(all$sw[all$species=="common" & all$trematode_presence=="no"])

summary(all$sw[all$species=="flat" & all$trematode_presence=="yes"])
summary(all$sw[all$species=="flat" & all$trematode_presence=="no"])
sd(all$sw[all$species=="flat" & all$trematode_presence=="yes"])
sd(all$sw[all$species=="flat" & all$trematode_presence=="no"])

summary(all$sw[all$species=="dog_whelk" & all$trematode_presence=="yes"])
summary(all$sw[all$species=="dog_whelk" & all$trematode_presence=="no"])
sd(all$sw[all$species=="dog_whelk" & all$trematode_presence=="yes"])
sd(all$sw[all$species=="dog_whelk" & all$trematode_presence=="no"])

#shell width graph

par(mfrow = c(3, 1))


par(cex.lab = 1, cex.axis = 1, cex.main = 1, cex.sub = 1)
boxplot(sw ~ trematode_presence + species, data = all,
        col = c("lightgreen", "orange"),
        xlab = "Species of Marine Gastropod", ylab = "Shell Width (mm)", xaxt = "n",
        outline = FALSE)
legend("topright", legend = c("Uninfected", "Infected"), col = c("light green", "orange"), cex= 1, pt.cex= 2, pch = 16, bty = "n")

axis(1, at = c(1.5, 3.5, 5.5), labels = c("Common Periwinkle", "Dog Whelk", "Flat Periwinkle"), tick = TRUE)


#shell width statistical analysis

hist(all$sw[all$species=="common"]) #normal
hist(all$sw[all$species=="flat"], breaks=7) #normal
hist(all$sw[all$species=="dog_whelk"]) #normal
leveneTest(sw~trematode_presence, data=subset(all, species=="common")) #variance is fine
leveneTest(sw~trematode_presence, data=subset(all, species=="flat")) #variance significant
leveneTest(sw~trematode_presence, data=subset(all, species=="dog_whelk")) #variance is fine


m1<-glm(sw~trematode_presence + species + trematode_presence*species, data=all)
par(mfrow=c(2,2))
plot(m1) 
anova(m1, test="Chi") 
boxplot(sw~trematode_presence*species,data=all) 
m2<-glm(sw~trematode_presence+species, data=all)
summary(m2)
anova(m1,m2, test="Chi") 
summary(m1) 
m3<-glm(sw~trematode_presence, data=all)
anova(m1, m3, test="Chi") 
m4 <- glm(sw ~ trematode_presence, data = all)  
anova(m2,m4, test="Chi")
m5<- glm(sw~species, data=all) 
anova(m2,m5, test="Chi")

TukeyHSD(aov(sw~trematode_presence*species, data=all))

t.test(sw~trematode_presence, data=subset(all, species=="common"),
       var.equal = TRUE)
cohen.d(sw~trematode_presence, data=subset(all, species=="common"))

t.test(sw~trematode_presence, data=subset(all, species=="dog_whelk"),
       var.equal = TRUE)
t.test(sw~trematode_presence, data=subset(all, species=="flat"))



hist(all$ssh[ssh1$species=="common"]) 
hist(all$ssh[ssh1$species=="dog_whelk"]) 
leveneTest(ssh~trematode_presence, data=subset(ssh1, species=="common")) 
leveneTest(ssh~trematode_presence, data=subset(ssh1, species=="dog_whelk"))

test<-lm(sw~trematode_presence *sh, data=subset(all, species=="common"))
summary(test)

m1<-glm(sw~trematode_presence *sh, data=subset(all, species=="common"))
par(mfrow=c(2,2))
plot(m1) 
anova(m1, test="Chi") 
plot(m1) 
anova(m1, test="Chi") 
boxplot(sw~trematode_presence*sh,data=subset(all, species=="common"))
m2<-glm(sw~trematode_presence+sh, data=subset(all, species=="common"))
summary(m1)
#r squared
1 - (3254.3 / 6734)
anova(m1,m2, test="Chi" ) 
summary(m2) 
m3<-glm(sw~trematode_presence, data=subset(all, species=="common"))
anova(m1, m3, test="Chi") 
m4 <- glm(sw ~ trematode_presence, data=subset(all, species=="common"))
anova(m2,m4, test="Chi")
m5<- glm(sw~sh, data=subset(all, species=="common"))
anova(m1,m5, test="Chi")

library(emmeans)
m1 <- glm(sw ~ trematode_presence * sh, data = subset(all, species == "common"))

sh_values <- data.frame(sh = 19.5)
emmeans(m1, ~ trematode_presence | sh, at = sh_values)

plot(all$sh[all$species=="common"], all$sw[all$species=="common"], col = ifelse(all$trematode_presence[all$species=="common"] == "yes", "orange", "light green"), pch = 16, cex= 0.4
     , xlab="Shell Height (mm)", ylab="Shell Width (mm)")
abline(a=-1.6533, b=0.6895, col="LIGHTGREEN", lwd="3")
abline((a=-1.6533+10.0506), b=0.6895-0.3560, col="ORANGE", lwd="3")
legend("topleft", legend = c("Uninfected", "Infected"), col = c("light green", "orange"), cex= 1, pt.cex= 2, pch = 16, bty = "n")

t.test(ssh~trematode_presence, data=subset(ssh1, species=="dog_whelk"),
       var.equal = TRUE)
t.test(ssh~trematode_presence, data=subset(ssh1, species=="common"),
       var.equal = TRUE)
cohen.d(ssh~trematode_presence, data=subset(ssh1, species=="common"))


#shell spire height graph

par(cex.lab = 1, cex.axis = 1, cex.main = 1, cex.sub = 1)
boxplot(ssh ~ trematode_presence + species, data = ssh1,
        col = c("lightgreen", "orange"),
        xlab = "Species of Marine Gastropod", ylab = "Shell Spire Height (mm)", xaxt = "n",
        outline = FALSE)
legend("topright", legend = c("Unparasitised", "Parasitised"), col = c("light green", "orange"), cex= 1, pt.cex= 2, pch = 16, bty = "n")

axis(1, at = c(1.5, 3.5), labels = c("Common Periwinkle", "Dog Whelk"), tick = TRUE)


par(cex.lab = 1, cex.axis = 1, cex.main = 1, cex.sub = 1)
boxplot(ssh ~ trematode_presence + species, data = ssh1,
        col = c("lightgreen", "orange"),
        xlab = "Species of Marine Gastropod", ylab = "Shell Spire Height (mm)", xaxt = "n",
        outline = FALSE,
        ylim = c(6, max(ssh1$ssh, na.rm = TRUE)))  # Set y-axis limits from 5 to the maximum value of ssh1$ssh
legend("topright", legend = c("Uninfected", "Infected"), col = c("light green", "orange"), cex= 1, pt.cex= 2, pch = 16, bty = "n")
axis(1, at = c(1.5, 3.5, 5.5), labels = c("Common Periwinkle", "Dog Whelk", "Flat Periwinkle"), tick = TRUE)
axis(2, at = seq(0, max(ssh1$ssh, na.rm = TRUE), by = 3), labels = FALSE)
par(cex.lab = 1, cex.axis = 1, cex.main = 1, cex.sub = 1)




summary(ssh1$ssh[all$species=="common" & ssh1$trematode_presence=="yes"])
summary(ssh1$ssh[all$species=="common" & ssh1$trematode_presence=="no"])
sd(ssh1$ssh[all$species=="common" & ssh1$trematode_presence=="yes"])
sd(ssh1$ssh[all$species=="common" & ssh1$trematode_presence=="no"])

summary(ssh1$ssh[all$species=="dog_whelk" & ssh1$trematode_presence=="yes"])
summary(ssh1$ssh[all$species=="dog_whelk" & ssh1$trematode_presence=="no"])
sd(ssh1$ssh[all$species=="dog_whelk" & ssh1$trematode_presence=="yes"])
sd(ssh1$ssh[all$species=="dog_whelk" & ssh1$trematode_presence=="no"])



#shell spire height ANOVA

hist(all$ssh[ssh1$species=="common"]) 
hist(all$ssh[ssh1$species=="dog_whelk"]) 
leveneTest(ssh~trematode_presence, data=subset(ssh1, species=="common")) 
leveneTest(ssh~trematode_presence, data=subset(ssh1, species=="dog_whelk"))

m1<-glm(ssh~trematode_presence *sh, data=subset(ssh1, species=="common"))
par(mfrow=c(2,2))
plot(m1) 
anova(m1, test="Chi") 
plot(m1) 
anova(m1, test="Chi") 
boxplot(ssh~trematode_presence*sh,data=subset(ssh1, species=="common"))
m2<-glm(ssh~trematode_presence+sh, data=subset(ssh1, species=="common"))
summary(m1)
anova(m1,m2, test="Chi" ) 
summary(m2) 
m3<-glm(ssh~trematode_presence, data=subset(ssh1, species=="common"))
anova(m1, m3, test="Chi") 
m4 <- glm(ssh ~ trematode_presence, data=subset(ssh1, species=="common"))
anova(m2,m4, test="Chi")
m5<- glm(ssh~sh, data=subset(ssh1, species=="common"))
anova(m1,m5, test="Chi")

library(emmeans)
m1 <- glm(ssh ~ trematode_presence * sh, data = subset(ssh1, species == "common"))
sh_values <- data.frame(sh = 27.5)
emmeans(m1, ~ trematode_presence | sh, at = sh_values)

plot(ssh1$sh[ssh1$species=="common"], ssh1$ssh[ssh1$species=="common"], xlab="Shell Height (mm)", ylab="Shell Spire Height (mm)", col = ifelse(ssh1$trematode_presence[ssh1$species=="common"] == "yes", "orange", "light green"), pch = 16, cex= 0.4)
abline(a=-1.46135, b=0.62103, col="LIGHTGREEN", lwd="3")
abline((a=-1.46135+4.23111), b=0.62103-0.15395, col="ORANGE", lwd="3")
legend("topleft", legend = c("Uninfected", "Infected"), col = c("light green", "orange"), cex= 1, pt.cex= 2, pch = 16, bty = "n")

regression<-lm(ssh~trematode_presence *sh, data=subset(ssh1, species=="common"))
summary(regression)

t.test(ssh~trematode_presence, data=subset(ssh1, species=="dog_whelk"),
       var.equal = TRUE)
t.test(ssh~trematode_presence, data=subset(ssh1, species=="common"),
       var.equal = TRUE)
cohen.d(ssh~trematode_presence, data=subset(ssh1, species=="common"))

#prediction 3

#weight graph

boxplot(weight ~ trematode_presence + species, data = all,
        col = c("lightgreen", "orange"),
        xlab = "Species of Marine Gastropod", ylab = "Snail Weight (g)", xaxt = "n",
        outline = FALSE)

legend("topright", legend = c("Uninfected", "Infected"), col = c("light green", "orange"), cex= 1, pt.cex= 2, pch = 16, bty = "n")

axis(1, at = c(1.5, 3.5, 5.5), labels = c("Common Periwinkle", "Dog Whelk", "Flat Periwinkle"), tick = TRUE)


#weight statistical analysis

summary(m1)

m1<-lm(weight~trematode_presence*sh, data=subset(all, species=="common"))
m2<-lm(weight~trematode_presence+sh, data=subset(all, species=="common"))
anova(m1,m2)
anova(m2)
anova(m1)
summary(m2)
summary(m1)
m2<-lm(weight~trematode_presence*sh, data=subset(all, species=="common"))
m3<-lm(weight~trematode_presence, data=subset(all, species=="common"))
plot(all$sh,all$weight,type="n")
abline(a=-4.586744,b=0.396843)
abline(a=(-4.586744+3.170444),b=0.396843-0.114430,col="RED")
anova(m2,m3)
summary(m3)
m4<-lm(weight~trematode_presence*sh, data=subset(all, species=="common"))
m5<-lm(weight~sh, data=subset(all, species=="common"))
anova(m4,m5)

summary(all$weight[all$trematode_presence=="no" & all$species=="common"])
summary(all$weight[all$trematode_presence=="yes" & all$species=="common"])

plot(all$sh[all$species=="common"], all$weight[all$species=="common"], ylab="Snail Weight (g)", xlab="Shell Height (mm)",
     col = ifelse(all$trematode_presence[all$species=="common"] == "yes", "orange", "light green"), pch = 16, cex= 0.4,
     xlim = c(10, max(all$sh[all$species=="common"])))
abline(a=-4.586744,b=0.396843, col="LIGHTGREEN", lwd="3")
abline(a=(-4.586744+3.170444),b=0.396843-0.114430,col="ORANGE", lwd="3")
legend("topleft", legend = c("Uninfected", "Infected"), col = c("light green", "orange"), cex= 1, pt.cex= 2, pch = 16, bty = "n")

library(emmeans)
m1 <- glm(weight ~ trematode_presence * sh, data = subset(all, species == "common"))
sh_values <- data.frame(sh = 18)
emmeans(m1, ~ trematode_presence | sh, at = sh_values)

m1<-lm(weight~trematode_presence*sh, data=subset(all, species=="dog_whelk"))
m2<-lm(weight~trematode_presence+sh, data=subset(all, species=="dog_whelk"))
anova(m1,m2)
anova(m2)
anova(m1)
summary(m2)

m1<-lm(weight~trematode_presence*sh, data=subset(all, species=="flat"))
m2<-lm(weight~trematode_presence+sh, data=subset(all, species=="flat"))
anova(m1,m2)
anova(m2)
anova(m1)
summary(m2)
summary(m1)
m2<-lm(weight~trematode_presence*sh, data=subset(all, species=="flat"))
m3<-lm(weight~trematode_presence, data=subset(all, species=="flat"))
anova(m2,m3)
m4<-lm(weight~trematode_presence*sh, data=subset(all, species=="flat"))
m5<-lm(weight~sh, data=subset(all, species=="flat"))
anova(m4,m5)

plot(all$sh[all$species=="common"], all$weight[all$species=="common"], ylab="Snail Weight (g)", xlab="Shell Height (mm)", col = ifelse(all$trematode_presence[all$species=="common"] == "yes", "orange", "light green"), pch = 16, cex= 0.4)
abline(a=-0.061686,b=0.079061, col="LIGHTGREEN", lwd="3")
abline(a=(-0.061686-1.460410),b=0.079061-0.118646,col="ORANGE", lwd="3")
legend("topleft", legend = c("Uninfected", "Infected"), col = c("light green", "orange"), cex= 1, pt.cex= 2, pch = 16, bty = "n")

library(emmeans)
summary(all$sh[all$species=="flat"])
m1 <- glm(weight ~ trematode_presence * sh, data = subset(all, species == "flat"))
sh_values <- data.frame(weight = 11)
emmeans(m1, ~ trematode_presence | sh, at = sh_values)

summary(all$weight[all$trematode_presence=="no" & all$species=="flat"])
summary(all$weight[all$trematode_presence=="yes" & all$species=="flat"])

#snail movement

boxplot(movement~parasite+species, data=movement1, range=0)

boxplot(movement ~ parasite + species, data = movement1, range=0, 
        col = c("lightgreen", "orange"),
        xlab = "Species of Marine Gastropod", ylab = "Number of Movements", xaxt = "n",
        outline = FALSE)

legend("topright", legend = c("Uninfected", "Infected"), col = c("light green", "orange"), cex= 1, pt.cex= 2, pch = 16, bty = "n")


axis(1, at = c(1.5, 3.5, 5.5), labels = c("Common Periwinkle", "Dog Whelk", "Flat Periwinkle"), tick = TRUE)


#movement statistics 

summary(movement1$movement[movement1$species=="common" & movement1$parasite=="no"])
summary(movement1$movement[movement1$species=="common" & movement1$parasite=="yes"])
sd(movement1$movement[movement1$species=="common" & movement1$parasite=="no"])
sd(movement1$movement[movement1$species=="common" & movement1$parasite=="yes"])

summary(movement1$movement[movement1$species=="dog_whelk" & movement1$parasite=="no"])
summary(movement1$movement[movement1$species=="dog_whelk" & movement1$parasite=="yes"])
sd(movement1$movement[movement1$species=="dog_whelk" & movement1$parasite=="no"])
sd(movement1$movement[movement1$species=="dog_whelk" & movement1$parasite=="yes"])

summary(movement1$movement[movement1$species=="flat" & movement1$parasite=="no"])
summary(movement1$movement[movement1$species=="flat" & movement1$parasite=="yes"])
sd(movement1$movement[movement1$species=="flat" & movement1$parasite=="no"])
sd(movement1$movement[movement1$species=="flat" & movement1$parasite=="yes"])

hist(movement1$movement[movement1$species=="dog_whelk"])
leveneTest(movement~parasite, data=subset(movement1, species=="dog_whelk")) 
wilcox.test(movement~parasite, data=subset(movement1, species=="dog_whelk"))
summary(movement1$species[movement1$species=="dog_whelk" & movement1$parasite=="no"])

hist(movement1$movement[movement1$species=="common"]) 
hist(log(movement1$movement[movement1$species=="common"]+1))
commonlog<-(log(movement1$movement[movement1$species=="common"]+1))
leveneTest(movement~parasite, data=subset(movement1, species=="common")) 
t.test(commonlog~parasite, data=subset(movement1, species=="common"),
       var.equal = TRUE)

hist(movement1$movement[movement1$species=="flat"])
hist(log(movement1$movement[movement1$species=="flat"]+1)) 
flatlog<-log(movement1$movement[movement1$species=="flat"]+1)
leveneTest(movement~parasite, data=subset(movement1, species=="flat"))
t.test(movement~parasite, data=subset(movement1, species=="flat"),
       var.equal = TRUE)
t.test(flatlog~parasite, data=subset(movement1, species=="flat"),
       var.equal = TRUE)


sum(all$trematode_presence=="yes" & all$species=="common")

summary(all$sh[all$trematode_presence=="yes" & all$species=="common"])
summary(all$sh[all$trematode_presence=="no" & all$species=="common"])

summary(all$weight[all$trematode_presence=="yes" & all$species=="common"])
summary(all$weight[all$trematode_presence=="no" & all$species=="common"])

