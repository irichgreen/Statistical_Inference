###Summary of ToothGrowth Dataset
data(ToothGrowth)
summary(ToothGrowth)
str(ToothGrowth)


##Scenario #1

a <- 0.05
m1 <- ToothGrowth[ToothGrowth$dose==0.5,"len"]
m2 <- ToothGrowth[ToothGrowth$dose==1.0,"len"]
t1 <- t.test(m2,m1,paired = FALSE,var.equal = FALSE)
t1$conf

m3 <- ToothGrowth[ToothGrowth$dose==2.0,"len"]
t1 <- t.test(m3,m2,paired = FALSE,var.equal = FALSE)
t1$conf

##Scenario #2

a  <- 0.05
m1 <- ToothGrowth[ToothGrowth$dose==0.5 & ToothGrowth$supp=="VC","len"]
m2 <- ToothGrowth[ToothGrowth$dose==0.5 & ToothGrowth$supp=="OJ","len"]
t1 <- t.test(m2,m1,paired = FALSE,var.equal = FALSE,conf.level=(1-a/2))
t1$conf

a  <- 0.05
m1 <- ToothGrowth[ToothGrowth$dose==1.0 & ToothGrowth$supp=="VC","len"]
m2 <- ToothGrowth[ToothGrowth$dose==1.0 & ToothGrowth$supp=="OJ","len"]
t1 <- t.test(m2,m1,paired = FALSE,var.equal = FALSE,conf.level=(1-a/2))
t1$conf

a  <- 0.05
m1 <- ToothGrowth[ToothGrowth$dose==2.0 & ToothGrowth$supp=="VC","len"]
m2 <- ToothGrowth[ToothGrowth$dose==2.0 & ToothGrowth$supp=="OJ","len"]
t1 <- t.test(m2,m1,paired = FALSE,var.equal = FALSE,conf.level=(1-a/2))
t1$conf

##Conclusion

library(ggplot2)

plot(x=ToothGrowth$dose,y=ToothGrowth$len,xlab="Vitamin C Dose",
ylab="Tooth Growth",
main="Vitamin C Impact on Tooth Growth")

gp <- ggplot(ToothGrowth, aes(x=interaction(supp, dose), y=len)) + geom_boxplot()

gp <- gp + xlab("Delivery Method & Dose")
gp <- gp + ylab("Tooth Growth")
gp <- gp + ggtitle("Boxplot of Response in Toothgrowth from Vitamin C")
gp

coplot(len ~ dose | supp, data = ToothGrowth, panel = panel.smooth,
xlab = "ToothGrowth data: length vs dose, given type of supplement")

a <- 0.05
m1 <- ToothGrowth[ToothGrowth$dose==0.5,"len"]
m2 <- ToothGrowth[ToothGrowth$dose==1.0,"len"]
t1 <- t.test(m2,m1,paired = FALSE,var.equal = FALSE)
t1

m3 <- ToothGrowth[ToothGrowth$dose==2.0,"len"]
t1 <- t.test(m3,m2,paired = FALSE,var.equal = FALSE)
t1

a  <- 0.05
m1 <- ToothGrowth[ToothGrowth$dose==0.5 & ToothGrowth$supp=="VC","len"]
m2 <- ToothGrowth[ToothGrowth$dose==0.5 & ToothGrowth$supp=="OJ","len"]
t1 <- t.test(m2,m1,paired = FALSE,var.equal = FALSE,conf.level=(1-a/2))
t1
