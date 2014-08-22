blood <- read.table('./Data/bloodcells.txt',header=T)

gender <- factor(rep(c('female','male'),c(5000,5000)))

blood$gender <- gender

tapply(blood$count, blood$gender, mean)

model <- glm(count ~ gender, family='poisson', data=blood)

summary(model)

### extension of example with second & third factor:

blood$groupx <- NA
blood$groupx[blood$count>2] <- rbinom(length(which(blood$count>2)), 1,prob=0.75)
blood$groupx[blood$count<=2] <- rbinom(length(which(blood$count<=2)), 1,prob=0.25)
blood$groupx <- factor(blood$groupx)

blood$groupz <- factor(round(runif(nrow(blood), 1, 3)))

model2 <- glm(count ~ gender*groupx*groupz, family='poisson', data=blood)

### Here, predict with newdata=... works ok:
predict(model2, newdata=data.frame(gender='male',groupx=factor(0), groupz=factor(1)))

### Re-specify model with specific interactions:

model3 <- glm(count ~ gender + groupx + groupz + gender:groupx, 
              family='poisson', data=blood)

predict(model3, newdata=data.frame(gender='male',groupx=factor(0), groupz=factor(1)))

blood2 <- rbind(blood, blood, blood, blood, blood)
re <- rnorm(5, 0, 3.6)
blood2$rgroup <- rep(re, each=nrow(blood))

library(lme4)
model3_re <- glmer(count ~ gender + groupx + groupz + gender:groupx + (1|rgroup), 
                   family='poisson', data=blood2)
