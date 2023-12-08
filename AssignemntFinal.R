HealthData <- read.csv("HealthData.csv")
#Subset the Data, using our age range. 
HealthData1 <-subset(df, Age>=30 & Age<=50)
# 1. convert the data as a table
modifiedDf <- table(HealthData1$Sex, HealthData1$Smoke)
n=sum(modifiedDf)
gender.counts = rowSums(modifiedDf)
smoker.counts = colSums(modifiedDf)
#Expected table
e = outer (gender.count, smoker.count)/n
e

# we should use the Fisher's exact test instead of the Chi-square test because there is at least one cell below 5.
#Male yes expected count is less than 5 
test <- fisher.test(modifiedDf)

test
test$p.value

#fisher.test(table(HealthData$Sex, HealthData$Smoke))

mosaicplot(modifiedDf, color = TRUE)  

# Question 2 
HealthData <- read.csv("HealthData.csv")

alcohol.female = subset(HealthData, Sex=="Female", Alcohol, drop=TRUE)
alcohol.male = subset(HealthData, Sex=="Male", Alcohol, drop=TRUE)
#Standardistion 
#H0: difference in means = 0
#HA: mean male alchol consumption > female alcohol consumption
mean.female = mean(alcohol.female)
n.female = length(alcohol.female)
var.female = var(alcohol.female)
mean.male = mean(alcohol.male)
n.male = length(alcohol.male)
var.male = var(alcohol.male)
#The pooled variance s^2p for two group is
var.pool = ((n.male - 1)*var.male
            + (n.female - 1)*var.female)/(n.male + n.female - 2)
s.pool = sqrt(var.pool)

t.stat = (mean.male - mean.female)/(s.pool*sqrt(1/n.male + 1/n.female))
t.stat
t.test(Alcohol~Sex, data=HealthData, var.equal=TRUE, alternative="greater")
# variance is not very different 
aggregate(Alcohol~Sex, HealthData, sd)

boxplot(Alcohol~Sex, HealthData)
t.test(Alcohol~Sex, data=HealthData, var.equal=TRUE, alternative="greater")$p.value
#Question 3
#Question 4 
#First compute the real difference in means from the sample
#RetinoalPlasma 
#smoker yes or no 

RetinolPlasma.nonsmoke = subset(HealthData, Smoke=="No", RetinolPlasma, drop=TRUE)
RetinolPlasma.smoke = subset(HealthData, Smoke=="Yes", RetinolPlasma, drop=TRUE)

d0 = mean(RetinolPlasma.nonsmoke) - mean(RetinolPlasma.smoke)
d0
#repeat this 1000 times to obtain a bootstrap distribution.
d = replicate(1000,{
  nonsmoke.resamp = sample(RetinolPlasma.nonsmoke, replace=TRUE)
  smoke.resamp = sample(RetinolPlasma.smoke, replace=TRUE)
  mean(nonsmoke.resamp) - mean(smoke.resamp)
})
#The histogram is an estimate of the distribution of the difference in means.

hist(d, xlab="Sample difference", main="", breaks=20)
#95% confidence interval we can use the d to estimate the points that have 2.5% below and 2.5%
#99% bootstrap confidence intervals using percentiles 
quantile(d, c(0.005, 0.995))
q99=quantile(d, c(0.008,0.995))
# A line showing us the 99% region 
abline(v=c(q99[1],q99[2]), lwd=2, col=c(3,3))
#4b 

t.test(RetinolPlasma~Smoke, data=HealthData, var.equal=TRUE, conf=0.99)

######## QUESTION 3 ##########
dotchart(HealthData$BetaDiet, group = as.factor(HealthData$Vitamin), xlab = "Micrograms of beta-carotene consumed",
         pch = 16)
boxplot(BetaDiet ~ Vitamin, data = HealthData)

library(ggplot2)

ggplot(HealthData) +
  aes(x = Vitamin, y = BetaDiet, color = Vitamin) +
  geom_jitter() +
  theme(legend.position = "none")
anova_one_way = aov(BetaDiet ~ factor(Vitamin), data = HealthData)
summary(anova_one_way)
#The p-value is lower than the usual threshold of 0.05. You are confident to say there is a statistical difference between the groups
#Here, the factor is the species variable which contains 3 modalities or groups (1,2,3).
#####The one-way ANOVA test does not inform which group has a different mean. Instead, you can perform a Tukey test with the function TukeyHSD().
TukeyHSD(anova_one_way)


res_aov <- aov(BetaDiet ~ Vitamin,
               data = HealthData
)
# histogram
hist(res_aov$residuals)
#histogram roughly form a bell curve, indicating that the residuals follow a normal distribution.
#H0: data come from a normal distribution
#H1: data do not come from a normal distribution
shapiro.test(res_aov$residuals)


