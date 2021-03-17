
student <- read.csv("student-por.csv")
table(student$school)

# only consider school = GP, it reduces observations from 649 to 423
mydata <- student[student$school == "GP", c("G1", "sex", "age")]

# remove outlier, the one with 0 score, sample size becomes n=422
mydata_clean = mydata[mydata$G1 != 0,]
dim(mydata_clean) # 422, 3
names(mydata_clean)[1] = "score"
#write.csv(mydata_clean, "mydata_clean.csv")

# check distribution of the score (roughly normal)
par(mfrow=c(1,2))
with(mydata_clean, hist(score))
with(mydata_clean, qqnorm(score))
with(mydata_clean, qqline(score))

# scatterplot
library(ggplot2)
ggplot(mydata_clean, aes(x = age, y = score, col = sex)) + geom_point() +
  ggtitle("Score against age by gender")
  
# two sample t test
with(mydata_clean, t.test(score ~ sex))

# test the score difference between male and female, control for age
fit1 = lm(score ~ sex + age, data = mydata_clean)
summary(fit1)