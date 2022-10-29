#============Analysis of mtcars R coading=============
library(dsEssex)
data("mtcars") #calling data of mtcars
names(mtcars)
head(mtcars) #checking the variables of the data
str(mtcars) #structure of the data
colSums(is.na(mtcars))# checking the missiing values
summary(mtcars2)# to get the summary of the mtcars data
library(dplyr)
library(explore)
mtcars %>% 
  explore_all()
hist(mtcars$mpg, breaks=10, xlab = "Miles per gallon",#create a histogram of mpg
     main = "Historgram of Miles per gallon of cars",
     xlim = range(10:35))
plot(mtcars$mpg ~ as.factor(mtcars$am), mtcars,#create boxplot of values for mpg with respect to transmission
     xlab = "Transmission type", ylab="Miles per gallon", col="steelblue",
     main="Histogram of MPG by transmission type")
ggplot(mtcars, aes(cyl)) + #create line graph of values for cars with respect to cylinder
  geom_histogram(binwidth=1) + xlab('Cylinders') + ylab('Number of Cars') +
  ggtitle('Distribution of Cars by Cylinders')
boxplot(mpg ~ gear, data = mtcars)
cov(mtcars$mpg, mtcars$gear) #to get covariance between mpg and gear
mtcars %>% 
  select(gear, mpg, hp, cyl, am) %>% 
  explore_all(target = gear)

ggplot(mtcars, aes(x = hp, y = mpg, color = vs)) + geom_point() + geom_smooth()

#=======Multiple regression========
mtcars.lm=lm(mpg~.,data=mtcars)
summary(mtcars.lm)
mtcars.1m=lm(mpg~wt,data=mtcars)
summary (mtcars.1m)
Model1 <- lm( mtcars$mpg ~ hp + wt + as.factor(am), data=mtcars)
summary(Model1)
Model2 <- lm( mtcars$mpg ~ hp + wt + cyl+carb, data=mtcars)
summary(Model2)
Model3 <- lm( mtcars$mpg ~ hp + wt +cyl, data=mtcars)
summary(Model3)

#correlation
pp <- cor(mtcars) #checkimg correlation of the different columns of mtcars
pp[1,] #checking correlation of mpg to other variables
pp[4,] #checking correlation of hp to other variables
pp[5,] #checking correlation of drat to other variables
pp[6,] #checking correlation of wt to other variables
corr<-select(mtcars,mpg,cyl,hp,drat,disp,wt,qsec,vs,gear,am,carb)
pairs(corr)
library(corrplot)
M<-cor(mtcars) #correlation plot
corrplot.mixed(M,lower="circle",upper="number")

fit1 <- lm( mtcars$mpg ~ hp + wt, data=mtcars)
summary(fit1) #restricted model

plot(resid(fit1))#checking the heteroskedasticitys
abline(h=0)
