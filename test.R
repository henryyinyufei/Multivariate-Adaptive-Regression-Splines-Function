source("mars.R")
source("print.R")
source("plot.R")
source("anova.R")
source("predict.R")
source("summary.R")

library(ISLR)
# example 1
data(Wage)
mc<- mars.control(Mmax=10)
mout <- mars(wage ~ age + education ,Wage,mc)
ff <- fitted(mout)
p1 <- predict(mout)
p2 <- predict(mout, newdata = data.frame(age=Wage$age, education=Wage$education))
res = head(cbind(ff,p1,p2)) # columns are identical
mout # tests print method
summary(mout)# test the summary function
anova(mout)#test anova function
plot(mout) # test plot method

# example 2
data = read.csv('Real estate.csv')
mc<- mars.control(Mmax=10)
mout = mars(Y.house.price.of.unit.area ~ X3.distance.to.the.nearest.MRT.station,data,mc)
ff = fitted(mout)
p1 = predict(mout)
p2 = predict(mout,
             newdata=data.frame(X3.distance.to.the.nearest.MRT.station=data$X3.distance.to.the.nearest.MRT.station))
res = head(cbind(ff,p1,p2))
mout
summary(mout) 
anova(mout) 
plot(mout) 

# example 3
data = read.csv('insurance.csv')
mc<- mars.control(Mmax=10)
mout = mars(charges ~ bmi + age, data, mc)
ff = fitted(mout)
p1 = predict(mout)
p2 = predict(mout,
             newdata=data.frame(bmi = data$bmi, age = data$age))
res = head(cbind(ff,p1,p2))
summary(mout) 
plot(mout) 
anova(mout)
