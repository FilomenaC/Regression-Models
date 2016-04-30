library(plyr)
library(dplyr)
library(ggplot2)
library(knitr)
## Exploratory Data Analysis 
data(mtcars)
dim(mtcars)
str(mtcars)
attach(mtcars)

mtcars$cyl<-as.factor(mtcars$cyl)
mtcars$vs<-as.factor(mtcars$vs)
mtcars$am<-factor(mtcars$am)
mtcars$gear<-factor(mtcars$gear)
mtcars$carb<-factor(mtcars$carb)
#boxplot(mpg ~ am,xlab="Transmission (Automatic(0), Manual(1))",ylab="Miles Per Gallon (mpg)", main="Fuel Consumption vs. Transmission")

#Fuel Consumption by Transmission Type
ggplot(mtcars,aes(am, mpg)) + geom_boxplot() +
        xlab("Transmission Type") +
        ylab("Fuel Consumption (MPG)") +
        ggtitle("Fuel Consumption by Transmission Type")
       
#significance test       
ttest<-t.test(mpg~am,data=mtcars)

#mpg as f(am)
amfit<-lm(mpg~am,data=mtcars)
summary(amfit)


#mpg as f(all variables)

fullfit<-lm(mpg~.,data=mtcars)
summary(fullfit)

#step model algorithm
stepModel<-step(fullfit,k=log(nrow(mtcars))) #backward
summary(stepModel)

####Scatter Plot for Interaction Model
ggplot(mtcars,aes(x=wt,y=mpg,color=am))+geom_point()+ggtitle("MPG vs. Weight by Transmission")

# model with wt & transmission interaction term
IntModel<-lm(mpg ~ wt + qsec + am + wt:am, data = mtcars)
summary(IntModel)

#### Residual Plots
par(mfrow=c(2,2))
plot(IntModel)

        