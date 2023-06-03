#installing the reguired packages for the study
library(corrplot)
library(mltools)
#importing the data csv
startup <- read.csv("C:\\Users\\Nikhil K\\OneDrive\\Desktop\\50_Startups.csv")
#displaying the data set
startup
#reading the initial 8 rows of the dataset
head(startup, 5)
#to check wether there are any missing values
cat("Contains missing values : ", any(is.na(startup)))
startup$State<-factor(startup$State,levels=c('New York','California','Florida'),labels=c(1,2,3))
startup
summary(startup)

#vizualization of data
#using graphs
#with rd
plot(startup[,5],startup[,1],type="l",main="RD VS Profit",xlab="Profit",ylab="RD",col="blue") 
#with admin
plot(startup[,5],startup[,2],type="l",main="admin vs Profit",xlab="Profit",ylab="admin",col="blue") 
#with
plot(startup[,5],startup[,3],type="l",main="marketing vs Profit",xlab="Profit",ylab="marketing",col="blue") 
#using histograhm
#with rd
hist(startup$R.D.Spend)
#with admin
hist(startup$Administration, col="blue")
#with marketing
hist(startup$Marketing, col="yellow")
#with profit
hist(startup$Profit, col="green")
#now we will plot all the dataset together
plot(startup)
#correlation of dependent variable with the other independent variables
cor(startup$R.D.Spend,startup$R.D.Spend)
cor(startup$Profit,startup$Administration)
cor(startup$Profit,startup$Marketing)
cor(startup[sapply(startup, is.numeric)])
#lets plot the correlation
corrplot(cor(startup[sapply(startup, is.numeric)]))
#we also know that scatter diagrams help us know the correlation
plot(R.D.Spend ~ Profit, data = startup)
plot(Marketing.Spend ~ Profit, data = startup)
plot(Administration ~ Profit, data = startup)
#setting up the models
regr <- lm(Profit ~ R.D.Spend + Administration + Marketing.Spend, data = startup)
summary(regr)
startup1 <- subset(startup, select = c(Profit, R.D.Spend))
startup1
#splitting the data into train and test data
set.seed(420) 
smp_siz = floor(0.75*nrow(startup1))
train_ind = sample(seq_len(nrow(startup1)),size = smp_siz)
train =startup1[train_ind,] 
test=startup1[-train_ind,] 
cat("Training set size is ", length(train$Profit), "\n")
cat("Testing set size is ", length(test$Profit))
test

model <- lm(Profit ~ R.D.Spend, data = train)
summary(model)
confint(model)

a=data.frame(R.D.Spend=101913.08)
result=predict(model,a)
result

predicteddata <- (predict(model,test))
bound <- cbind(test$Profit, predicteddata)
colnames(bound) = c('Real', 'Predicted')
predicted <- as.data.frame(bound)
head(predicted, n = 10)


model2<-glm(formula = Profit ~ R.D.Spend, data = train,family = poisson)
summary(model2)
confint(model2)

b=data.frame(R.D.Spend= 101913.08)
result1=exp(predict(model2,b))
result1

predicteddata <- exp((predict(model,test)))
bound <- cbind(test$Profit, predicteddata)
colnames(bound) = c('Real', 'Predicted')
predicted <- as.data.frame(bound)
head(predicted, n = 10)



