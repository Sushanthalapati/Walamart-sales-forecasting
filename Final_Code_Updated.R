##############################  installing packages  ###################################

install.packages("dplyr")
install.packages("ggplot2")
install.packages("readr")
install.packages("lubridate")
install.packages("rpart")
install.packages("caret")
install.packages("corrplot")
install.packages("rpart.plot")
install.packages("glmnet")
install.packages("metrics")

################################  Calling libraries  ###################################

library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(rpart)
library(caret)
library(corrplot)
library(rpart.plot)
library(glmnet)
library(Metrics)

################################  Data Read and Manipulation  #######################################

train_data<- read.csv("train.csv")
stores <- read.csv("stores.csv")
features <- read.csv("features.csv")
stores$Store <- factor(stores$Store)
features$Store <- factor(features$Store)
train_data$Store <- factor(train_data$Store)
train_data <- full_join(train_data,stores,by=c("Store"))
train_data$WeekNum <- as.numeric(isoweek(ymd(train_data$Date)))

###################################  Aggregrating the sales  #########################################

final_data <- data.frame(Store=factor(),Date=as.Date(character()),Weekly_Sales=numeric(),IsHoliday=logical(),Type=factor(),WeekNum=factor())
aggregate_sales <- function(){
  for(i in 1:45){
    
    store_data <- train_data %>% filter(Store == i)
    u_d <- unique(train_data$Date)
    k = length(u_d)
    for (j in 1:k){
      wsc <-  subset(store_data, Date == u_d[j])
      wsc$Weekly_Sales <- as.numeric(wsc$Weekly_Sales)
      Weekly_Sales <- sum(wsc$Weekly_Sales)
      Store <- i
      Date <- u_d[j]
      IsHoliday <- wsc$IsHoliday[1]
      Type <- wsc$Type[1]
      WeekNum <- wsc$WeekNum[1]
      fdata <- data.frame(Store, Date, Weekly_Sales, IsHoliday, Type, WeekNum)
      final_data <- rbind(final_data,fdata)
    }
  }
  return(final_data)
}
# Sum the sales by store without taking into account each department
final_data <- aggregate_sales()
final_data$Store <- as.factor(final_data$Store)

final_data$seq_weeknos<-isoweek(final_data$Date) +52*(year(final_data$Date)-min(year(final_data$Date))) 

combined_data <- left_join(final_data, features,by=c("Store","Date","IsHoliday"))


#######################################  Checking for nulls  #########################################

sum(is.na(combined_data))
sum(is.na(combined_data$MarkDown1))
sum(is.na(combined_data$MarkDown2))
sum(is.na(combined_data$MarkDown3))
sum(is.na(combined_data$MarkDown4))
sum(is.na(combined_data$MarkDown5))


##############################  Replacing missing data with 0s in Markdown Events  #######################################

combined_data$MarkDown1 <- sapply(combined_data$MarkDown1, function(value){
  ifelse(is.na(value),0,value)
})
combined_data$MarkDown2 <- sapply(combined_data$MarkDown2, function(value){
  ifelse(is.na(value),0,value)
})
combined_data$MarkDown3 <- sapply(combined_data$MarkDown3, function(value){
  ifelse(is.na(value),0,value)
})
combined_data$MarkDown4 <- sapply(combined_data$MarkDown4, function(value){
  ifelse(is.na(value),0,value)
})
combined_data$MarkDown5 <- sapply(combined_data$MarkDown5, function(value){
  ifelse(is.na(value),0,value)
})

#######################################  Splitting the data set into train data set and test data set #######################

indices <- seq(1, nrow(combined_data), 143 )   #143 is the number of unique dates common for all store
test<- combined_data[indices,]
train<-combined_data[-indices,]
## combined data has 6435 records, test has 45 records corresponding to one per store, train has other 6390 record
#Descriptive analysis
storedata <- select(train_data, -Date) 
databyweek <- storedata %>% group_by(WeekNum)

######################################  Exploratory_analysis and plots ###########################################
summary(train)
ggplot(stores, aes(x = Size)) + geom_histogram(binwidth = 15000, color = "black", fill = "white") + facet_wrap(Type~.)
ggplot(train, aes(x = train$Weekly_Sales)) + geom_histogram() + facet_grid(train$Type~.) + scale_x_log10()

#CPI vs Sales for different Store Types
store_A <- combined_data %>% filter(Type == "A")
store_B <- combined_data %>% filter(Type == "B")
store_C <- combined_data %>% filter(Type == "C")

ggplot(store_A,aes(x=CPI,y=Weekly_Sales)) + geom_point(aes(color=store_A$IsHoliday)) + geom_smooth()
ggplot(store_B,aes(x=CPI,y=Weekly_Sales)) + geom_point(aes(color=store_B$IsHoliday)) + geom_smooth()
ggplot(store_C,aes(x=CPI,y=Weekly_Sales)) + geom_point(aes(color=store_C$IsHoliday)) + geom_smooth()

#Store Vs Unemployement
store_graph05 <- combined_data %>% filter(Store == 5)
store_graph10 <- combined_data %>% filter(Store == 10)
store_graph13 <- combined_data %>% filter(Store == 13)
store_graph30 <- combined_data %>% filter(Store == 30)
store_graph40 <- combined_data %>% filter(Store == 40)
store_graph42 <- combined_data %>% filter(Store == 42)

store_graph05$Date <- as.Date.character(store_graph05$Date)
store_graph10$Date <- as.Date.character(store_graph10$Date)
store_graph13$Date <- as.Date.character(store_graph13$Date)
store_graph30$Date <- as.Date.character(store_graph30$Date)
store_graph40$Date <- as.Date.character(store_graph40$Date)
store_graph42$Date <- as.Date.character(store_graph42$Date)

par(mfrow = c(2,3))
plot(store_graph05$Date,store_graph05$Unemployment)
plot(store_graph10$Date,store_graph10$Unemployment)
plot(store_graph13$Date,store_graph13$Unemployment)
plot(store_graph30$Date,store_graph30$Unemployment)
plot(store_graph40$Date,store_graph40$Unemployment)
plot(store_graph42$Date,store_graph42$Unemployment)

#Increasing Trend
plot(store_graph40$Date,store_graph40$CPI, xlab = "Time", ylab = "CPI")

# Fuel_Price Trend
plot(store_graph40$Date,store_graph40$CPI, xlab = "Time", ylab = "Fuel_Price")

#CPI vs Sales for combined data
ggplot(combined_data,aes(x=Date,y=Weekly_Sales)) + geom_point() + geom_smooth()

ggplot(combined_data,aes(x=Unemployment,y=Weekly_Sales)) + geom_point(aes(color=combined_data$IsHoliday)) + geom_smooth()

#Temperature vs Sales - Optimal sales during 25-80F else too low, too high - uncomfortable
ggplot(combined_data,aes(x=combined_data$Temperature,y=Weekly_Sales)) + geom_point(aes(color=combined_data$IsHoliday)) + geom_smooth()

#Impact of Markdown events on Sales
ggplot(combined_data,aes(x=combined_data$MarkDown2,y=Weekly_Sales)) + geom_point(aes(color=combined_data$IsHoliday)) + geom_smooth()
ggplot(combined_data,aes(x=combined_data$MarkDown3,y=Weekly_Sales)) + geom_point(aes(color=combined_data$IsHoliday)) + geom_smooth()

#Sales_Trend
combined_data$year<-year(combined_data$Date)

f<-combined_data %>% select(Store, WeekNum, year,Weekly_Sales,Type) %>%filter(Store ==30,year==2011,Type=="C")

plot(f$WeekNum, f$Weekly_Sales, main="Relation between weekly sales and week number for store 30 and Type C", sub="Year 2011", xlab="Week Number", ylab="Weekly sales")


#####################################################  Modelling  #################################################################

########################## Linear Model  #########################

#################### 45 individual Linear regressions ################
linear<-list()
for(i in 1:45)
{
  trains <- subset(train, train$Store == i)
  linear[[i]] <- lm(Weekly_Sales ~MarkDown2 + MarkDown3 + CPI+Unemployment+Temperature+Fuel_Price+seq_weeknos,data=trains)
}

predict_fit_pred<-list()
predict_fit_confidence<-list()
test$Store<-as.numeric(test$Store)

for(i in 1:nrow(test))
{ SToRE = test$Store[i]
  predict_fit_pred[[i]]<-predict(linear[SToRE], test[i,])
}

ypred<-unlist(predict_fit_pred)
yactual<-test$Weekly_Sales
deviation1<-ypred-yactual
percent_deviation1<-(ypred-yactual)*100/yactual
absolute_average_deviation1<-mean(abs(percent_deviation1))

######## Linear Regression with all stores as Categorical varaible ########

	Model_lm <- lm(Weekly_Sales ~ Store +MarkDown1 + MarkDown2+MarkDown3+ MarkDown4 + MarkDown5 + CPI + Unemployment + Temperature + Fuel_Price + seq_weeknos, IsHoliday, data = train)
	summary(Model_lm)
	test$Store<-as.factor(test$Store)
	Reg_pred<-predict(Model_lm, test)

deviation2<-Reg_pred-yactual
percent_deviation2<-(Reg_pred-yactual)*100/yactual
absolute_average_deviation2<-mean(abs(percent_deviation2))

######## 3 Different Linear Regression based on Store Type A, B, C ########

####### Splitting test and train data by store category ####### 

train_A<-subset(train, Type == "A")
train_B<-subset(train, Type == "B")
train_C<-subset(train, Type == "C")

test_A<-subset(test, Type == "A")
test_B<-subset(test, Type == "B")
test_C<-subset(test, Type == "C")

Model_lmA <- lm(Weekly_Sales ~ Store + MarkDown2 + MarkDown3  + CPI + Unemployment + Temperature + Fuel_Price + seq_weeknos, IsHoliday, data = train_A)
summary(Model_lmA)

Model_lmB <- lm(Weekly_Sales ~ Store + MarkDown2 + MarkDown3 + CPI + Unemployment + Temperature + Fuel_Price + seq_weeknos, IsHoliday, data = train_B)
summary(Model_lmB)

Model_lmC <- lm(Weekly_Sales ~ Store + MarkDown2 + MarkDown3 + CPI + Unemployment + Temperature + Fuel_Price + seq_weeknos, IsHoliday, data = train_C)
summary(Model_lmC)

test_A$Store<-as.factor(test_A$Store)
test_B$Store<-as.factor(test_B$Store)
test_C$Store<-as.factor(test_C$Store)
Reg_pred_A<-predict(Model_lmA, test_A)
Reg_pred_B<-predict(Model_lmB, test_B)
Reg_pred_C<-predict(Model_lmC, test_C)


######################### Ridge and Lasso ############################

n <- nrow(train)
shuffled_df <- train[sample(n), ]
train_indices <- 1:round(0.8 * n)
trainingdata <- shuffled_df[train_indices, ]
test_indices <- (round(0.8 * n) + 1):n
testdata <- shuffled_df[test_indices, ]

X=model.matrix(Weekly_Sales~., trainingdata)[, -1]
y=trainingdata$Weekly_Sales

#ridge regression: alpha=0; lasso regression: alpha = 1
#We run it for 100 lamda values
lambdas=10^seq(10, -2, length=100)
ridge.fit=glmnet(X, y, alpha=0, lambda = lambdas)

summary(ridge.fit)

#The coeffients are a 20X100 matrix. Each column is corresponding to each lamda. We take a look at one.
coef(ridge.fit)[,50]

#cross_valudation - cv.glmnet to select the optimal model
set.seed(1)
ridge.cv = cv.glmnet(X, y, alpha = 0)
plot(ridge.cv)
ridge.cv$lambda
opt_lambda = ridge.cv$lambda.min
opt_lambda = ridge.cv$lambda.1se
# we are finding optimal lambda 
opt_lambda
# the optimal lambda is 35654.42


testnew = testdata
Z=model.matrix(Weekly_Sales~., testnew)[, -1]
# we are predicting the weekly sales for optimal lamba using the ridge model we obtained
testnew$test_predicted = predict(ridge.fit, s = opt_lambda, newx = Z)
testnew$Std_dev1 <- (testnew$Weekly_Sales-testnew$test_predicted)/testnew$Weekly_Sales
library(dplyr)
subset1 <- testnew %>%
  select(Store,Date,Weekly_Sales,test_predicted,Std_dev1)
res <- glmnet(X, y, alpha = 0, lambda = lambdas, standardize = FALSE)
 coef(ridge.cv)
plot(res, xvar = "lambda",label = TRUE)
summary(res)
res


###############LASSO REGRESSION####################

#Using cross validation with default value k=10 
lasso.cv = cv.glmnet(X, y, alpha = 1)
plot(lasso.cv)
lasso.cv$lambda
lasso_lambda = lasso.cv$lambda.min
#optimal value for lambda
lasso_lambda = lasso.cv$lambda.1se
lasso_lambda

testnew2 = testdata

A=model.matrix(Weekly_Sales~., testnew2)[, -1]
test_predicted2 = predict(lasso.cv, s = lasso_lambda, newx = A)
testnew2$pred = test_predicted2

compare2 <- cbind(actual=testnew2$pred,testnew2$Weekly_Sales)
subset2 <- testnew2 %>%
  select(Store,Date,Weekly_Sales,pred)
reslasso <- glmnet(X, y, alpha = 1, lambda = lambdas, standardize = FALSE)
coef(lasso.cv)
plot(reslasso, xvar = "lambda",label = TRUE)
#cross_valudation - cv.glmnet to select the optimal model
set.seed(1)
ridge.cv = cv.glmnet(X, y, alpha = 0)
plot(ridge.cv)
ridge.cv$lambda
opt_lambda = ridge.cv$lambda.min
opt_lambda = ridge.cv$lambda.1se
# we are finding optimal lambda 
opt_lambda
# the optimal lambda is 35654.42


testnew = testdata
Z=model.matrix(Weekly_Sales~., testnew)[, -1]
# we are predicting the weekly sales for optimal lamba using the ridge model we obtained
testnew$test_predicted = predict(ridge.fit, s = opt_lambda, newx = Z)
testnew$Std_dev1 <- (testnew$Weekly_Sales-testnew$test_predicted)/testnew$Weekly_Sales
library(dplyr)
subset1 <- testnew %>%
  select(Store,Date,Weekly_Sales,test_predicted,Std_dev1)
res <- glmnet(X, y, alpha = 0, lambda = lambdas, standardize = FALSE)
plot(res, xvar = "lambda")