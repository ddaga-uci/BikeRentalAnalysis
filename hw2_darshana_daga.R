### BANA 288: Predictive Analytics

### Homework 2
### Name: Darshana Daga

### Question 1  
### Create two data sets from the original for analysis


setwd('C:\\Users\\darsh\\OneDrive\\Spring Quarter\\Predictive Analytics\\Assignment2')
options(stringsAsFactors = TRUE)    #need to run this twice
dat <- read.csv("hw2_hour.csv")
str(dat)
names(dat)
###  Creating dat1 - qualitative variables as factors
dat1 <- dat
dat1$mnth <- as.factor(dat1$mnth)
dat1$season <- as.factor(dat1$season)
dat1$hr <- as.factor(dat1$hr)
dat1$wkday <- as.factor(dat1$wkday)
dat1$weathersit <- as.factor(dat1$weathersit)
dat1 <- dat1[,c(15,1,3:14)]
str(dat1)
### Creating dat2 - with indicator variables
mnth <- as.factor(dat$mnth)
season <- as.factor(dat$season)
hr <- as.factor(dat$hr)
wkday <- as.factor(dat$wkday)
weathersit <- as.factor(dat$weathersit)
tmp_mnth <- data.frame(model.matrix(~mnth-1))
tmp_season <- data.frame(model.matrix(~season-1))
tmp_hr <- data.frame(model.matrix(~hr-1))
tmp_wkday <- data.frame(model.matrix(~wkday-1))
tmp_weathersit <- data.frame(model.matrix(~weathersit-1))
dat2 <- cbind(dat[,c(15,1,4)], tmp_season[,1:3], 
              tmp_mnth[,1:11], dat[,c(7,9)], 
              tmp_wkday[,1:6], tmp_hr[,1:23], 
              tmp_weathersit[,2:4], dat[,11:14])
rm(mnth, season, hr, wkday, weathersit)
rm(tmp_mnth, tmp_season, tmp_hr, tmp_wkday)
rm(tmp_weathersit)
dat2$workday <- NULL  
## removed workday as it seems to be co-linear with combination of weekday &  holidays together
str(dat2)



### Question 2 - Correlation

dat_cor <-data.frame(cor(dat[,c(15,1,3:14)])[,1])
colnames(dat_cor) <- 'correlation'
dat_cor


# The top 3 correlated variable to response (bike cnt) is temp, atemp and hr(temperature, feeling temperature and hour of the day). The variables do make sense, from a customer perspective if its cold (temp) or feels like cold (atemp) staying indoors seems more obvious choice, whereas if the temperatures are warmer, more people would like to step out. The relationship between temp, atemp and bike cnt is positive which is true to the former statement. Similarly people would be more active according to certain hour of the day, like morning school/work hours, lunch time, evening for going back home or just for fun. The positive correlation does suggest similar relationship among bike rental and the hour of the day.

### Question 3

part3 <- lm(cnt ~ season1 + season2 + season3 + hr0 + hr1 + hr2 + hr3
            + hr4 + hr5 + hr6 + hr7 + hr8 + hr9 + hr10 + hr11 + hr12 +
              hr13 + hr14 + hr15 + hr16 + hr17 + hr18 + hr19 +
              hr20 + hr21 + hr22 + weathersit2 + weathersit3 + weathersit4
            + temp + atemp + hum + windspeed, data = dat2)
summary(part3)

# The regression model part3 interpretation of coefficient of determination:
#   The part3 model explains 62.61% variability in the bike rentals,
# by using the seasonal variables, the hourly indicators,
# and the weather variables.
# At a 5% confidence level all variables except weathersit4 and hr6 are
# significant.
# Significant variables;
# 
# 1. Season 1  to 4
# 2. Hour 0 to 23 (except hour 6)
# 3. Weather 1, 2, and 3 (not weathersit4)
# 4. temp
# 5. atemp
# 6. hum
# 7. windspeed

### Question 4

part4 <- lm(cnt ~ season1 + season2 + season3 + hr0 + hr1 + hr2 + hr3
            + hr4 + hr5 + hr6 + hr7 + hr8 + hr9 + hr10 + hr11 + hr12 +
              hr13 + hr14 + hr15 + hr16 + hr17 + hr18 + hr19 +
              hr20 + hr21 + hr22 + weathersit2 + weathersit3
            + temp + atemp + hum + windspeed, data = dat2)
summary(part4)

### F Test
anova(part4,part3)


# Based on the part3 regression summary i removed the variables which
# were less significant at a 5% significance level. The two variables which
# were less significant are;
# 
# 1. Weathersit4
# 2. Hr6
# 
# I choose to remove only weathersit4 as practically the chances of extreme thunder or
# snow seems less reliable to describe variability in the sales (bike rental) forecasting.
# Hr6 however i decided to include in my model as it had 726 row observation which is a lot.
# And practically also the morning time should have a higher traffic and impact on the bike rentals.
# Then I ran a F test or anova to see if the model part4 performs better with 1 less variable and it does.
# The p value is 0.5744 which is very high and we fail to reject the null hypothesis,
# and model part4 is better than model part3. 

## Part 5

part5.1 <- lm(cnt ~., data = dat2)
summary(part5.1)

part5.2 <- lm(cnt ~ . - obs - mnth1 - mnth2 - mnth3 - mnth4 - 
                mnth5 - mnth6 - mnth8 - mnth10 - mnth11 - 
                wkday1 - wkday2 - wkday3 - wkday4 - wkday5 - weathersit4, data = dat2)
summary(part5.2)

part5 <- lm(cnt ~ . - obs - mnth1 - mnth2 - mnth3 - mnth4 -mnth5 - mnth6 - mnth8 -
              weathersit4, data = dat2)
summary(part5)



# I started by running a linear regression with all variables in. Then manually
# choose the variables which were significant at 5% significance level or higher while keeping an eye on the r squared,
# and removed variable which were less significant. From the initial all in
# model part5.1 with 53 variables with R squared of 0.6864, the model part5.2
# is giving similar result of R squared of 0.6848 but with just 37 variables.
# Practically the months effect will be covered within the year variable. 
# But i did feel that every day of week will have difference as some days would
# be more popular than other. So I decided to put back those variables.
# So my final model has 44 variables. Its also based on mthe fact that my
# adjusted R square only changed from 0.6855 to 0.6849 from all in model to 44
# variable model in part5, and r square is 0.6857.
# Also as learned from previous model, i did keep the hr6 as it seems important 
# business wise to include the peak hours and only removed weathersit4.

### Question 6


#dividing the data in train and test set
set.seed(123)
train_num <- sample(1:nrow(dat2), nrow(dat2) * 0.5)
dat.train <- dat2[train_num,]
dat.test <- dat2[-train_num,]

part6 <- lm(cnt ~ . - obs - mnth1 - mnth2 - mnth3 - mnth4 -mnth5 - mnth6 - mnth8 -
              weathersit4, data = dat2)
summary(part6)

#training
yhat.train1 <- predict(part6, dat.train)
#yhat.train
RSS.train1 <- sum((dat.train$cnt - yhat.train1)^2)
RSS.train1
MSE.train1 <- RSS.train1/(nrow(dat.train)-44-1)
MSE.train1
RMSE.train1 <- MSE.train1^0.5
RMSE.train1

#test
yhat.test1 <- predict(part6, dat.test)
#yhat.test1
RSS.test1 <- sum((dat.test$cnt - yhat.test1)^2)
RSS.test1
MSE.test1 <- RSS.test1/nrow(dat.test)
MSE.test1
RMSE.test1 <- MSE.test1^0.5
RMSE.test1

options(scipen=999)

tab1 <- matrix(c(RSS.train1, MSE.train1, RMSE.train1, 
                 RSS.test1, MSE.test1, RMSE.test1), ncol=2, byrow=FALSE)
colnames(tab1) <- c("Training6", "Test6")
rownames(tab1) <- c("RSS", "MSE", 
                    "RMSE")
tab1

# From the model part6 we calculate the RSS, MSE and RMSE on both training and
# test set. We can see that the numbers in both training and test sets for all
# the parameters are quite close with this model of selection. But its important 
# to notice the fit is slightly better on the training set than the test showing
# bias towards the training data than the test data. I do feel the model has
# high variance, but is relatively stable for both training and test set.
# But improvement in overall model fit has to be worked on further 
# as the R-squared is still around 68.57%.


### Question 7

part7 <- lm(cnt ~ ., data = dat.train)
summary(part7)

#training
yhat.train2 <- predict(part7, dat.train)
#yhat.train2
RSS.train2 <- sum((dat.train$cnt - yhat.train2)^2)
RSS.train2
MSE.train2 <- RSS.train2/(nrow(dat.train)-53-1)
MSE.train2
RMSE.train2 <- MSE.train2^0.5
RMSE.train2

#test
yhat.test2 <- predict(part7, dat.test)
#yhat.test2
RSS.test2 <- sum((dat.test$cnt - yhat.test2)^2)
RSS.test2
MSE.test2<- RSS.test2/nrow(dat.test)
MSE.test2
RMSE.test2 <- MSE.test2^0.5
RMSE.test2

options(scipen=999)

tab2 <- matrix(c(RSS.train2, MSE.train2, RMSE.train2, 
                 RSS.test2, MSE.test2, RMSE.test2), ncol=2, byrow=FALSE)
colnames(tab2) <- c("Training7", "Test7")
rownames(tab2) <- c("RSS", "MSE", 
                    "RMSE")
tab2
#combining the output from model part6 7 model part7 to compare the fit
compare <- cbind(tab1, tab2)
compare


# As we can see the model part6 has slightly better fit on training and test set
# when compared to model part 7.With RMSE 101.5367 for training and 102.1046 
# for test in part6 and 101.3428 for training and 102.4509 for test.
# Even though the model part7 is more flexible, part6 had a better bias-variance 
# trade-off and has about 9 less variables (44 variable in part6 vs 53 in part7).

### Question 8

#dividing the data in train and test set
train1 <- dat2[dat2$yr == 0,]
test1 <- dat2[dat2$yr == 1,]

#plotting the bike rental cnt for both years
plot(train1$cnt)
plot(test1$cnt)

#running the all in regression model on training data
part8 <- lm(cnt ~ ., data = train1)
summary(part8)

#training
yhat.train3 <- predict(part8, train1)
#yhat.train3
RSS.train3 <- sum((train1$cnt - yhat.train3)^2)
RSS.train3
MSE.train3 <- RSS.train3/(nrow(train1)-53-1)
MSE.train3
RMSE.train3 <- MSE.train3^0.5
RMSE.train3

#test
yhat.test3 <- predict(part8, test1)
#yhat.test3
RSS.test3 <- sum((test1$cnt - yhat.test3)^2)
RSS.test3
MSE.test3<- RSS.test3/nrow(test1)
MSE.test3
RMSE.test3 <- MSE.test3^0.5
RMSE.test3

options(scipen=999)

tab3 <- matrix(c(RSS.train3, MSE.train3, RMSE.train3, 
                 RSS.test3, MSE.test3, RMSE.test3), ncol=2, byrow=FALSE)
colnames(tab3) <- c("Training8", "Test8")
rownames(tab3) <- c("RSS", "MSE", 
                    "RMSE")
tab3


# Compared to the earlier two models part6 & part7, the current model performs 
# really badly on the training model(RMSE = 75.34)
# but fits very well on the test model(RMSE = 190.8469)
# It also has to be noted from the two plots that looks like there is visible 
# differences in the bike rentals in year 1 compared to year 2. And it seems 
# to be the reason that the model might not be fitting so well as it cannot
# explain the variability in year 1 or the training data.The model8
# is not biased for the training data but is more flexible and accommodates 
# changes of the test data for year 2.

### Question 9
# The managers of the bike rental should know few things about the predictions and models;
# 
# 1. As  seen in part8 each year is unique and definitely a relatively flexible model 
# will be more helpful to cover some of that variance year on year.Also a more random
# selection of sales instances is better for training model than yearly split.
# 2. The weather factor do seem quite related for predicting sales for next years.
# 3. There is still a lot of room for improvement but as per the current working
# model part6 seems to be doing well enough to help predict the year3 sales.
# The model part6 is relatively less flexible but is more consistent and stable
# across the training and test set.
# 4. For future prediction the variance coverage has to be improved. Currently
# we are explaining less than 70% of the variability and it can cost us huge
# dollars in lost revenues. 

### Question 10

part10A <- lm(cnt~. + I(hum^2), data = dat.train)
summary(part10A)


#training
yhat.train4 <- predict(part10A, dat.train)
RSS.train4 <- sum((dat.train$cnt - yhat.train4)^2)
MSE.train4 <- RSS.train4/(nrow(dat.train)-54-1)
RMSE.train4 <- MSE.train4^0.5

#test
yhat.test4 <- predict(part10A, dat.test)
RSS.test4 <- sum((dat.test$cnt - yhat.test4)^2)
MSE.test4 <- RSS.test4/(nrow(dat.test))
RMSE.test4 <- MSE.test4^0.5

options(scipen=999)

tab4 <- matrix(c(RSS.train4, MSE.train4, RMSE.train4, 
                 RSS.test4, MSE.test4, RMSE.test4), ncol=2, byrow=FALSE)
colnames(tab4) <- c("Training10A", "Test10A")
rownames(tab4) <- c("RSS", "MSE", 
                    "RMSE")
tab4

part10B <- lm(cnt~. + I(hum*season2), data = dat.train)
summary(part10B)


#training
yhat.train5 <- predict(part10B, dat.train)
RSS.train5<- sum((dat.train$cnt - yhat.train5)^2)
MSE.train5 <- RSS.train5/(nrow(dat.train)-54-1)
RMSE.train5 <- MSE.train5^0.5

#test
yhat.test5 <- predict(part10B, dat.test)
RSS.test5 <- sum((dat.test$cnt - yhat.test5)^2)
MSE.test5 <- RSS.test5/(nrow(dat.test))
RMSE.test5 <- MSE.test5^0.5

options(scipen=999)

tab5 <- matrix(c(RSS.train4, MSE.train4, RMSE.train4, 
                 RSS.test4, MSE.test4, RMSE.test4), ncol=2, byrow=FALSE)
colnames(tab5) <- c("Training10B", "Test10B")
rownames(tab5) <- c("RSS", "MSE", 
                    "RMSE")
tab5

compare10 <- cbind(tab1, tab4, tab5)
compare10


# As you can see from the above table compare10, there is not huge changes in
# the RMSE but a slight decline in the training RMSE with the quadratic variable 
# as well as with the interaction variable.  it drops from RMSE of 101.5367 to
# 101.2037. There can be further improvements made on this model. 





