######
######
##### Assignment Question no 2 , Delivery time , Predict  the Delivery time based on sorting time
###### Y(output) is Delivery time and X(input) is Soting time


Del_time <- read.csv(file.choose())
attach(Del_time)
View(Del_time)

summary(Del_time)
plot(Sorting.Time, Delivery.Time)
### Sorting Time is X, and Delivery Time is Y

### after visualization of scatter plot, we can say it is positive in direction 
### strength is moderate
### 

cor(Sorting.Time, Delivery.Time)
### Sorting Time is X and Delivery Time is Y
## cor value = 0.0.8259973, correlatation is good, we can proceed with model building
### proceed with linear model with formula Y= Bo +B1x

DTmodel1 <- lm(Delivery.Time ~ Sorting.Time)
####### lm(Output Y ~ Input X)

DTmodel1
summary(DTmodel1)
### pvalue is less than 0.05, good significant value with *** (p-value: 3.98e-06)
### Multiple R-squared: 0.6823,	Adjusted R-squared:  0.6655, both values are close

predict(DTmodel1)
DTmodel1$residuals

confint(DTmodel1, level = 0.95)
#########          2.5 %    97.5 %
###(Intercept)  2.979134 10.186334
###Sorting.Time 1.108673  2.189367

predict(DTmodel1, interval = "confidence")

DTrmse <- sqrt(mean(DTmodel1$residuals^2))
DTrmse



##### LOG MODEL #####
### delivery time is Y, Sorting time is X
plot(log(Sorting.Time), Delivery.Time)
### Sorting Time is X, and Deliery time is Y
### after visualization of scatter plot, we can say it is positive in direction 
### strength is moderate

cor(log(Sorting.Time), Delivery.Time)

DTmodel2 = lm(Delivery.Time ~ log(Sorting.Time))
summary(DTmodel2)

DTrmse2 <- sqrt(mean(DTmodel2$residuals^2))
DTrmse2



plot(Sorting.Time, log(Delivery.Time))
### Sorting Time is X, and Deliery time is Y
### after visualization of scatter plot, we can say it is positive in direction 
### strength is strong

cor(Sorting.Time, log(Delivery.Time))
DTmodel3 = lm(log(Delivery.Time) ~ Sorting.Time)
##### lm(Output)
summary(DTmodel3)

log_DT <- predict(DTmodel3,interval = "confidence")
log_DT
EXP_DT <- exp(log_DT)

DT_err<- Delivery.Time - EXP_DT
DT_err

DTrmse3 <- sqrt(mean(DT_err^2))
DTrmse3
summary(DTmodel3)


########
######### Polynomial Transformation

DTmodel4 <- lm(Delivery.Time ~ Sorting.Time)
summary(DTmodel4)

confint(DTmodel4, level = 0.95)
DTlog_res <- predict(DTmodel4, interval = "confidence")
DTpoly <- exp(DTlog_res)
summary(DTlog_res)
summary(DTpoly)
DTpoly
err_DTpoly <- Sorting.Time - DTpoly
err_DTpoly
DTrmse4 <- sqrt(mean(err_DTpoly^2))
DTrmse4
