library(haven)
library(lubridate)
library(tidyverse)
library(tsibble)
library(urca)
library(ggplot2)
library(forecast)
library(stargazer)
library(vars)
library(cowplot)
library(gridExtra)


USdata <- read_dta("/Users/wilsontai/Downloads/assignment1.dta")
USdata <- USdata[, c(1, 2, 8)]
colnames(USdata)[2] = "GDP" 
colnames(USdata)[3] = "Chocolate"
attach(USdata)

##PART 1
##Question a
USdata  <- USdata %>%
  mutate(date=ymd(datestr), logGDP = log(GDP)) %>% 
  as_tsibble(index=date)

cols <- c("GDP" = "blue")

ggplot(data=USdata, aes(x=date)) +
  theme_bw() +
  geom_line(aes(y=logGDP, color = "GDP")) + 
  scale_colour_manual(name="Legend",values=cols) +
  labs(x = "Date", y = "log(GDP)", title = "GDP for US")

##Question b
summary(ur.df(USdata$logGDP, type = "trend")) ##null: unit root: fail to reject. Solution is to first difference


USdata <- USdata %>% 
  mutate(growth = difference(logGDP)) %>% drop_na() ##growth

summary(ur.df(USdata$growth)) 

cols <- c("Growth" = "blue")
ggplot(data=USdata, aes(x=date)) +
  theme_bw() +
  geom_line(aes(y=growth, color="Growth")) +
  scale_colour_manual(name="Legend",values=cols) +
  labs(x = "Date", y = "Growth", title = "Growth for US")


##Question c
ggAcf(USdata$growth, lag.max = 25, main = "ACF for Growth") + theme_bw()


#Question d
ar1 <- arima(USdata$logGDP, c(1,1,0))
ar2 <- arima(USdata$logGDP, c(2,1,0))
ma1 <- arima(USdata$logGDP, c(0,1,1))
ma2 <- arima(USdata$logGDP, c(0,1,2))

##Question e
stargazer(ar1, ar2, ma1, ma2, type = "latex")


cols <- c("AR2" = "blue")
ggplot(data=USdata, aes(x=date)) +
  theme_bw() +
  geom_line(aes(y=ar2$residuals, color="AR2")) + 
  scale_colour_manual(name="Legend",values=cols) +
  labs(x = "Date", y = "AR(2) Residuals", title = "AR(2) Residuals")

cols <- c("MA2" = "blue")
ggplot(data=USdata, aes(x=date)) +
  theme_bw() +
  geom_line(aes(y=ma2$residuals, color="MA2")) +
  scale_colour_manual(name="Legend",values=cols) +
  labs(x = "Date", y = "MA(2) Residuals", title = "MA(2) Residuals")


Box.test(ar2$residuals, lag = 10, type = "Ljung")
Box.test(ma2$residuals, lag = 10, type = "Ljung")



##Question f
#Train/Test rule of thumb: 80/20 split
154*0.8
train <- filter(USdata ,date < as.Date('2014-04-01'))
test = filter(USdata, date>= as.Date('2014-07-01'))

ar2 <- arima(train$growth ,order = c(2,1,0))
ar2fit <- fitted(ar2)
ar2pred <- predict(ar2, n.ahead = nrow(test))
ma2 <- arima(train$growth ,order = c(0,1,2))
ma2fit <- fitted(ma2)
ma2pred <- predict(ma2, n.ahead = nrow(test))

test$ar2pred <- ar2pred$pred
test$ar2low <- ar2pred$pred-1.96*ar2pred$se
test$ar2high <- ar2pred$pred + 1.96*ar2pred$se

test$ma2pred <- ma2pred$pred
test$ma2low <- ma2pred$pred-1.96*ma2pred$se
test$ma2high <- ma2pred$pred + 1.96*ma2pred$se

train$ar2fit <- ar2fit
train$ma2fit <- ma2fit

cols = c("Growth" = "blue", "AR(2)" = "red", "MA(2)" = "green")
ggplot(data = train, aes(x = date)) +
  geom_line(aes(y=growth, color="Growth"))+
  geom_line(aes(y=ar2fit, color="AR(2)")) +
  geom_line(aes(y=ma2fit, color="MA(2)")) +
  scale_colour_manual(name="Legend",values=cols) +
  theme_bw() +
  labs(x="Date",
       y="US Growth", title= "How well does AR(2) and MA(2) fit?")


ggplot(data=test, aes(x=date)) +
  geom_line(aes(y=growth, color="Growth"))+
  geom_line(aes(y=ar2pred, color="AR(2)"))+
  geom_line(aes(y=ar2low), color="red", linetype = "dashed")+
  geom_line(aes(y=ar2high), color="red", linetype = "dashed") +
  geom_line(aes(y=ma2pred, color="MA(2)"))+
  geom_line(aes(y=ma2low), color="green", linetype = "dashed") +
  geom_line(aes(y=ma2high), color="green", linetype = "dashed") +
  theme_bw() +
  scale_colour_manual(name="Legend",values=cols) +
  labs(x="Date",
       y="US Growth Forecast", title= "AR(2) and MA(2) Forecasts")


##question g
##this section requires me to comment on forecasting power




#############
##PART 2
##Question a
##comment on how chocolate could impact GDP
cols = c("log(GDP)" = "blue", "log(Chocolate)" = "red")
ggplot(data=USdata, aes(x=date)) +
  theme_bw() +
  geom_line(aes(y=log(Chocolate), color="log(Chocolate)")) + 
  geom_line(aes(y=logGDP, color="log(GDP)")) +
  scale_colour_manual(name="Legend",values=cols) +
  labs(x = "Date", y = "Log Values", title = "log(GDP) and log(Chocolate)")


##Question b
cols = c("log(Chocolate)" = "red")
ggplot(data=USdata, aes(x=date)) +
  theme_bw() +
  geom_line(aes(y=log(Chocolate), color="log(Chocolate)")) + 
  scale_colour_manual(name="Legend",values=cols) +
  labs(x = "Date", y = "Chocolate Price", title = "Price of Chocolate")

summary(ur.df(log(Chocolate), type = "trend")) ##null: unit root: fail to reject. Solution is to first difference


USdata <- USdata %>% 
  mutate(Coco = difference(log(Chocolate))) %>% drop_na() ##Chocolate FD

summary(ur.df(USdata$Coco)) 

cols = c("FD log(Chocolate)" = "red")
ggplot(data=USdata, aes(x= date)) +
  theme_bw() +
  geom_line(aes(y=Coco, color="FD log(Chocolate)")) + 
  scale_colour_manual(name="Legend",values=cols) +
  labs(x = "Date", y = expression(Delta*Chocolate), 
       title = "Price of Cholocate (First Difference)")


#Question c
VARDATA <- ts(cbind(USdata$growth,USdata$Coco),names = c("growth","Coco"))
print(VARselect(VARDATA,type="const", lag.max = 5)) #1 lag


#Question d
var1 <- VAR(VARDATA ,p=1 , type="const")
print(summary(var1))
stargazer::stargazer(var1$varresult$growth, var1$varresult$Coco)

summary(var1)


#Qeustion e
p1 <- irf(var1, impulse = "growth", response = "growth", n.ahead = 10, boot = TRUE)
plot(p1, ylab = "Growth", main = "Growth shock to Growth")

p2 <- irf(var1, impulse = "growth", response = "Coco", n.ahead = 10, boot = TRUE)
plot(p2, ylab = "Chocolate", main = "Growth shock to Chocolate")

p3 <- irf(var1, impulse = "Coco", response = "Coco", n.ahead = 10, boot = TRUE)
plot(p3, ylab = "Chocolate", main = "Chocolate shock to Chocolate")

p4 <- irf(var1, impulse = "Coco", response = "growth", n.ahead = 10, boot = TRUE)
plot(p4, ylab = "Growth", main = "Chocolate shock to Growth")


##Question f
print(causality(var1, cause="Coco")$Granger)
print(causality(var1, cause="growth")$Granger)


##Question g
Varforecasts <- predict(var1, n.ahead = nrow(test))
Varfitted <- fitted(var1)
plot(Varforecasts)


test$Varfc <- Varforecasts$fcst$growth[,1]
test$Varlow <- Varforecasts$fcst$growth[,2]
test$Varhigh <- Varforecasts$fcst$growth[,3]

train$Varfit <- Varfitted[1:122, 1]
cols = c("Growth" = "blue", "AR(2)" = "red", "MA(2)" = "green", "VAR(1)" = "black")
ggplot(data = train, aes(x = date)) +
  geom_line(aes(y=growth, color="Growth"))+
  geom_line(aes(y=ar2fit, color="AR(2)")) +
  geom_line(aes(y=ma2fit, color="MA(2)")) +
  geom_line(aes(y=Varfit, color="VAR(1)")) +
  scale_colour_manual(name="Legend",values=cols) +
  theme_bw() +
  labs(x="Date",
       y="US Growth", title= "How well does AR(2), MA(2) and VAR(1) fit?")




cols = c("Growth" = "blue", "VAR(1)" = "black")
ggplot(data=test, aes(x=date)) +
  geom_line(aes(y=growth, color="Growth")) +
  geom_line(aes(y=Varfc, color="Var(1)")) +
  geom_line(aes(y=Varlow), color="black", linetype = "dashed") +
  geom_line(aes(y=Varhigh), color="black", linetype = "dashed") +
  scale_colour_manual(name="Legend",values=cols) +
  theme_bw() +
  labs(x="Date",
       y="US Growth Forecast", title= "Var(1) Forecasts")



cols = c("Growth" = "blue", "AR(2)" = "red", "MA(2)" = "green", "VAR(1)" = "black")
ggplot(data=test, aes(x=date)) +
  geom_line(aes(y=growth, color="Growth"))+
  geom_line(aes(y=ar2pred, color="AR(2)"))+
  geom_line(aes(y=ar2low), color="red", linetype = "dashed")+
  geom_line(aes(y=ar2high), color="red", linetype = "dashed") +
  geom_line(aes(y=ma2pred, color="MA(2)"))+
  geom_line(aes(y=ma2low), color="green", linetype = "dashed") +
  geom_line(aes(y=ma2high), color="green", linetype = "dashed") +
  geom_line(aes(y=Varfc, color="VAR(1)")) +
  scale_colour_manual(name="Legend",values=cols) +
  geom_line(aes(y=Varlow), color="black", linetype = "dashed") +
  geom_line(aes(y=Varhigh), color="black", linetype = "dashed") +
  theme_bw() +
  labs(x="Date",
       y="US Growth Forecast", title= "AR(2), MA(2) and VAR(1) Forecasts")
