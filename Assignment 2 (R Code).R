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

FRED <- read_dta("/Users/wilsontai/Downloads/assignment2.dta")
FRED  <- FRED %>%
  mutate(date=ymd(datestr)) %>% 
  as_tsibble(index=date)

FRED <- FRED %>% 
  mutate(growth = difference(log(GDPC1)), inflation = difference(log(USACPICORMINMEI))) 

FRED <- FRED %>%
  drop_na()
attach(FRED)

###Question 1
##checking unit roots first
summary(ur.df(FRED$FF, type = "trend")) ##null: unit root: fail to reject. Solution is to first difference
summary(ur.df(log(FRED$UNRATE), type = "trend")) ##null: unit root: fail to reject. Solution is to first difference

summary(ur.df(FRED$growth, type = "trend")) ##null: unit root: fail to reject. Solution is to first difference
summary(ur.df(FRED$inflation, type = "trend")) ##null: unit root: fail to reject. Solution is to first difference



cols <- c("Growth" = "blue")
ggplot(data=FRED, aes(x=date)) +
  theme_bw() +
  geom_line(aes(y= growth, color = "Growth")) + 
  scale_colour_manual(name="Legend",values=cols) +
  labs(x = "Date", y = "Growth", title = "Quarterly Growth for US")


cols <- c("Inflation" = "red")
ggplot(data=FRED, aes(x=date)) +
  theme_bw() +
  geom_line(aes(y= inflation, color = "Inflation")) + 
  scale_colour_manual(name="Legend",values=cols) +
  labs(x = "Date", y = "Inflation", title = "Quarterly Inflation for US")

cols <- c("Federal Funds Rate" = "green")
ggplot(data=FRED, aes(x=date)) +
  theme_bw() +
  geom_line(aes(y= FF , color = "Federal Funds Rate")) + 
  scale_colour_manual(name="Legend",values=cols) +
  labs(x = "Date", y = "Federal Funds Rate", title = "Quarterly Federal Funds Rate for US")

cols <- c("Unemployment" = "black")
ggplot(data=FRED, aes(x=date)) +
  theme_bw() +
  geom_line(aes(y= UNRATE, color = "Unemployment")) + 
  scale_colour_manual(name="Legend",values=cols) +
  labs(x = "Date", y = "Unemployment", title = "Quarterly Unemployment Rate for US")



##Question 2
VARDATA <- ts(cbind(FRED$FF,FRED$growth, FRED$inflation, FRED$UNRATE), 
              names = c("FF_rate","growth", "inflation", "unemp"))
print(VARselect(VARDATA,type="const", lag.max = 10)) #go for 2 lag: we have large data and SIC good for this


var1 <- VAR(VARDATA ,p=2 , type="const")
print(summary(var1)) 
stargazer::stargazer(var1$varresult$growth, var1$varresult$inflation, 
                     var1$varresult$FF_rate, var1$varresult$unemp)


##Question 3 
##by hand

##Question 4 and 5
r1 = c(1, NA, NA, 0)
r2 = c(NA, 1 , NA, 0)
r3 = c(0, 0, 1, NA)
r4 = c(0, 1, 0, NA)

B0_matrix = rbind(r1, r2 ,r3, r4)
B0_matrix

sf <- SVAR(var1, Amat = B0_matrix, estmethod = "direct", hessian = T, method = "BFGS")
print(summary(sf))
stargazer::stargazer(sf$varresult$growth, var1$varresult$inflation, 
                     var1$varresult$interest_rate, var1$varresult$unemp)
##Question 6


##Question 7


##Question 8
plot(irf(sf, response = "FF_rate"))
plot(fevd(sf, response = "unemp"))

