
# CROSSSECTION ------------------------------------------------------------

# LIBRARIES ---------------------------------------------------------------

library(readxl)
library(leaps)
library(glmnet)
library(forecast)
library(ggfortify)
library(tseries)
library(timeSeries)
library(ggplot2)
library(scales)
library(lubridate)
library(writexl)
library(tibble)

# CROSS SECTION -----------------------------------------------------------
## Dataset -----------------------------------------------------------------


DS_crossection <- read_excel("DS_crossection.xlsx")
colnames(DS_crossection) <- DS_crossection[1,]
DS_crossection <- DS_crossection[-1,-1]
colnames(DS_crossection)[11] <- 'fr'
colSums(is.na(DS_crossection))
rowSums(is.na((DS_crossection)))
DS_crossection <- as.data.frame(sapply(DS_crossection, as.numeric))
#s_DS_crossection <- scale(DS_crossection)
#s_DS_crossection <- as.data.frame(s_DS_crossection)

cor.mat <- cor(DS_crossection)

## Models ------------------------------------------------------------------



#basic models
#mod1 <- regsubsets(cr~.,data = DS_crossection,nvmax = ncol(DS_crossection))
#smod1 <- summary(mod1)
#which.min(smod1$adjr2) #1
#coef(mod1, 1) #fc
#which.min(smod1$bic) #6
#coef(mod1, 6) #gdp, fc, fert, imp, exp, co2


#Ridge and Lasso
#xx2 <- model.matrix(cr ~ ., data = DS_crossection)[,-1]
#y2 <- DS_crossection$cr
#ridge.2 <- glmnet(xx2, y2, alpha = 0)
#set.seed(1)
#cv.ridge.2 <- cv.glmnet(xx2, y2, alpha = 0) #MSE 2.719e+10      
#lambda.ridge.2 <- cv.ridge.2$lambda.1se #231068.7
#lasso.2 <- glmnet(xx2, y2, alpha = 1)
#set.seed(1)
#cv.lasso.2 <- cv.glmnet(xx2, y2, alpha = 1) #MSE 2.545e+10  
#lambda.lasso.2 <- cv.lasso.2$lambda.1se #16685  



### Create the sample -------------------------------------------------------


set.seed(1)
divide <- sample(nrow(DS_crossection), nrow(DS_crossection)/2)
train <- DS_crossection[divide,]
test <- DS_crossection[-divide,]

mod.train <- lm(cr ~ ., data = train)
mean((test$cr - predict(mod.train, newdata = test))^2) #27,142,524,705


### Lasso model w/ validation set -------------------------------------------


xx.tr <- model.matrix(cr ~ ., data = train)[,-1]
y.tr <- train$cr
#xx.te <- model.matrix(cr ~ ., data = test)[,-1]
lasso.mod <- glmnet(xx.tr, y.tr, alpha = 1)
set.seed(1)
cv.lasso.mod <- cv.glmnet(xx.tr, y.tr, alpha = 1)
cv.lasso.mod #3 non zero coeff.
lambda <- cv.lasso.mod$lambda.1se #57300
lasso.pred <- predict(lasso.mod, s = lambda, newx = xx.tr)
mean((train$cr - lasso.pred)^2) #28,922,093,012
coef(cv.lasso.mod) #pop, coe, fc 

modL <- lm(cr ~ pop + coe + fc, data = train)
summary(modL) #Adjusted R-squared:  0.9589 
anova(modL, mod.train) #refuse H0

### Ridge w/ validation set -------------------------------------------------


xx.train <- model.matrix(cr ~ ., data = train)[,-1]
#xx.test <- model.matrix(cr ~ ., data = test)[,-1]
y.train <- train$cr
ridge.2 <- glmnet(xx.train, y.train, alpha = 0)
set.seed(1)
cv.ridge.2 <- cv.glmnet(xx.train, y.train, alpha = 0) #29 non zero
lambda.ridge.2 <- cv.ridge.2$lambda.1se #0.3966429
pred.ridge <- predict(ridge.2, s = lambda.ridge.2, newx = xx.train)
coef(cv.ridge.2) 
mean((train$cr- pred.ridge)^2) #27,707,033,115

modR <- lm(cr ~ ., data = train)
summary(modR) #Adjusted R-squared:  0.9712
mean((train$cr - predict(modR, newdata = train))^2) #13,643,021,529


### Validation models  --------------------------------------------


#### Forward -----------------------------------------------------------------


fwd.mod <- regsubsets(cr ~ ., data = train, nvmax = 29, method = "forward")
train.mat <- model.matrix(cr ~ ., data = train) 
mse <- rep(NA, 29)
for (i in 1:29) {
  beta <- coef(fwd.mod, id = i)
  fwd.pred <- train.mat[, names(beta)]%*%beta
  mse[i] <- mean((train$cr - fwd.pred)^2)
}
which.min(mse) #29 pred.
mse[29] #13,643,021,529
fwd.mod <- regsubsets(cr ~ ., data = train, nvmax = 29, method = "forward")
coef(fwd.mod, 29) #all the regressors kept into the models

s.fwdmod <- summary(fwd.mod)
which.max(s.fwdmod$adjr2) #19
s.fwdmod$adjr2[19] #0.9726691
coef(fwd.mod, 19) #gdp + coe + fc + mar + le + fert + fr + or + rgdp + imp + exp +
#eal3 + hdi + eyos + myos + gnipc + gdi + co2 + mfpc

modfw19 <- lm(cr ~ gdp + coe + fc + mar + le + fert + fr + or + rgdp + imp + exp +
                eal3 + hdi + eyos + myos + gnipc + gdi + co2 + mfpc, data = train)
anova(modfw19, mod.train) #not refuse H0
summary(modfw19) #Adjusted R-squared:  0.9733
mean((train$cr - predict(modfw19, newdata = train))^2) #14,489,443,932




#### Backward ----------------------------------------------------------------

bwd.mod <- regsubsets(cr ~ ., data = train, nvmax = 29, method = "backward")
train.mat2 <- model.matrix(cr ~ ., data = train) 
mse2 <- rep(NA, 29)
for (i in 1:29) {
  beta2 <- coef(bwd.mod, id = i)
  bwd.pred <- train.mat2[, names(beta2)]%*%beta2
  mse2[i] <- mean((train$cr - bwd.pred)^2)
}
which.min(mse2) #29 pred.
mse2[29] #13,643,021,529
bwd.mod <- regsubsets(cr ~ ., data = train, nvmax = 29, method = "backward")
coef(bwd.mod, 29) #all the regressors kept in the model

s.bwdmod <- summary(bwd.mod)
which.max(s.bwdmod$adjr2) #16
s.bwdmod$adjr2[16] #0.9738555
coef(bwd.mod, 16) #coe + fc + el + rgdp + imp + exp + eal1 + eal2 + eal3 + eyos 
#+ gnipc + gdi + gii + mmr + co2 + mfpc

modbw16 <- lm(cr ~ coe + fc + el + rgdp + imp + exp + eal1 + eal2 + eal3 + eyos 
              + gnipc + gdi + gii + mmr + co2 + mfpc, data = train)
anova(modbw16, mod.train) #not refuse H0
summary(modbw16) #Adjusted R-squared:  0.9739
mean((train$cr - predict(modbw16, newdata = train))^2) #14,298,151,125




#### Best subset (selected)----------------------------------------------------------------

bss.mod <- regsubsets(cr ~ ., data = train, nvmax = 29, method = "exhaustive")
train.mat3 <- model.matrix(cr ~ ., data = train) 
mse3 <- rep(NA, 29)
for (i in 1:29) {
  beta3 <- coef(bss.mod, id = i)
  bss.pred <- train.mat3[, names(beta3)]%*%beta3
  mse3[i] <- mean((train$cr - bss.pred)^2)
}
which.min(mse3) #29 pred.
mse3[29] #13,643,021,529
bss.mod <- regsubsets(cr ~ ., data = train, nvmax = 29, method = "exhaustive")
coef(bss.mod, 29) #all the regressors kept in the model

s.bssmod <- summary(bss.mod)
which.max(s.bssmod$adjr2) #17
s.bssmod$adjr2[17] #0.9738586
coef(bss.mod, 17) #coe + fc + el + rgdp + imp + exp + at + eal1 + eal2 + eal3 + 
#eyos + gnipc + gdi + gii + mmr + co2 + mfpc

modbs17 <- lm(cr ~ coe + fc + el + rgdp + imp + exp + at + eal1 + eal2 + eal3 + 
                eyos + gnipc + gdi + gii + mmr + co2 + mfpc, data = train)
anova(modbs17, mod.train) #not refuse H0
summary(modbs17) #Adjusted R-squared:  0.9739
mean((train$cr - predict(modbs17, newdata = train))^2) #14,150,595,302
mean((test$cr - predict(modbs17, newdata = test))^2) #26,362,944,877
plot(modbs17) #Modello vincente

### Econometric path (selected) --------------------------------------------------------


mod.eco1 <- lm(cr ~ pop + gdp + coe + fc + rd + or + co2, data = train)
summary(mod.eco1) #Adjusted R-squared:  0.964 
anova(mod.eco1, mod.train) #not refused at 0.001
mean((train$cr - predict(mod.eco1, newdata = train))^2) #21,483,946,274

mod.eco2 <- lm(cr ~ pop + gdp + coe + fc + rd + co2, data = train)
summary(mod.eco2) #Adjusted R-squared:  0.9643
anova(mod.eco2, mod.train) #not refused at 0.001
mean((train$cr - predict(mod.eco2, newdata = train))^2) #21,543,655,213
mean((test$cr - predict(mod.eco2, newdata = test))^2) #36,328,891,512

mod.eco3 <- lm(cr ~ pop + gdp + coe + fc + co2, data = train)
summary(mod.eco3) #Adjusted R-squared:  0.9641
anova(mod.eco3, mod.train) #not refused at 0.001
mean((train$cr - predict(mod.eco3, newdata = train))^2) #21,855,224,748


## LOG DATASET --------------------------------------------------------------


DS_logcs <- DS_crossection
names(DS_logcs)[c(2:5, 11, 13, 15:17, 28)] <- c("logpop", "loggdp", "logcoe",
                                                "logfc", "logfr", "logdens", "logimp",
                                                "logexp", "logat", "logabr")
DS_logcs[,c(2:5, 11, 13, 15:17, 28)] <- log(DS_crossection[,c(2:5, 11, 13, 15:17, 28)])
summary(DS_logcs)

### Division in training and test -------------------------------------------


set.seed(1)
divide.l <- sample(nrow(DS_logcs), nrow(DS_logcs)/2)
train.l <- DS_logcs[divide,]
test.l <- DS_logcs[-divide,]


## Log models --------------------------------------------------------------


mod.train.l <- lm(cr ~ ., train.l)
mean((train.l$cr - predict(mod.train.l, newdata = train.l))^2) #59,624,970,193


modA <- lm(cr ~ logpop + loggdp, data = train.l)
mean((train.l$cr - predict(modA, newdata = train.l))^2) #275,605,096,739
summary(modA) #Adjusted R-squared:  0.559
anova(modA, mod.train.l) #refuse H0


ModL3l <- lm(cr ~ logpop + logcoe + logfc, data = train.l) #w/ the regressors 
#suggested by the lasso model
mean((train.l$cr - predict(ModL3l, newdata = train.l))^2) #262,734,879,148
summary(ModL3l) #Adjusted R-squared:  0.5758
anova(ModL3l, mod.train.l) #refuse H0 

modfw19l <- lm(cr ~ loggdp + logcoe + logfc + mar + le + fert + logfr + or + 
                 rgdp + logimp + logexp + eal3 + hdi + eyos + myos + gnipc + 
                 gdi + co2 + mfpc, data = train.l)
#w/ the regressors suggested by the forward model
summary(modfw19l) #Adjusted R-squared:  0.612
anova(modfw19l, mod.train.l) #refuse H0

modbw16l <- lm(cr ~ logcoe + logfc + el + rgdp + logimp + logexp + eal1 + eal2 
               + eal3 + eyos + gnipc + gdi + gii + mmr + co2 + mfpc, data = train.l)
summary(modbw16l) #Adjusted R-squared:  0.6403 
anova(modbw16l, mod.train.l) #refuse H0

modbs17l <- lm(cr ~ logcoe + logfc + el + rgdp + logimp + logexp + logat + eal1 
               + eal2 + eal3 + eyos + gnipc + gdi + gii + mmr + co2 + mfpc,
               data = train.l)
summary(modbs17l) #Adjusted R-squared:  0.6387
anova(modbs17l, mod.train.l) #refuse H0

### Lasso model w/ validation set -------------------------------------------


xx.tr.l <- model.matrix(cr ~ ., data = train.l)[,-1]
y.tr.l <- train.l$cr
#xx.te.l <- model.matrix(cr ~ ., data = test.l)[,-1]
lasso.mod.l <- glmnet(xx.tr.l, y.tr.l, alpha = 1)
set.seed(1)
cv.lasso.mod.l <- cv.glmnet(xx.tr.l, y.tr.l, alpha = 1)
cv.lasso.mod.l #11 non zero coeff.
lambda.l <- cv.lasso.mod.l$lambda.1se #0.009538737
lasso.pred.l <- predict(lasso.mod.l, s = lambda.l, newx = xx.tr.l)
mean((train.l$cr - lasso.pred.l)^2) #108,821,095,144
coef(cv.lasso.mod.l) #logfc, el, logfr, or, logat, eal1, eal3, myos, logabr, co2, mfpc


modL11l <- lm(cr ~ logfc + el + logfr + or + logat + eal1 + eal3 + myos + logabr 
              + co2 + mfpc, data = train.l)
anova(modL11l, mod.train.l) #cannot refuse H0 at 0.001 significance level
summary(modL11l) #Adjusted R-squared:  0.8378
mean((train.l$cr - predict(modL11l, newdata = train.l))^2) #93,206,570,506



### Forward model (selected)-----------------------------------------------------------


fwd.mod.l <- regsubsets(cr ~ ., data = train.l, nvmax = 29, method = "forward")
train.mat.l <- model.matrix(cr ~ ., data = train.l) 
mse.l <- rep(NA, 29)
for (i in 1:29) {
  beta.l <- coef(fwd.mod.l, id = i)
  fwd.pred.l <- train.mat.l[, names(beta.l)]%*%beta.l
  mse.l[i] <- mean((train.l$cr - fwd.pred.l)^2)
}
which.min(mse.l) #29 pred.
mse.l[29] #59,624,970,193
fwd.mod.l <- regsubsets(cr ~ ., data = train.l, nvmax = 29, method = "forward")
coef(fwd.mod.l, 29) #all the regressors kept into the models

s.fwdmod.l <- summary(fwd.mod.l)
which.max(s.fwdmod.l$adjr2) #18
s.fwdmod.l$adjr2[18] #0.88072
coef(fwd.mod.l, 18) #logcoe + logfc + el + le + logfr + or + rgdp + logimp + 
#logexp + eal2 + eal3 + hdi + eyos + myos + gdi + gii + logabr + co2

modfw18.l <- lm(cr ~ logcoe + logfc + el + le + logfr + or + rgdp + logimp + 
                  logexp + eal2 + eal3 + hdi + eyos + myos + gdi + gii + logabr 
                + co2, data = train.l)
anova(modfw18.l, mod.train.l) #not refuse H0
summary(modfw18.l) #Adjusted R-squared:  0.8807 
mean((train.l$cr - predict(modfw18.l, newdata = train.l))^2) #63,901,656,667
mean((test.l$cr - predict(modfw18.l, newdata = test.l))^2) #1.54962e+11

### Backward model (selected)-------------------------------------------------------

bwd.mod.l <- regsubsets(cr ~ ., data = train.l, nvmax = 29, method = "backward")
train.mat.l2 <- model.matrix(cr ~ ., data = train.l) 
mse.l2 <- rep(NA, 29)
for (i in 1:29) {
  beta.l2 <- coef(bwd.mod.l, id = i)
  bwd.pred.l <- train.mat.l2[, names(beta.l2)]%*%beta.l2
  mse.l2[i] <- mean((train.l$cr - bwd.pred.l)^2)
}
which.min(mse.l2) #29 pred.
mse.l2[29] #59,624,970,193
bwd.mod.l <- regsubsets(cr ~ ., data = train.l, nvmax = 29, method = "backward")
coef(bwd.mod.l, 29) #all the regressors kept into the models

s.bwdmod.l <- summary(bwd.mod.l)
which.max(s.bwdmod.l$adjr2) #19
s.bwdmod.l$adjr2[19] #0.8845281
coef(bwd.mod.l, 19) #logpop, loggdp, logcoe, logfr, or, logdens, rgdp, logimp,
#logexp, eal1, hdi, eyos, myos, gii, logabr

modbw19.l <- lm(cr ~ logpop + loggdp + logcoe + logfr + or + logdens + rgdp + 
                  logimp + logexp + eal1 + eal2 + hdi + eyos + myos + gdi + gii
                + mmr + logabr + co2, data = train.l)
anova(modbw19.l, mod.train.l) #not refuse H0
summary(modbw19.l) #Adjusted R-squared:   0.8845281
mean((train.l$cr - predict(modbw19.l, newdata = train.l))^2) #61,217,163,482
mean((test.l$cr - predict(modbw19.l, newdata = test.l))^2) #147,317,726,713


### Best subset -------------------------------------------------------------

bs.mod.l <- regsubsets(cr ~ ., data = train.l, nvmax = 29, method = "exhaustive")
train.mat.l3 <- model.matrix(cr ~ ., data = train.l) 
mse.l3 <- rep(NA, 29)
for (i in 1:29) {
  beta.l3 <- coef(bs.mod.l, id = i)
  bs.pred.l <- train.mat.l3[, names(beta.l3)]%*%beta.l3
  mse.l3[i] <- mean((train.l$cr - bs.pred.l)^2)
}
which.min(mse.l3) #29 pred.
mse.l3[29] #59,624,970,193
bs.mod.l <- regsubsets(cr ~ ., data = train.l, nvmax = 29, method = "exhaustive")
coef(bs.mod.l, 29) #all the regressors kept into the models

s.bsmod.l <- summary(bs.mod.l)
which.max(s.bsmod.l$adjr2) #19
s.bsmod.l$adjr2[19] #0.8845281
coef(bs.mod.l, 19) #logpop + loggdp + logcoe + logfr + or + logdens + rgdp + 
#logimp + logexp + eal1 + eal2 + hdi + eyos + myos + gdi + gii + mmr + logabr + co2
#same model as in backward method

modbs19.l <- lm(cr ~ logpop + loggdp + logcoe + logfr + or + logdens + rgdp + 
                  logimp + logexp + eal1 + eal2 + hdi + eyos + myos + gdi + gii
                + mmr + logabr + co2, data = train.l)
summary(modbs19.l) #Adjusted R-squared:   0.8845281
anova(modbs19.l, mod.train.l) #not refuse H0

### Econometric path --------------------------------------------------------

mod.eco1.l <- lm(cr ~ logpop + loggdp + logcoe + logfc + rd + or + co2, data = train.l)
summary(mod.eco1.l) #Adjusted R-squared:  0.6462
anova(mod.eco1.l, mod.train.l) #refused H0

mod.eco2.l <- lm(cr ~ logpop + loggdp + logfc + rd + or + co2, data = train.l)
summary(mod.eco2.l) #Adjusted R-squared:  0.6478
anova(mod.eco2.l, mod.train.l) #refused H0

mod.eco3.l <- lm(cr ~ logpop + loggdp + logfc + rd + co2, data = train.l)
summary(mod.eco3.l) #Adjusted R-squared:  0.6471 
anova(mod.eco3.l, mod.train.l) #refused H0

mod.eco4.l <- lm(cr ~ logpop + loggdp + logfc + co2, data = train.l)
anova(mod.eco4.l, mod.eco1.l) #not refused H0
summary(mod.eco4.l) #Adjusted R-squared:  0.6466  
mean((train.l$cr - predict(mod.eco4.l, newdata = train.l))^2) #216,909,466,622



# TIME SERIES -------------------------------------------------------------


# LIBRARIES ---------------------------------------------------------------
library(readxl)
library(leaps)
library(glmnet)
library(forecast)
library(ggfortify)
library(tseries)
library(timeSeries)
library(ggplot2)
library(scales)
library(lubridate)
library(writexl)
library(tibble)
# TIME SERIES -------------------------------------------------------------
DS_timeseries_Month <- read_excel("dataset_time_series.xlsx",col_types = c("date", "numeric", "numeric", "numeric"))
## FIRST APPROACH: BMW CAR REGISTRATIONS FORECAST ---------------------------------------------------------------------
bmw <- ts(as.vector(DS_timeseries_Month$BMW_Month), start = 1990, frequency = 12)
bmw <- log(bmw)
### Time-series BMW ---------------------------------------------------------
autoplot(bmw) +
  scale_y_continuous(labels = label_number(big.mark = ".")) +
  ylab("Car Registrations") +
  xlab("Year") +
  ggtitle("BMW Car Registrations per Month from 1990 to 2019")

### time-series with MA5 ----------------------------------------------------
start_year <- 1990
frequency <- 12
dates <- seq(as.Date(paste(start_year, "01", "01", sep = "-")), by = "month", length.out = length(bmw))
bmw_df <- data.frame(Date = dates, Value = as.vector(bmw))
ma5 <- ma(bmw_df$Value, order = 5)
ggplot(bmw_df, aes(x = Date, y = Value)) +
  geom_line(color = "black", linetype = "solid") +
  geom_line(aes(y = ma5), color = "red", linetype = "dashed") +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  labs(x = "Year", y = "Car Registrations", title = "Monthly Average BMW Car Registrations with MA-5") +
  theme_minimal()

### Seasonal Plot -----------------------------------------------------------
ggseasonplot(bmw, col = rainbow(12)) +
  scale_y_continuous(labels = label_number(big.mark = "."))
#we see seasonalities
#ACF ---------------------------------------------------------------------
ggAcf(bmw, lag.max = 300)
ggPacf(bmw, lag.max = 300)
#Modern Decomposition ----------------------------------------------------
bmw %>%  
  stl(t.window = 13, s.window = 12, robust = T) %>%
  autoplot() + xlab("Year") + scale_y_continuous(labels = label_number(big.mark = ".")) +
  ggtitle("Monthly average BMW Car Registrations Concentration")

#we can see we have a clear trend and seasonality 

### Trend + Seasonality ------------------------------------
n.bmw <- length(bmw)
t.bmw <- 1:n.bmw
d <- diag(12) 
s.bmw <- apply(replicate(n.bmw/12,d),1,"c")
lr_bmw2 <- lm(bmw ~ t.bmw+s.bmw)
summary(lr_bmw2)
#the trend is significant and also some seasonalities
y2 <- fitted(lr_bmw2)
y.ts <- ts(as.vector(y2),start=1990,frequency = 12)
ggplot(bmw_df, aes(x = Date, y = Value)) +
  geom_line(color = "black", linetype = "solid") +
  geom_line(aes(y = y.ts), color = "red", linetype = "solid") +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  labs(x = "Year", y = "Car Registrations", title = "Monthly Average BMW Car Registrations") +
  theme_minimal()
#significant trend and some significant seasonalities

### Only Trend -------------------------------------------------------------
lr_bmw <- lm(bmw ~ t.bmw) 
summary(lr_bmw)#significant trend
#Plot linear trend
bmwh <- fitted(lr_bmw)
ggplot(bmw_df, aes(x = Date, y = Value)) +
  geom_line(color = "black", linetype = "solid") +
  geom_line(aes(y = bmwh), color = "red", linetype = "solid") +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  labs(x = "Year", y = "Car Registrations", title = "Monthly Average BMW Car Registrations") +
  theme_minimal()

### Only Seasonality ---------------------------------------------------------
lr_bmw3 <- lm(bmw ~ s.bmw)
summary(lr_bmw3)# some significant seasonalities
y3 <- fitted(lr_bmw3)
y.ts <- ts(as.vector(y3),start=1990,frequency = 12)
ggplot(bmw_df, aes(x = Date, y = Value)) +
  geom_line(color = "black", linetype = "solid") +
  geom_line(aes(y = y.ts), color = "red", linetype = "solid") +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  labs(x = "Year", y = "Car Registrations", title = "Monthly Average BMW Car Registrations") +
  theme_minimal()
#Diagnostic linear trend regression WITH SEASONAL AND TREND MODEL
sum(abs(residuals(lr_bmw3)))
sum(abs(residuals(lr_bmw2))) #lowest sum of the residuals
sum(abs(residuals(lr_bmw)))
#WHICH ONE IS BETTER?? THE ONE MORE ACCURATE IS THE SEASONAL+TREND
### Diagnostic and Dickey-Fuller -----------------------------------------------------------
adf.test(bmw)
#we reject the null hypothesis
# Insample ----------------------------------------------------------------
ix.is <- 1:300
ix.os <- 301:312
n.bmw <- length(bmw)
t.bmw <- 1:n.bmw
d <- diag(12) 
s.bmw <- apply(replicate(n.bmw/12,d),1,"c")
x.bmw <- cbind(t.bmw,s.bmw)
# For cycles for model on all dataset----------------------------------------------------
i=0
j=0
k=0
l=0
m=0
n=0
SARIMAfor_bmw<- as.data.frame(matrix(0, nrow = 10, ncol = 11))
colnames(SARIMAfor_bmw) <- c("AR", "I", "MA","SAR", "SI", "SMA", "Bic", "Aic", "Mse", "Mae", "RMSE")
index <- 1
for (i in 0:2) {
  for (j in 0:0) {
    for (k in 0:2 ) {
      for (l in 0:2) {
        for (m in 1:1) {
          for (n in 0:2 ) {
            SARIMAfor_bmw[index,1] <- i
            SARIMAfor_bmw[index,2] <- j
            SARIMAfor_bmw[index,3] <- k
            SARIMAfor_bmw[index,4] <- l
            SARIMAfor_bmw[index,5] <- m
            SARIMAfor_bmw[index,6] <- n
            fit1 <- arima(bmw[ix.is], order=c(i,j,k),seasonal = list(order=c(l,m,n),period=12))
            SARIMAfor_bmw[index,7] <- BIC(fit1)
            SARIMAfor_bmw[index,8] <- AIC(fit1)
            SARIMAfor_bmw[index,9] <- mean(residuals(fit1)^2)
            SARIMAfor_bmw[index,10] <- mean(abs(residuals(fit1)))
            SARIMAfor_bmw[index,11] <- sqrt(mean(residuals(fit1)^2))
            index <- index+1}}}}}}
#when I compute the 0:2 cycle for every 4 index, it return me only 78, the last is missing because
#Errore in optim(init[mask], SARIMAfn, method = optim.method, hessian = TRUE, : valore iniziale in 'vmmin' non finito
view(SARIMAfor_bmw)
#I choose the following models 
#SARIMA(1,0,2)(0,1,1)
#SARIMA(1,0,2)(2,1,2)
#SARIMA(2,0,2)(0,1,1)
#SARIMA(0,0,0)(0,1,0)
j=0
i=0
k=0
ARMAfor_bmw<- as.data.frame(matrix(0, nrow = 9, ncol = 7))
colnames(ARMAfor_bmw) <- c("AR", "I", "MA", "Bic", "Aic", "Mse", "Mae")
index <- 1
x.bmw <- cbind(t.bmw,s.bmw)
for (i in 0:2) {
  for (j in 0:0) {
    for (k in 0:2 ) {
      ARMAfor_bmw[index,1] <- i
      ARMAfor_bmw[index,2] <- j
      ARMAfor_bmw[index,3] <- k
      fit1 <- Arima(bmw[ix.is], xreg=x.bmw[ix.is], order=c(i,j,k), include.mean = F)
      ARMAfor_bmw[index,4] <- BIC(fit1)
      ARMAfor_bmw[index,5] <- AIC(fit1)
      ARMAfor_bmw[index,6] <- mean(residuals(fit1)^2)
      ARMAfor_bmw[index,7] <- mean(abs(residuals(fit1)))
      index <- index+1}}}
#the result are very similar so we try with ARMA(12,12)
#we want to see the different models
view(ARMAfor_bmw)
# I choose the following models
#ARMA(2,2)
#ARMA(0,0)
#AR(1)
#MA(1)
#SARIMA -----------------------------------------------------
model_bmw<- as.data.frame(matrix(0, nrow = 8, ncol = 8))
colnames(model_bmw) <- c("AR", "I", "MA","SAR", "SI", "SMA", "Mse", "TotMSe")
#model 1
i <- 0
mse <- 0
totmse <- 0
n <- 300
for (i in 0:48){
  fit.os1 <- Arima(bmw[(1:n+i)],  order=c(1,0,2), seasonal = list(order=c(0,1,1),period=12), include.mean = F)
  yh.os <- forecast(object = fit.os1, h=12)
  mse <- mse+mean((yh.os$mean-bmw[ix.os+i])^2)
  totmse <- totmse+(sum(yh.os$mean)-sum(bmw[ix.os+i]))^2
  print(c(i,mean((yh.os$mean-bmw[ix.os+i])^2),(sum(yh.os$mean)-sum(bmw[ix.os+i]))^2))}
model_bmw[1,1:6] <- c(1,0,2,0,1,1)
model_bmw[1,7] <- mse/49
model_bmw[1,8] <- totmse/49

#model 2
i <- 0
mse <- 0
totmse <- 0
n <- 300
for (i in 0:48){
  fit.os2 <- Arima(bmw[(1:n+i)],  order=c(1,0,2), seasonal = list(order=c(2,1,2),period=12), include.mean = F)
  yh.os2 <- forecast(object = fit.os2, h=12)
  mse <- mse+mean((yh.os2$mean-bmw[ix.os+i])^2)
  totmse <- totmse+(sum(yh.os2$mean)-sum(bmw[ix.os+i]))^2
  print(c(i,mean((yh.os2$mean-bmw[ix.os+i])^2),(sum(yh.os2$mean)-sum(bmw[ix.os+i]))^2))}
model_bmw[2,1:6] <- c(1,0,2,2,1,2)
model_bmw[2,7] <- mse/49
model_bmw[2,8] <- totmse/49

#model 3
i <- 0
mse <- 0
totmse <- 0
n <- 300
for (i in 0:48){
  fit.os3 <- Arima(bmw[(1:n+i)],  order=c(2,0,2), seasonal = list(order=c(0,1,1),period=12), include.mean = F)
  yh.os3 <- forecast(object = fit.os3, h=12)
  mse <- mse+mean((yh.os3$mean-bmw[ix.os+i])^2)
  totmse <- totmse+(sum(yh.os3$mean)-sum(bmw[ix.os+i]))^2
  print(c(i,mean((yh.os3$mean-bmw[ix.os+i])^2),(sum(yh.os3$mean)-sum(bmw[ix.os+i]))^2))}
model_bmw[3,1:6] <- c(2,0,2,0,1,1)
model_bmw[3,7] <- mse/49
model_bmw[3,8] <- totmse/49

#model 4
i <- 0
mse <- 0
totmse <- 0
n <- 300
for (i in 0:48){
  fit.os4 <- Arima(bmw[(1:n+i)],  order=c(0,0,0), seasonal = list(order=c(0,1,0),period=12), include.mean = F)
  yh.os4 <- forecast(object = fit.os4, h=12)
  mse <- mse+mean((yh.os4$mean-bmw[ix.os+i])^2)
  totmse <- totmse+(sum(yh.os4$mean)-sum(bmw[ix.os+i]))^2
  print(c(i,mean((yh.os4$mean-bmw[ix.os+i])^2),(sum(yh.os4$mean)-sum(bmw[ix.os+i]))^2))}
model_bmw[4,1:6] <- c(0,0,0,0,1,0)
model_bmw[4,7] <- mse/49
model_bmw[4,8] <- totmse/49

#we compare different models in the first example just to see how they behave in 301:312 forecast
plot(ix.os,bmw[ix.os],type="l")
lines(yh.os4$mean, col="orange")
lines(yh.os3$mean, col="green")
lines(yh.os2$mean, col="blue")
lines(yh.os$mean, col="red")
# ARMA with deterministic seasonalities -----------------------------------

# Forecast out-of-sample
#model1 ARMA(2,2)
i <- 0
mse <- 0
totmse <- 0
n <- 300
for (i in 0:48){
  fit.is <- Arima(bmw[(1:n+i)], xreg=x.bmw[(1:n+i),], order=c(2,0,2), include.mean = F)
  yh.os <- forecast(object = fit.is, h=12, xreg = x.bmw[ix.os+i,])
  mse <- mse+ mean((yh.os$mean-bmw[ix.os+i])^2)
  totmse <- totmse+(sum(yh.os$mean)-sum(bmw[ix.os+i]))^2
  print(c(i,mean((yh.os$mean-bmw[ix.os+i])^2),(sum(yh.os$mean)-sum(bmw[ix.os+i]))^2))}
model_bmw[5,1:6] <- c(2,0,2,0,0,0)
model_bmw[5,7] <- mse/49
model_bmw[5,8] <- totmse/49

#model2 ARMA(0,0)
i <- 0
mse <- 0
totmse <- 0
n <- 300
for (i in 0:48){
  fit.is2 <- Arima(bmw[(1:n+i)], xreg=x.bmw[(1:n+i),], order=c(0,0,0), include.mean = F)
  yh.os2 <- forecast(object = fit.is2, h=12, xreg = x.bmw[ix.os+i,])
  mse <- mse+ mean((yh.os2$mean-bmw[ix.os+i])^2)
  totmse <- totmse+(sum(yh.os2$mean)-sum(bmw[ix.os+i]))^2
  print(c(i,mean((yh.os2$mean-bmw[ix.os+i])^2),(sum(yh.os2$mean)-sum(bmw[ix.os+i]))^2))}
model_bmw[6,1:6] <- c(0,0,0,0,0,0)
model_bmw[6,7] <- mse/49
model_bmw[6,8] <- totmse/49

#model3 AR(1)
i <- 0
mse <- 0
totmse <- 0
n <- 300
for (i in 0:48){
  fit.is3 <- Arima(bmw[(1:n+i)], xreg=x.bmw[(1:n+i),], order=c(1,0,0), include.mean = F)
  yh.os3 <- forecast(object = fit.is3, h=12, xreg = x.bmw[ix.os+i,])
  mse <- mse+ mean((yh.os3$mean-bmw[ix.os+i])^2)
  totmse <- totmse+(sum(yh.os3$mean)-sum(bmw[ix.os+i]))^2
  print(c(i,mean((yh.os3$mean-bmw[ix.os+i])^2),(sum(yh.os3$mean)-sum(bmw[ix.os+i]))^2))}
model_bmw[7,1:6] <- c(1,0,0,0,0,0)
model_bmw[7,7] <- mse/49
model_bmw[7,8] <- totmse/49

#model4 MA(1)
i <- 0
mse <- 0
totmse <- 0
n <- 300
for (i in 0:48){
  fit.is4 <- Arima(bmw[(1:n+i)], xreg=x.bmw[(1:n+i),], order=c(0,0,1), include.mean = F)
  yh.os4 <- forecast(object = fit.is4, h=12, xreg = x.bmw[ix.os+i,])
  mse <- mse+ mean((yh.os4$mean-bmw[ix.os+i])^2)
  totmse <- totmse+(sum(yh.os4$mean)-sum(bmw[ix.os+i]))^2
  print(c(i,mean((yh.os4$mean-bmw[ix.os+i])^2),(sum(yh.os4$mean)-sum(bmw[ix.os+i]))^2))}
model_bmw[8,1:6] <- c(0,0,1,0,0,0)
model_bmw[8,7] <- mse/49
model_bmw[8,8] <- totmse/49

#WE compare the different models just to see
plot(ix.os,bmw[ix.os],type="l")
lines(yh.os4$mean, col="orange")
lines(yh.os3$mean, col="green")
lines(yh.os2$mean, col="blue")
lines(yh.os$mean, col="red")
#AND WE HAVE TO CHOOSE THE BEST ONE

# BEST BMW MODEL ----------------------------------------------------------

view(model_bmw)
#the best model according to MSE is SARIMA(1,0,2,2,1,2)
#the best model according to TOTMSE is ARMA(2,2) with deterministic seasonalities and trend

fit.os2 <- Arima(bmw[ix.is],  order=c(1,0,2), seasonal = list(order=c(2,1,2),period=12), include.mean = F)
yh.os2 <- forecast(object = fit.os2, h=12)
plot(ix.os,exp(bmw[ix.os]),type="l")
lines(exp(yh.os2$mean), col=2)
mse <- mean((exp(yh.os3$mean)-exp(bmw[ix.os])^2))
(sum(exp(yh.os3$mean))-sum(exp(bmw[ix.os])))
#these are natural numbers  in the first case of 301:312 estimation
#the overall error over the next 12 months is 81882.62
# 
fit.os2 <- Arima(bmw,  order=c(1,0,2), seasonal = list(order=c(2,1,2),period=12), include.mean = F)
start_year <- 1990
frequency <- 12
dates <- seq(as.Date(paste(start_year, "01", "01", sep = "-")), by = "month", length.out = length(bmw))
bmw_df <- data.frame(Date = dates, Value = as.vector(bmw))
ma5 <- (fitted(fit.os2))
ggplot(bmw_df, aes(x = Date, y = Value)) +
  geom_line(color = "black", linetype = "solid",lwd=0.8) +
  geom_line(aes(y = ma5), color = "red", linetype = "solid",lwd=0.8) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  labs(x = "Year", y = "Car Registrations", title = "Monthly Average BMW Car Registrations with fitted model SARIMA(1,0,2)(2,1,2)") +
  theme_minimal()
## SECOND APPROACH: TOTAL CAR REGISTRATIONS (TCR) & BMW MARKETE SHARE (BMS) ---------------------------------------------------------------------
### TCR ---------------------------------------------------------------------
tcr <- ts(as.vector(DS_timeseries_Month$Total_Month), start = 1990, frequency = 12)
tcr <- log(tcr)
#### Time-Series ---------------------------------------------------------
autoplot(tcr) +
  scale_y_continuous(labels = label_number(big.mark = ".")) +
  ylab("Car Registrations") +
  xlab("Year") +
  ggtitle("Total Car Registrations per Month from 1990 to 2021") #TCR

#### Seasonal plot ---------------------------------------
ggseasonplot(tcr, col = rainbow(12)) +
  scale_y_continuous(labels = label_number(big.mark = ".")) #TCR

#### ACF  -----------------------------------------------------------------
ggAcf(tcr, lag.max = 100) #TCR
ggPacf(tcr, lag.max = 100)
#### Moving average 5 --------------------------------------------------------
tcr_df <- data.frame(Date = dates, Value = as.vector(tcr))
ma5 <- ma(tcr_df$Value, order = 5)
ggplot(tcr_df, aes(x = Date, y = Value)) +
  geom_line(color = "black", linetype = "solid") +
  geom_line(aes(y = ma5), color = "red", linetype = "solid") +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  labs(x = "Year", y = "Car Registrations", title = "Monthly Average BMW Car Registrations") +
  theme_minimal()

#### Modern Decomposition ----------------------------------------------------
tcr %>%  
  stl(t.window = 13, s.window = 12, robust = T) %>%
  autoplot() + xlab("Year") + scale_y_continuous(labels = label_number(big.mark = ".")) +
  ggtitle("Monthly average Total Car Registrations Concentration") #TCR
#NO TREND BUT SEASONALITY

#### Trend + Seasonality  ----------------------------------------------------
d <- diag(12) 
n.tcr <- length(tcr)
t.tcr <- 1:n.tcr
s.tcr <- apply(replicate(n.tcr/12,d),1,"c")
lr_tcr2 <- lm(tcr ~ t.tcr+s.tcr)
summary(lr_tcr2) #not significant trend but some significant seasonalaties
y2 <- fitted(lr_tcr2)
y.ts <- ts(as.vector(y2),start=1990,frequency = 12)
ggplot(tcr_df, aes(x = Date, y = Value)) +
  geom_line(color = "black", linetype = "solid") +
  geom_line(aes(y = y.ts), color = "red", linetype = "solid") +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  labs(x = "Year", y = "Car Registrations", title = "Monthly Average BMW Car Registrations") +
  theme_minimal()
#not significant trend and some significant seasonalities

#### Only Trend --------------------------------------------------------------
lr_tcr <- lm(tcr ~ t.tcr) #Linear trend regression model for tcr
summary(lr_tcr)#not significant trend
#Plot linear trend
tcrh <- fitted(lr_tcr)
ggplot(tcr_df, aes(x = Date, y = Value)) +
  geom_line(color = "black", linetype = "solid") +
  geom_line(aes(y = tcrh), color = "red", linetype = "solid") +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  labs(x = "Year", y = "Car Registrations", title = " Car Registrations") +
  theme_minimal()
#no trend
#### Only Seasonality ---------------------------------------------------------
lr_tcr3 <- lm(tcr ~ s.tcr)
summary(lr_tcr3) #some significant seasonalities
y3 <- fitted(lr_tcr3)
y.ts <- ts(as.vector(y3),start=1990,frequency = 12)
ggplot(tcr_df, aes(x = Date, y = Value)) +
  geom_line(color = "black", linetype = "solid") +
  geom_line(aes(y = y.ts), color = "red", linetype = "solid") +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  labs(x = "Year", y = "Car Registrations", title = "Monthly Average BMW Car Registrations") +
  theme_minimal()
#
sum(abs(residuals(lr_tcr3)))
sum(abs(residuals(lr_tcr2))) #lowest
sum(abs(residuals(lr_tcr)))
#WHICH ONE IS BETTER?? They are very similar but in the model with also the trend, this one is not significant
#so we don't include the trend
#### Diagnostic and Dickey-Fuller -----------------------------------------------------------
adf.test(tcr)
#reject the null hypothesis
# Insample ----------------------------------------------------------------
#I then go out of sample 
ix.is <- 1:300
ix.os <- 301:312
n.tcr <- length(tcr)
t.tcr <- 1:n.tcr
d <- diag(12) 
s.tcr <- apply(replicate(n.tcr/12,d),1,"c")
x.tcr <- cbind(s.tcr) # WE PUT ONLY S BECAUSE WE HAVE SEEN THAT THE TREND IS NOT SIGNIFICANT
# For cycles for models on all dataset ----------------------------------------------------------
i=0
j=0
k=0
l=0
m=0
n=0
SARIMAfor_tcr<- as.data.frame(matrix(0, nrow = 10, ncol = 11))
colnames(SARIMAfor_tcr) <- c("AR", "I", "MA","SAR", "SI", "SMA", "Bic", "Aic", "Mse", "Mae", "RMSE")
index <- 1
for (i in 0:2) {
  for (j in 0:0) {
    for (k in 0:2 ) {
      for (l in 0:2) {
        for (m in 1:1) {
          for (n in 0:2 ) {
            SARIMAfor_tcr[index,1] <- i
            SARIMAfor_tcr[index,2] <- j
            SARIMAfor_tcr[index,3] <- k
            SARIMAfor_tcr[index,4] <- l
            SARIMAfor_tcr[index,5] <- m
            SARIMAfor_tcr[index,6] <- n
            fit1 <- arima(tcr[ix.is], order=c(i,j,k),seasonal = list(order=c(l,m,n),period=12))
            SARIMAfor_tcr[index,7] <- BIC(fit1)
            SARIMAfor_tcr[index,8] <- AIC(fit1)
            SARIMAfor_tcr[index,9] <- mean(residuals(fit1)^2)
            SARIMAfor_tcr[index,10] <- mean(abs(residuals(fit1)))
            SARIMAfor_tcr[index,11] <- sqrt(mean(residuals(fit1)^2))
            index <- index+1}}}}}}
#when I compute the 0:2 cycle for every 4 index, it doesn't return me 81 rows, but only 80, the last is missing because
#Errore in optim(init[mask], armafn, method = optim.method, hessian = TRUE, : valore iniziale in 'vmmin' non finito
view(SARIMAfor_tcr)
#SARIMA(1,0,2)(0,1,1)
#SARIMA(1,0,2)(2,1,2)
#SARIMA(1,0,1)(1,1,1)
#SARIMA(0,0,0)(0,1,0)
i=0
j=0
k=0
ARMAfor_tcr<- as.data.frame(matrix(0, nrow = 9, ncol = 7))
colnames(ARMAfor_tcr) <- c("AR", "I", "MA", "Bic", "Aic", "Mse", "Mae")
index <- 1

for (i in 0:2) {
  for (j in 0:0) {
    for (k in 0:2 ) {
      ARMAfor_tcr[index,1] <- i
      ARMAfor_tcr[index,2] <- j
      ARMAfor_tcr[index,3] <- k
      fit1 <- Arima(tcr[ix.is], xreg=x.tcr[ix.is], order=c(i,j,k), include.mean = F)
      ARMAfor_tcr[index,4] <- BIC(fit1)
      ARMAfor_tcr[index,5] <- AIC(fit1)
      ARMAfor_tcr[index,6] <- mean(residuals(fit1)^2)
      ARMAfor_tcr[index,7] <- mean(abs(residuals(fit1)))
      index <- index+1}}}
#we want to see the different models
view(ARMAfor_tcr)
# I choose the following models
#ARMA(2,2)
#ARMA(0,0)
#AR(1)
#MA(1)
#SARIMA -----------------------------------------------------
model_tcr<- as.data.frame(matrix(0, nrow = 8, ncol = 8))
colnames(model_tcr) <- c("AR", "I", "MA","SAR", "SI", "SMA", "Mse", "TotMSe")

#model 1
i <- 0
mse <- 0
totmse <- 0
n <- 300
for (i in 0:48){
  fit.os1 <- Arima(tcr[(1:n+i)],  order=c(1,0,2), seasonal = list(order=c(0,1,1),period=12), include.mean = F)
  yh.os <- forecast(object = fit.os1, h=12)
  mse <- mse+mean((yh.os$mean-tcr[ix.os+i])^2)
  totmse <- totmse+(sum(yh.os$mean)-sum(tcr[ix.os+i]))^2
  print(c(i,mean((yh.os$mean-tcr[ix.os+i])^2),(sum(yh.os$mean)-sum(tcr[ix.os+i]))^2))}
model_tcr[1,1:6] <- c(1,0,2,0,1,1)
model_tcr[1,7] <- mse/49
model_tcr[1,8] <- totmse/49

#model 2
i <- 0
mse <- 0
totmse <- 0
n <- 300
for (i in 0:48){
  fit.os2 <- Arima(tcr[(1:n+i)],  order=c(1,0,2), seasonal = list(order=c(2,1,2),period=12), include.mean = F)
  yh.os2 <- forecast(object = fit.os2, h=12)
  mse <- mse+mean((yh.os2$mean-tcr[ix.os+i])^2)
  totmse <- totmse+(sum(yh.os2$mean)-sum(tcr[ix.os+i]))^2
  print(c(i,mean((yh.os2$mean-tcr[ix.os+i])^2),(sum(yh.os2$mean)-sum(tcr[ix.os+i]))^2))}
model_tcr[2,1:6] <- c(1,0,2,2,1,2)
model_tcr[2,7] <- mse/49
model_tcr[2,8] <- totmse/49

#model 3
i <- 0
mse <- 0
totmse <- 0
n <- 300
for (i in 0:48){
  fit.os3 <- Arima(tcr[(1:n+i)],  order=c(1,0,1), seasonal = list(order=c(1,1,1),period=12), include.mean = F)
  yh.os3 <- forecast(object = fit.os3, h=12)
  mse <- mse+mean((yh.os3$mean-tcr[ix.os+i])^2)
  totmse <- totmse+(sum(yh.os3$mean)-sum(tcr[ix.os+i]))^2
  print(c(i,mean((yh.os3$mean-tcr[ix.os+i])^2),(sum(yh.os3$mean)-sum(tcr[ix.os+i]))^2))}
model_tcr[3,1:6] <- c(1,0,1,1,1,1)
model_tcr[3,7] <- mse/49
model_tcr[3,8] <- totmse/49

#model 4
i <- 0
mse <- 0
totmse <- 0
n <- 300
for (i in 0:48){
  fit.os4 <- Arima(tcr[(1:n+i)],  order=c(0,0,0), seasonal = list(order=c(0,1,0),period=12), include.mean = F)
  yh.os4 <- forecast(object = fit.os4, h=12)
  mse <- mse+mean((yh.os4$mean-tcr[ix.os+i])^2)
  totmse <- totmse+(sum(yh.os4$mean)-sum(tcr[ix.os+i]))^2
  print(c(i,mean((yh.os4$mean-tcr[ix.os+i])^2),(sum(yh.os4$mean)-sum(tcr[ix.os+i]))^2))}
model_tcr[4,1:6] <- c(0,0,0,0,1,0)
model_tcr[4,7] <- mse/49
model_tcr[4,8] <- totmse/49

#we compare different models in the first example just to see how they behave in 301:312 forecast
plot(ix.os,tcr[ix.os],type="l")
lines(yh.os4$mean, col="orange")
lines(yh.os3$mean, col="green")
lines(yh.os2$mean, col="blue")
lines(yh.os$mean, col="red")
# ARMA with deterministic seasonalities -----------------------------------

# Forecast out-of-sample

#model1 ARMA(2,2)
i <- 0
mse <- 0
totmse <- 0
n <- 300
for (i in 0:48){
  fit.is <- Arima(tcr[(1:n+i)], xreg=x.tcr[(1:n+i),], order=c(2,0,2), include.mean = F)
  yh.os <- forecast(object = fit.is, h=12, xreg = x.tcr[ix.os+i,])
  mse <- mse+ mean((yh.os$mean-tcr[ix.os+i])^2)
  totmse <- totmse+(sum(yh.os$mean)-sum(tcr[ix.os+i]))^2
  print(c(i,mean((yh.os$mean-tcr[ix.os+i])^2),(sum(yh.os$mean)-sum(tcr[ix.os+i]))^2))}
model_tcr[5,1:6] <- c(2,0,2,0,0,0)
model_tcr[5,7] <- mse/49
model_tcr[5,8] <- totmse/49

#model2 ARMA(0,0)
i <- 0
mse <- 0
totmse <- 0
n <- 300
for (i in 0:48){
  fit.is2 <- Arima(tcr[(1:n+i)], xreg=x.tcr[(1:n+i),], order=c(0,0,0), include.mean = F)
  yh.os2 <- forecast(object = fit.is2, h=12, xreg = x.tcr[ix.os+i,])
  mse <- mse+ mean((yh.os2$mean-tcr[ix.os+i])^2)
  totmse <- totmse+(sum(yh.os2$mean)-sum(tcr[ix.os+i]))^2
  print(c(i,mean((yh.os2$mean-tcr[ix.os+i])^2),(sum(yh.os2$mean)-sum(tcr[ix.os+i]))^2))}
model_tcr[6,1:6] <- c(0,0,0,0,0,0)
model_tcr[6,7] <- mse/49
model_tcr[6,8] <- totmse/49

#model3 AR(1)
i <- 0
mse <- 0
totmse <- 0
n <- 300
for (i in 0:48){
  fit.is3 <- Arima(tcr[(1:n+i)], xreg=x.tcr[(1:n+i),], order=c(1,0,0), include.mean = F)
  yh.os3 <- forecast(object = fit.is3, h=12, xreg = x.tcr[ix.os+i,])
  mse <- mse+ mean((yh.os3$mean-tcr[ix.os+i])^2)
  totmse <- totmse+(sum(yh.os3$mean)-sum(tcr[ix.os+i]))^2
  print(c(i,mean((yh.os3$mean-tcr[ix.os+i])^2),(sum(yh.os3$mean)-sum(tcr[ix.os+i]))^2))}
model_tcr[7,1:6] <- c(1,0,0,0,0,0)
model_tcr[7,7] <- mse/49
model_tcr[7,8] <- totmse/49

#model4 MA(1)
i <- 0
mse <- 0
totmse <- 0
n <- 300
for (i in 0:48){
  fit.is4 <- Arima(tcr[(1:n+i)], xreg=x.tcr[(1:n+i),], order=c(0,0,1), include.mean = F)
  yh.os4 <- forecast(object = fit.is4, h=12, xreg = x.tcr[ix.os+i,])
  mse <- mse+ mean((yh.os4$mean-tcr[ix.os+i])^2)
  totmse <- totmse+(sum(yh.os4$mean)-sum(tcr[ix.os+i]))^2
  print(c(i,mean((yh.os4$mean-tcr[ix.os+i])^2),(sum(yh.os4$mean)-sum(tcr[ix.os+i]))^2))}
model_tcr[8,1:6] <- c(0,0,1,0,0,0)
model_tcr[8,7] <- mse/49
model_tcr[8,8] <- totmse/49

#WE compare the different models just to see
plot(ix.os,tcr[ix.os],type="l")
lines(yh.os4$mean, col="orange")
lines(yh.os3$mean, col="green")
lines(yh.os2$mean, col="blue")
lines(yh.os$mean, col="red")
#AND WE HAVE TO CHOOSE THE BEST ONE 
# BEST tcr MODEL ----------------------------------------------------------
view(model_tcr)
#the best model according to MSE is SARIMA(1,0,1,1,1,1)
#the best model according to TOTMSE is ARMA(2,2)  with deterministic seasonalities and trend

### MARKET SHARE ------------------------------------------------------------
ms <- ts(as.vector(DS_timeseries_Month$Market_share_Month), start = 1990, frequency = 12)
ms <- log(ms)
#### Time-Series -------------------------------------------------------------
autoplot(ms) +
  scale_y_continuous(labels = label_number(big.mark = ".")) +
  ylab("Car Registrations") +
  xlab("Year") +
  ggtitle("Total Car Registrations per Month from 1990 to 2021") 

#### Seasonal plot -----------------------------------------------------------
ggseasonplot(ms, col = rainbow(12)) +
  scale_y_continuous(labels = label_number(big.mark = ".")) 

#### ACF ---------------------------------------------------------------------
ggAcf(ms, lag.max = 100)
ggPacf(ms, lag.max = 100)

#### Moving average 5 ----------------------------------------------------
ms_df <- data.frame(Date = dates, Value = as.vector(ms))
ma5 <- ma(ms_df$Value, order = 5)
ggplot(ms_df, aes(x = Date, y = Value)) +
  geom_line(color = "black", linetype = "solid") +
  geom_line(aes(y = ma5), color = "red", linetype = "solid") +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  labs(x = "Year", y = "Car Registrations", title = "Monthly Average BMW Car Registrations") +
  theme_minimal()

#### Modern Decomposition ----------------------------------------------------
ms %>%  
  stl(t.window = 13, s.window = 12, robust = T) %>%
  autoplot() + xlab("Year") + scale_y_continuous(labels = label_number(big.mark = ".")) +
  ggtitle("Monthly average Total Car Registrations Concentration") 
#we see trend and seasonality

#### Trend + Seasonality  ----------------------------------------------------
d <- diag(12) 
n.ms <- length(ms)
t.ms <- 1:n.ms
s.ms <- apply(replicate(n.ms/12,d),1,"c")
lr_ms2 <- lm(ms ~ t.ms+s.ms)
summary(lr_ms2) #significant trend and also some seasonalities 
y2 <- fitted(lr_ms2)
y.ts <- ts(as.vector(y2),start=1990,frequency = 12)
ggplot(ms_df, aes(x = Date, y = Value)) +
  geom_line(color = "black", linetype = "solid") +
  geom_line(aes(y = y.ts), color = "red", linetype = "solid") +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  labs(x = "Year", y = "Car Registrations", title = "Monthly Average BMW Car Registrations") +
  theme_minimal()
#significant trend and some significant seasonalities

#### Only Trend --------------------------------------------------------------
lr_ms <- lm(ms ~ t.ms) 
summary(lr_ms)#significant trend

#Plot linear trend
msh <- fitted(lr_ms)
ggplot(ms_df, aes(x = Date, y = Value)) +
  geom_line(color = "black", linetype = "solid") +
  geom_line(aes(y = msh), color = "red", linetype = "solid") +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  labs(x = "Year", y = "Car Registrations", title = "Monthly Average BMW Car Registrations") +
  theme_minimal()


#### Only Seasonality ---------------------------------------------------------
lr_ms3 <- lm(ms ~ s.ms)
summary(lr_ms3) #some significant seasonalities
y3 <- fitted(lr_ms3)
y.ts <- ts(as.vector(y3),start=1990,frequency = 12)
ggplot(ms_df, aes(x = Date, y = Value)) +
  geom_line(color = "black", linetype = "solid") +
  geom_line(aes(y = y.ts), color = "red", linetype = "solid") +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  labs(x = "Year", y = "Car Registrations", title = "Monthly Average BMW Car Registrations") +
  theme_minimal()
sum(abs(residuals(lr_ms3)))
sum(abs(residuals(lr_ms2))) #lowest sum of residuals
sum(abs(residuals(lr_ms)))
#WHICH ONE IS BETTER?? It's better the one with both seasonalities and trend
#so we include both trend and seasonalities
#### Diagnostic and Dickey-Fuller -----------------------------------------------------------
adf.test(ms)
#reject the null hypothesis
# Insample ----------------------------------------------------------------
#I then go out of sample 
ix.is <- 1:300
ix.os <- 301:312
n.ms <- length(ms)
t.ms <- 1:n.ms
d <- diag(12) 
s.ms <- apply(replicate(n.ms/12,d),1,"c")
x.ms <- cbind(t.ms,s.ms)#here we insert even the trend because we have seen that it's significant

# For cycles for models on all dataset ---------------------------------------
i=0
j=0
k=0
l=0
m=0
n=0
SARIMAfor_ms<- as.data.frame(matrix(0, nrow = 10, ncol = 11))
colnames(SARIMAfor_ms) <- c("AR", "I", "MA","SAR", "SI", "SMA", "Bic", "Aic", "Mse", "Mae", "RMSE")
index <- 1
for (i in 0:2) {
  for (j in 0:0) {
    for (k in 0:2 ) {
      for (l in 0:2) {
        for (m in 1:1) {
          for (n in 0:2 ) {
            SARIMAfor_ms[index,1] <- i
            SARIMAfor_ms[index,2] <- j
            SARIMAfor_ms[index,3] <- k
            SARIMAfor_ms[index,4] <- l
            SARIMAfor_ms[index,5] <- m
            SARIMAfor_ms[index,6] <- n
            fit1 <- arima(ms[ix.is], order=c(i,j,k),seasonal = list(order=c(l,m,n),period=12))
            SARIMAfor_ms[index,7] <- BIC(fit1)
            SARIMAfor_ms[index,8] <- AIC(fit1)
            SARIMAfor_ms[index,9] <- mean(residuals(fit1)^2)
            SARIMAfor_ms[index,10] <- mean(abs(residuals(fit1)))
            SARIMAfor_ms[index,11] <- sqrt(mean(residuals(fit1)^2))
            index <- index+1}}}}}}
#when I compute the 0:2 cycle for every 4 index, it doesn't return me 81 rows, but only 80, the last is missing because
#Errore in optim(init[mask], armafn, method = optim.method, hessian = TRUE, : valore iniziale in 'vmmin' non finito
view(SARIMAfor_ms)
#SARIMA(1,0,2)(1,1,1)
#SARIMA(1,0,2)(2,1,2)
#SARIMA(1,0,1)(0,1,1)
#SARIMA(0,0,0)(0,1,0)
#
i=0
j=0
k=0
ARMAfor_ms<- as.data.frame(matrix(0, nrow = 9, ncol = 7))
colnames(ARMAfor_ms) <- c("AR", "I", "MA", "Bic", "Aic", "Mse", "Mae")
index <- 1

for (i in 0:2) {
  for (j in 0:0) {
    for (k in 0:2) {
      ARMAfor_ms[index,1] <- i
      ARMAfor_ms[index,2] <- j
      ARMAfor_ms[index,3] <- k
      fit1 <- Arima(ms[ix.is], xreg=x.ms[ix.is], order=c(i,j,k), include.mean = F)
      ARMAfor_ms[index,4] <- BIC(fit1)
      ARMAfor_ms[index,5] <- AIC(fit1)
      ARMAfor_ms[index,6] <- mean(residuals(fit1)^2)
      ARMAfor_ms[index,7] <- mean(abs(residuals(fit1)))
      index <- index+1}}}
#we want to see the different models
view(ARMAfor_ms)
# I choose the following models
#ARMA(1,2)
#ARMA(0,0)
#AR(1)
#ARMA(1,1)
#SARIMA -----------------------------------------------------
model_ms<- as.data.frame(matrix(0, nrow = 8, ncol = 8))
colnames(model_ms) <- c("AR", "I", "MA","SAR", "SI", "SMA", "Mse", "TotMSe")

#model 1
i <- 0
mse <- 0
totmse <- 0
n <- 300
for (i in 0:48){
  fit.os1 <- Arima(ms[(1:n+i)],  order=c(1,0,1), seasonal = list(order=c(0,1,1),period=12), include.mean = F)
  yh.os <- forecast(object = fit.os1, h=12)
  mse <- mse+mean((yh.os$mean-ms[ix.os+i])^2)
  totmse <- totmse+(sum(yh.os$mean)-sum(ms[ix.os+i]))^2
  print(c(i,mean((yh.os$mean-ms[ix.os+i])^2),(sum(yh.os$mean)-sum(ms[ix.os+i]))^2))}
model_ms[1,1:6] <- c(1,0,1,0,1,1)
model_ms[1,7] <- mse/49
model_ms[1,8] <- totmse/49

#model 2
i <- 0
mse <- 0
totmse <- 0
n <- 300
for (i in 0:48){
  fit.os2 <- Arima(ms[(1:n+i)],  order=c(1,0,2), seasonal = list(order=c(2,1,2),period=12), include.mean = F)
  yh.os2 <- forecast(object = fit.os2, h=12)
  mse <- mse+mean((yh.os2$mean-ms[ix.os+i])^2)
  totmse <- totmse+(sum(yh.os2$mean)-sum(ms[ix.os+i]))^2
  print(c(i,mean((yh.os2$mean-ms[ix.os+i])^2),(sum(yh.os2$mean)-sum(ms[ix.os+i]))^2))}
model_ms[2,1:6] <- c(1,0,2,2,1,2)
model_ms[2,7] <- mse/49
model_ms[2,8] <- totmse/49

#model 3
i <- 0
mse <- 0
totmse <- 0
n <- 300
for (i in 0:48){
  fit.os3 <- Arima(ms[(1:n+i)],  order=c(1,0,2), seasonal = list(order=c(1,1,1),period=12), include.mean = F)
  yh.os3 <- forecast(object = fit.os3, h=12)
  mse <- mse+mean((yh.os3$mean-ms[ix.os+i])^2)
  totmse <- totmse+(sum(yh.os3$mean)-sum(ms[ix.os+i]))^2
  print(c(i,mean((yh.os3$mean-ms[ix.os+i])^2),(sum(yh.os3$mean)-sum(ms[ix.os+i]))^2))}
model_ms[3,1:6] <- c(1,0,2,1,1,1)
model_ms[3,7] <- mse/49
model_ms[3,8] <- totmse/49

#model 4
i <- 0
mse <- 0
totmse <- 0
n <- 300
for (i in 0:48){
  fit.os4 <- Arima(ms[(1:n+i)],  order=c(0,0,0), seasonal = list(order=c(0,1,0),period=12), include.mean = F)
  yh.os4 <- forecast(object = fit.os4, h=12)
  mse <- mse+mean((yh.os4$mean-ms[ix.os+i])^2)
  totmse <- totmse+(sum(yh.os4$mean)-sum(ms[ix.os+i]))^2
  print(c(i,mean((yh.os4$mean-ms[ix.os+i])^2),(sum(yh.os4$mean)-sum(ms[ix.os+i]))^2))}
model_ms[4,1:6] <- c(0,0,0,0,1,0)
model_ms[4,7] <- mse/49
model_ms[4,8] <- totmse/49

#we compare different models in the first example just to see how they behave in 301:312 forecast
plot(ix.os,ms[ix.os],type="l")
lines(yh.os4$mean, col="orange")
lines(yh.os3$mean, col="green")
lines(yh.os2$mean, col="blue")
lines(yh.os$mean, col="red")
# ARMA with deterministic seasonalities -----------------------------------

# Forecast out-of-sample

#model1 ARMA(1,1)
i <- 0
mse <- 0
totmse <- 0
n <- 300
for (i in 0:48){
  fit.is <- Arima(ms[(1:n+i)], xreg=x.ms[(1:n+i),], order=c(1,0,1), include.mean = F)
  yh.os <- forecast(object = fit.is, h=12, xreg = x.ms[ix.os+i,])
  mse <- mse+ mean((yh.os$mean-ms[ix.os+i])^2)
  totmse <- totmse+(sum(yh.os$mean)-sum(ms[ix.os+i]))^2
  print(c(i,mean((yh.os$mean-ms[ix.os+i])^2),(sum(yh.os$mean)-sum(ms[ix.os+i]))^2))}
model_ms[5,1:6] <- c(1,0,1,0,0,0)
model_ms[5,7] <- mse/49
model_ms[5,8] <- totmse/49

#model2 ARMA(0,0)
i <- 0
mse <- 0
totmse <- 0
n <- 300
for (i in 0:48){
  fit.is2 <- Arima(ms[(1:n+i)], xreg=x.ms[(1:n+i),], order=c(0,0,0), include.mean = F)
  yh.os2 <- forecast(object = fit.is2, h=12, xreg = x.ms[ix.os+i,])
  mse <- mse+ mean((yh.os2$mean-ms[ix.os+i])^2)
  totmse <- totmse+(sum(yh.os2$mean)-sum(ms[ix.os+i]))^2
  print(c(i,mean((yh.os2$mean-ms[ix.os+i])^2),(sum(yh.os2$mean)-sum(ms[ix.os+i]))^2))}
model_ms[6,1:6] <- c(0,0,0,0,0,0)
model_ms[6,7] <- mse/49
model_ms[6,8] <- totmse/49

#model3 AR(1)
i <- 0
mse <- 0
totmse <- 0
n <- 300
for (i in 0:48){
  fit.is3 <- Arima(ms[(1:n+i)], xreg=x.ms[(1:n+i),], order=c(1,0,0), include.mean = F)
  yh.os3 <- forecast(object = fit.is3, h=12, xreg = x.ms[ix.os+i,])
  mse <- mse+ mean((yh.os3$mean-ms[ix.os+i])^2)
  totmse <- totmse+(sum(yh.os3$mean)-sum(ms[ix.os+i]))^2
  print(c(i,mean((yh.os3$mean-ms[ix.os+i])^2),(sum(yh.os3$mean)-sum(ms[ix.os+i]))^2))}
model_ms[7,1:6] <- c(1,0,0,0,0,0)
model_ms[7,7] <- mse/49
model_ms[7,8] <- totmse/49

#model4 ARMMA(1,2)
i <- 0
mse <- 0
totmse <- 0
n <- 300
for (i in 0:48){
  fit.is4 <- Arima(ms[(1:n+i)], xreg=x.ms[(1:n+i),], order=c(1,0,2), include.mean = F)
  yh.os4 <- forecast(object = fit.is4, h=12, xreg = x.ms[ix.os+i,])
  mse <- mse+ mean((yh.os4$mean-ms[ix.os+i])^2)
  totmse <- totmse+(sum(yh.os4$mean)-sum(ms[ix.os+i]))^2
  print(c(i,mean((yh.os4$mean-ms[ix.os+i])^2),(sum(yh.os4$mean)-sum(ms[ix.os+i]))^2))}
model_ms[8,1:6] <- c(1,0,2,0,0,0)
model_ms[8,7] <- mse/49
model_ms[8,8] <- totmse/49

#WE compare the different models just to see
plot(ix.os,ms[ix.os],type="l")
lines(yh.os4$mean, col="orange")
lines(yh.os3$mean, col="green")
lines(yh.os2$mean, col="blue")
lines(yh.os$mean, col="red")
#AND WE HAVE TO CHOOSE THE BEST ONE 
# BEST ms MODEL ----------------------------------------------------------
view(model_ms)
#the best model according to MSE is SARIMA(1,0,1,0,1,1)
#the best model according to TOTMSE is SARIMA(1,0,1,0,1,1)

fit.os2 <- Arima(ms[ix.is],  order=c(1,0,1), seasonal = list(order=c(0,1,1),period=12), include.mean = F)
yh.os2 <- forecast(object = fit.os2, h=12)
plot(ix.os,exp(ms[ix.os]),type="l")
lines(exp(yh.os2$mean), col=2)
mse <- mean((exp(yh.os2$mean)-exp(ms[ix.os])^2))
totmse <- (sum(exp(yh.os2$mean))-sum(exp(ms[ix.os])))
#these are natural numbers  in the first case of 301:312 estimation
#the overall error over the next 12 months is -0.008672837

#the best model according to TOTMSE is SARIMA(0,0,0,0,1,0)
fit.os2 <- Arima(ms[ix.is],  order=c(0,0,0), seasonal = list(order=c(0,1,0),period=12), include.mean = F)
yh.os2 <- forecast(object = fit.os2, h=12)
plot(ix.os,exp(ms[ix.os]),type="l")
lines(exp(yh.os2$mean), col=2)
mse <- mean((exp(yh.os2$mean)-exp(ms[ix.os])^2))
totmse <- (sum(exp(yh.os2$mean))-sum(exp(ms[ix.os])))
#these are natural numbers  in the first case of 301:312 estimation
#the overall error over the next 12 months is -0.009670593


# FORECAST ESTIMATION -----------------------------------------------------
oos2020 <- 361:372
#WE WANT NOW TRY TO SEE THE DIFFERENCES IN FORECAST BETWEEN THE 2 DIFFERENT APPROACH
#USING THE BEST MODELS ACCORDING TO TOTMSE
#BMW
fit.os2 <- Arima(bmw,  order=c(1,0,2), seasonal = list(order=c(2,1,2),period=12), include.mean = F)
yh.os2 <- forecast(object = fit.os2, h=12)
exp(yh.os2$mean)
sum(exp(yh.os2$mean))
plot(exp(yh.os2$mean),type="l")

#TCR * MS
fit.os3 <- Arima(tcr,  order=c(1,0,1), seasonal = list(order=c(1,1,1),period=12), include.mean = F)
yh.os3 <- forecast(object = fit.os3, h=12)
fit.os4 <- Arima(ms,  order=c(1,0,1), seasonal = list(order=c(0,1,1),period=12), include.mean = F)
yh.os4 <- forecast(object = fit.os4, h=12)
(exp(yh.os3$mean) *exp(yh.os4$mean))
sum(exp(yh.os3$mean) *exp(yh.os4$mean))
plot(exp(yh.os2$mean),type="l")
lines((exp(yh.os3$mean) *exp(yh.os4$mean)), col=2)
#we plot the differences between the 2 
y1 <- exp(yh.os2$mean)
y2 <- exp(yh.os3$mean) * exp(yh.os4$mean)
plot(y1, type = "l", xaxt="n",  ylab = "Bmw Immatricolations", xlab = "2020 Months", main = "2020 Forecast for the 2 approaches")
lines(y2, col = 3)


# CLASSIFICATION ----------------------------------------------------------
library(readxl)
library(leaps)
library(glmnet)
library(forecast)
library(ggfortify)
library(tseries)
library(timeSeries)
library(ggplot2)
library(scales)
library(lubridate)
library(writexl)
library(tibble)
library(pROC)
library(ROCR)
DS_classification <- read_excel("DS_classification.xlsx", sheet = 2)
leader <- ts(as.vector(DS_classification$Leader_binary), start = 1990, frequency = 12)

##Exploratory Analysis ---------------------------------------------------
plot(DS_classification$Date, DS_classification$Leader_binary, 
     xlab = "Date", 
     ylab = "Leader", 
     main = "Car Market Leader in Europe from 1990 to 2021")
#1 represent BMW and 0 the others 
barplot(table(DS_classification$Leader_categorical), 
        main = "Car Market Leader in Europe from 1990 to 2021", 
        xlab = "Leader", 
        ylab = "Count",
        names.arg = c("BMW", "Mercedes", "Audi"))

barplot(table(DS_classification$Leader_binary), 
        main = "Car Market Leader in Europe from 1990 to 2021", 
        xlab = "Leader", 
        ylab = "Count",
        names.arg = c("Others", "Bmw"))

# Time-Series -------------------------------------------------------------
ts <- ts(as.vector(DS_classification$Leader_binary), start = 1990, frequency = 12)

autoplot(ts) +  
  labs(title = "Car Market Leader in Europe from 1990 to 2021",
       x = "Year", y = "Leader")
#1 represent BMW and 0 the others 

### Seasonal Plot -----------------------------------------------------------
ggseasonplot(leader, col = rainbow(12)) +
  scale_y_continuous(labels = label_number(big.mark = "."))

### ACF ---------------------------------------------------------------------
acf(leader, lag.max = 300)
Pacf(leader, lag.max = 300)
### Modern Decomposition ----------------------------------------------------
leader %>%  
  stl(t.window = 13, s.window = 12, robust = T) %>%
  autoplot() + xlab("Year") + scale_y_continuous(labels = label_number(big.mark = ".")) +
  ggtitle("Monthly average leader Car Registrations Concentration")
#we can see we have a clear seasonality, but not trend 

### Trend + Seasonality ------------------------------------
n.leader <- length(leader)
t.leader <- 1:n.leader
d <- diag(12) 
s.leader <- apply(replicate(n.leader/12,d),1,"c")
lr_leader2 <- lm(leader ~ t.leader+s.leader)
summary(lr_leader2)
#the trend is not significant but some seasonalities

y2 <- fitted(lr_leader2)
y.ts <- ts(as.vector(y2),start=1990,frequency = 12)
start_year <- 1990
dates <- seq(as.Date(paste(start_year, "01", "01", sep = "-")), by = "month", length.out = length(leader))
leader_df <- data.frame(Date = dates, Value = as.vector(leader))
ggplot(leader_df, aes(x = Date, y = Value)) +
  geom_line(color = "black", linetype = "solid") +
  geom_line(aes(y = y.ts), color = "red", linetype = "solid") +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  labs(x = "Year", y = "Car Registrations", title = "Monthly Average leader Car Registrations") +
  theme_minimal()
#some significant seasonalities

### Only Trend -------------------------------------------------------------
lr_leader <- lm(leader ~ t.leader) 
summary(lr_leader)# not significant trend
#Plot linear trend
leaderh <- fitted(lr_leader)
ggplot(leader_df, aes(x = Date, y = Value)) +
  geom_line(color = "black", linetype = "solid") +
  geom_line(aes(y = leaderh), color = "red", linetype = "solid") +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  labs(x = "Year", y = "Car Registrations", title = "Monthly Average leader Car Registrations") +
  theme_minimal()

### Only Seasonality ---------------------------------------------------------
lr_leader3 <- lm(leader ~ s.leader)
summary(lr_leader3)# some significant seasonalities
y3 <- fitted(lr_leader3)
y.ts <- ts(as.vector(y3),start=1990,frequency = 12)
ggplot(leader_df, aes(x = Date, y = Value)) +
  geom_line(color = "black", linetype = "solid") +
  geom_line(aes(y = y.ts), color = "red", linetype = "solid") +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  labs(x = "Year", y = "Car Registrations", title = "Monthly Average leader Car Registrations") +
  theme_minimal()
#we can see clearly that we have seasonality, but the trend part is missing...
sum(abs(residuals(lr_leader3)))
sum(abs(residuals(lr_leader2)))#lowest sum of residuals
sum(abs(residuals(lr_leader)))
#I check also with the plot
#WHICH ONE IS BETTER?? I think that we can go with only seasonalities
### Diagnostic and Dickey-Fuller -----------------------------------------------------------
adf.test(leader)
#reject
# Insample ----------------------------------------------------------------
ix.is <- 1:300
ix.os <- 301:312
n.leader <- length(leader)
t.leader <- 1:n.leader
d <- diag(12) 
s.leader <- apply(replicate(n.leader/12,d),1,"c")
x.leader <- cbind(s.leader)
# PROBIT AND LOGIT --------------------------------------------------------
n.leader <- length(leader)
t.leader <- 1:n.leader
d <- diag(12) 
s.leader <- apply(replicate(n.leader/12,d),1,"c")
s.leader <- as.data.frame(s.leader)

#Probit model 1
b <- 0
i <- 0
acctot <- 0
prectot <- 0
tre <- rep(0,48)
i <- 320
prec.1 <- 0
acc.1 <- 0
for (i in 300:348){
  leaderX <- leader[1:(i-1)]
  leaderOOSX <- leader[(i):(i+11)]
  leaderY <- leader[2:(i)]
  leaderOOSY <- leader[(i+1):(i+12)]
  mod <- glm(leaderY~leaderX, family = binomial(link = "probit"))
  p1.is <- predict(mod, newdata = data.frame(leaderX = leaderOOSX), type="response")
  if(sum(leaderOOSY) > 0) {
    roc_curve <- roc(leaderOOSY, p1.is)
    tresh <- roc_curve$thresholds[2]
    tre[i-299] <- tresh
  } else {
    tresh <- 1
  }
  dummyp1 <-1*(p1.is>tresh)
  cm<- table(leaderOOSY, dummyp1)
  if(all(dim(cm)==c(2,2))){
    acc.1 <- (cm[1,1] + cm[2,2]) /12 #accuracy
    prec.1 <- cm[2,2] /sum(dummyp1)
    b=b+1} #precision
  if(all(dim(cm)==c(1,2)) & sum(leaderOOSY)==0){
    acc.1 <- (cm[1,1]) / 12}
  if(all(dim(cm)==c(1,2)) & sum(leaderOOSY)==12){
    acc.1 <- (cm[1,2]) / 12}
  if(all(dim(cm)==c(2,1)) & sum(dummyp1)==0){
    acc.1 <- (cm[1,1]) / 12}
  if(all(dim(cm)==c(2,1)) & sum(dummyp1)==12){
    acc.1 <- (cm[2,1]) / 12 
    prec.1 <- (cm[2,1]) /sum(dummyp1)
    b=b+1}
  if(all(dim(cm)==c(1,1)) & sum(dummyp1)==0 & sum(leaderOOSY)==0){
    acc.1 <- 1}
  if(all(dim(cm)==c(1,1)) & sum(dummyp1)==0 & sum(leaderOOSY)==12){
    acc.1 <- 0}
  if(all(dim(cm)==c(1,1)) & sum(dummyp1)==12 & sum(leaderOOSY)==12){
    acc.1 <- 1
    prec.1 <- 1
    b=b+1}
  if(all(dim(cm)==c(1,1)) & sum(dummyp1)==12 & sum(leaderOOSY)==0){
    acc.1 <- 0}
  acctot <- acctot + acc.1
  prectot <- prectot + prec.1
  print(c(i,acc.1,prec.1,tresh))
  prec.1 <- 0
  acc.1 <- 0
}
acctot/49
model_probit<- as.data.frame(matrix(0, nrow = 5, ncol = 3))
colnames(model_probit) <- c("Model","Accuracy","Precision")
model_probit[1,1]<- "Yt-1"
model_probit[1,2] <- acctot/49
model_probit[1,3] <- prectot/b

#Probit model 2
b <- 0
i <- 0
acctot <- 0
prectot <- 0
tre <- rep(0,48)
i <- 300
prec.1 <- 0
acc.1 <- 0
for (i in 300:348){
  leaderX <- leader[1:(i-12)]
  leaderOOSX <- leader[(i-11):(i)]
  leaderY <- leader[13:(i)]
  leaderOOSY <- leader[(i+1):(i+12)]
  mod <- glm(leaderY~leaderX, family = binomial(link = "probit"))
  p1.is <- predict(mod, newdata = data.frame(leaderX = leaderOOSX), type="response")
  if(sum(leaderOOSY) > 0) {
    roc_curve <- roc(leaderOOSY, p1.is)
    tresh <- roc_curve$thresholds[2]
    tre[i-299] <- tresh
  } else {
    tresh <- 1
  }
  dummyp1 <-1*(p1.is>tresh)
  cm<- table(leaderOOSY, dummyp1)
  if(all(dim(cm)==c(2,2))){
    acc.1 <- (cm[1,1] + cm[2,2]) /12 #accuracy
    prec.1 <- cm[2,2] /sum(dummyp1)
    b=b+1} #precision
  if(all(dim(cm)==c(1,2)) & sum(leaderOOSY)==0){
    acc.1 <- (cm[1,1]) / 12}
  if(all(dim(cm)==c(1,2)) & sum(leaderOOSY)==12){
    acc.1 <- (cm[1,2]) / 12}
  if(all(dim(cm)==c(2,1)) & sum(dummyp1)==0){
    acc.1 <- (cm[1,1]) / 12}
  if(all(dim(cm)==c(2,1)) & sum(dummyp1)==12){
    acc.1 <- (cm[2,1]) / 12 
    prec.1 <- (cm[2,1]) /sum(dummyp1)
    b=b+1}
  if(all(dim(cm)==c(1,1)) & sum(dummyp1)==0 & sum(leaderOOSY)==0){
    acc.1 <- 1}
  if(all(dim(cm)==c(1,1)) & sum(dummyp1)==0 & sum(leaderOOSY)==12){
    acc.1 <- 0}
  if(all(dim(cm)==c(1,1)) & sum(dummyp1)==12 & sum(leaderOOSY)==12){
    acc.1 <- 1
    prec.1 <- 1
    b=b+1}
  if(all(dim(cm)==c(1,1)) & sum(dummyp1)==12 & sum(leaderOOSY)==0){
    acc.1 <- 0}
  acctot <- acctot + acc.1
  prectot <- prectot + prec.1
  print(c(i,acc.1,prec.1,tresh))
  prec.1 <- 0
  acc.1 <- 0
}
acctot/49
model_probit[2,1]<- "Yt-12"
model_probit[2,2] <- acctot/49
model_probit[2,3] <- prectot/b

#Probit model 3
b <- 0
i <- 0
acctot <- 0
prectot <- 0
tre <- rep(0,48)
prec.1 <- 0
acc.1 <- 0
for (i in 300:348){
  leaderX <- s.leader[1:(i-12),]
  leaderOOSX <- s.leader[(i-11):i,]
  leaderY <- leader[13:(i)]
  leaderOOSY <- leader[(i+1):(i+12)]
  leaderX <- as.data.frame(leaderX)
  leaderOOSX <- as.data.frame(leaderOOSX)
  mod <- glm(leaderY~. , data=leaderX, family = binomial(link = "probit"))
  p1.is <- predict(mod, newdata = leaderOOSX, type="response")
  if(sum(leaderOOSY) > 0) {
    roc_curve <- roc(leaderOOSY, p1.is)
    youden_index <- which.max(roc_curve$sensitivities + roc_curve$specificities - 1)
    tresh <- roc_curve$thresholds[youden_index]
    tre[i-299] <- tresh
  } else {
    tresh <- 1
  }
  dummyp1 <-1*(p1.is>tresh)
  cm<- table(leaderOOSY, dummyp1)
  if(all(dim(cm)==c(2,2))){
    acc.1 <- (cm[1,1] + cm[2,2]) /12 #accuracy
    prec.1 <- cm[2,2] /sum(dummyp1)
    b=b+1} #precision
  if(all(dim(cm)==c(1,2)) & sum(leaderOOSY)==0){
    acc.1 <- (cm[1,1]) / 12}
  if(all(dim(cm)==c(1,2)) & sum(leaderOOSY)==12){
    acc.1 <- (cm[1,2]) / 12}
  if(all(dim(cm)==c(2,1)) & sum(dummyp1)==0){
    acc.1 <- (cm[1,1]) / 12}
  if(all(dim(cm)==c(2,1)) & sum(dummyp1)==12){
    acc.1 <- (cm[2,1]) / 12 
    prec.1 <- (cm[2,1]) /sum(dummyp1)
    b=b+1}
  if(all(dim(cm)==c(1,1)) & sum(dummyp1)==0 & sum(leaderOOSY)==0){
    acc.1 <- 1}
  if(all(dim(cm)==c(1,1)) & sum(dummyp1)==0 & sum(leaderOOSY)==12){
    acc.1 <- 0}
  if(all(dim(cm)==c(1,1)) & sum(dummyp1)==12 & sum(leaderOOSY)==12){
    acc.1 <- 1
    prec.1 <- 1
    b=b+1}
  if(all(dim(cm)==c(1,1)) & sum(dummyp1)==12 & sum(leaderOOSY)==0){
    acc.1 <- 0}
  acctot <- acctot + acc.1
  prectot <- prectot + prec.1
  print(c(i,acc.1,prec.1,tresh))
  prec.1 <- 0
  acc.1 <- 0
}
acctot/49
model_probit[3,1]<- "Seas"
model_probit[3,2] <- acctot/49
model_probit[3,3] <- prectot/b

#Probit model 4
b <- 0
i <- 0
acctot <- 0
prectot <- 0
tre <- rep(0,48)
prec.1 <- 0
acc.1 <- 0
for (i in 300:348){
  leaderX <- cbind(s.leader[1:(i-12),],leader[1:(i-12)])
  leaderOOSX <- cbind(s.leader[(i-11):i,],leader[(i-11):(i)])
  leaderY <- leader[13:(i)]
  leaderOOSY <- leader[(i+1):(i+12)]
  leaderX <- as.data.frame(leaderX)
  leaderOOSX <- as.data.frame(leaderOOSX)
  colnames(leaderX)[13] <- "V13"
  colnames(leaderOOSX)[13] <- "V13"
  mod <- glm(leaderY~. , data=leaderX, family = binomial(link = "probit"))
  p1.is <- predict(mod, newdata = leaderOOSX, type="response")
  if(sum(leaderOOSY) > 0) {
    roc_curve <- roc(leaderOOSY, p1.is)
    youden_index <- which.max(roc_curve$sensitivities + roc_curve$specificities - 1)
    tresh <- roc_curve$thresholds[youden_index]
    tre[i-299] <- tresh
  } else {
    tresh <- 1
  }
  dummyp1 <-1*(p1.is>tresh)
  cm<- table(leaderOOSY, dummyp1)
  if(all(dim(cm)==c(2,2))){
    acc.1 <- (cm[1,1] + cm[2,2]) /12 #accuracy
    prec.1 <- cm[2,2] /sum(dummyp1)
    b=b+1} #precision
  if(all(dim(cm)==c(1,2)) & sum(leaderOOSY)==0){
    acc.1 <- (cm[1,1]) / 12}
  if(all(dim(cm)==c(1,2)) & sum(leaderOOSY)==12){
    acc.1 <- (cm[1,2]) / 12}
  if(all(dim(cm)==c(2,1)) & sum(dummyp1)==0){
    acc.1 <- (cm[1,1]) / 12}
  if(all(dim(cm)==c(2,1)) & sum(dummyp1)==12){
    acc.1 <- (cm[2,1]) / 12 
    prec.1 <- (cm[2,1]) /sum(dummyp1)
    b=b+1}
  if(all(dim(cm)==c(1,1)) & sum(dummyp1)==0 & sum(leaderOOSY)==0){
    acc.1 <- 1}
  if(all(dim(cm)==c(1,1)) & sum(dummyp1)==0 & sum(leaderOOSY)==12){
    acc.1 <- 0}
  if(all(dim(cm)==c(1,1)) & sum(dummyp1)==12 & sum(leaderOOSY)==12){
    acc.1 <- 1
    prec.1 <- 1
    b=b+1}
  if(all(dim(cm)==c(1,1)) & sum(dummyp1)==12 & sum(leaderOOSY)==0){
    acc.1 <- 0}
  acctot <- acctot + acc.1
  prectot <- prectot + prec.1
  print(c(i,acc.1,prec.1,tresh))
  prec.1 <- 0
  acc.1 <- 0
}
acctot/49
model_probit[4,1]<- "SEAS + Yt-12"
model_probit[4,2] <- acctot/49
model_probit[4,3] <- prectot/b

#Probit model 5
b <- 0
i <- 0
acctot <- 0
prectot <- 0
tre <- rep(0,48)
prec.1 <- 0
acc.1 <- 0
for (i in 300:348){
  leaderX <- cbind(s.leader[1:(i-12),],leader[12:(i-1)])
  leaderOOSX <- cbind(s.leader[(i-11):i,],leader[(i):(i+11)])
  leaderY <- leader[13:(i)]
  leaderOOSY <- leader[(i+1):(i+12)]
  colnames(leaderX)[1:13] <- seq(1,13)
  colnames(leaderOOSX)[1:13] <- seq(1,13)
  leaderX <- as.data.frame(leaderX)
  leaderOOSX <- as.data.frame(leaderOOSX)
  mod <- glm(leaderY~., data=leaderX, family = binomial(link = "probit"))
  p1.is <- predict(mod, newdata = leaderOOSX, type="response")
  if(sum(leaderOOSY) > 0) {
    roc_curve <- roc(leaderOOSY, p1.is)
    youden_index <- which.max(roc_curve$sensitivities + roc_curve$specificities - 1)
    tresh <- roc_curve$thresholds[youden_index]
    tre[i-299] <- tresh
  } else {
    tresh <- 1
  }
  dummyp1 <-1*(p1.is>tresh)
  cm<- table(leaderOOSY, dummyp1)
  if(all(dim(cm)==c(2,2))){
    acc.1 <- (cm[1,1] + cm[2,2]) /12 #accuracy
    prec.1 <- cm[2,2] /sum(dummyp1)
    b=b+1} #precision
  if(all(dim(cm)==c(1,2)) & sum(leaderOOSY)==0){
    acc.1 <- (cm[1,1]) / 12}
  if(all(dim(cm)==c(1,2)) & sum(leaderOOSY)==12){
    acc.1 <- (cm[1,2]) / 12}
  if(all(dim(cm)==c(2,1)) & sum(dummyp1)==0){
    acc.1 <- (cm[1,1]) / 12}
  if(all(dim(cm)==c(2,1)) & sum(dummyp1)==12){
    acc.1 <- (cm[2,1]) / 12 
    prec.1 <- (cm[2,1]) /sum(dummyp1)
    b=b+1}
  if(all(dim(cm)==c(1,1)) & sum(dummyp1)==0 & sum(leaderOOSY)==0){
    acc.1 <- 1}
  if(all(dim(cm)==c(1,1)) & sum(dummyp1)==0 & sum(leaderOOSY)==12){
    acc.1 <- 0}
  if(all(dim(cm)==c(1,1)) & sum(dummyp1)==12 & sum(leaderOOSY)==12){
    acc.1 <- 1
    prec.1 <- 1
    b=b+1}
  if(all(dim(cm)==c(1,1)) & sum(dummyp1)==12 & sum(leaderOOSY)==0){
    acc.1 <- 0}
  acctot <- acctot + acc.1
  prectot <- prectot + prec.1
  print(c(i,acc.1,prec.1,tresh))
  prec.1 <- 0
  acc.1 <- 0
}
acctot/49
model_probit[5,1] <- "SEAS + Yt-1"
model_probit[5,2] <- acctot/49
model_probit[5,3] <- prectot/b
## BEST leader MODEL ----------------------------------------------------------
#AND WE HAVE TO CHOOSE THE BEST ONE 
view(model_probit)
##the best model according toboth accuracy and precision is the one with seasonalities

# Insample ----------------------------------------------------------------
ix.is <- 1:300
ix.os <- 301:312
n.leader <- length(leader)
t.leader <- 1:n.leader
d <- diag(12) 
s.leader <- apply(replicate(n.leader/12,d),1,"c")
x.leader <- cbind(s.leader)
# For cycles for model on all dataset----------------------------------------------------
i=0
j=0
k=0
l=0
m=0
n=0
SARIMAfor_leader<- as.data.frame(matrix(0, nrow = 10, ncol = 11))
colnames(SARIMAfor_leader) <- c("AR", "I", "MA","SAR", "SI", "SMA", "Bic", "Aic", "Mse", "Mae", "RMSE")
index <- 1
for (i in 0:2) {
  for (j in 0:0) {
    for (k in 0:2 ) {
      for (l in 0:2) {
        for (m in 1:1) {
          for (n in 0:2 ) {
            SARIMAfor_leader[index,1] <- i
            SARIMAfor_leader[index,2] <- j
            SARIMAfor_leader[index,3] <- k
            SARIMAfor_leader[index,4] <- l
            SARIMAfor_leader[index,5] <- m
            SARIMAfor_leader[index,6] <- n
            fit1 <- arima(leader[ix.is], order=c(i,j,k),seasonal = list(order=c(l,m,n),period=12))
            SARIMAfor_leader[index,7] <- BIC(fit1)
            SARIMAfor_leader[index,8] <- AIC(fit1)
            SARIMAfor_leader[index,9] <- mean(residuals(fit1)^2)
            SARIMAfor_leader[index,10] <- mean(abs(residuals(fit1)))
            SARIMAfor_leader[index,11] <- sqrt(mean(residuals(fit1)^2))
            index <- index+1}}}}}}
#when I compute the 0:2 cycle for every 4 index, it doesn't return me only 70, the last is missing because
#Errore in optim(init[mask], SARIMAfn, method = optim.method, hessian = TRUE, : valore iniziale in 'vmmin' non finito
view(SARIMAfor_leader)
#I choose the following models 
#model1 SARIMA(1,0,1)(1,1,1)
#model2 SARIMA(1,0,1)(0,1,1)
#model3 SARIMA(0,0,0)(0,1,0)
#model4 SARIMA(1,0,1)(0,1,2)
#
i=0
j=0
k=0
ARMAfor_leader<- as.data.frame(matrix(0, nrow = 9, ncol = 7))
colnames(ARMAfor_leader) <- c("AR", "I", "MA", "Bic", "Aic", "Mse", "Mae")
index <- 1
x.leader<- cbind(s.leader)
for (i in 0:2) {
  for (j in 0:0) {
    for (k in 0:2) {
      ARMAfor_leader[index,1] <- i
      ARMAfor_leader[index,2] <- j
      ARMAfor_leader[index,3] <- k
      fit1 <- Arima(leader[ix.is], xreg=x.leader[ix.is,], order=c(i,j,k), include.mean = F)
      ARMAfor_leader[index,4] <- BIC(fit1)
      ARMAfor_leader[index,5] <- AIC(fit1)
      ARMAfor_leader[index,6] <- mean(residuals(fit1)^2)
      ARMAfor_leader[index,7] <- mean(abs(residuals(fit1)))
      index <- index+1}}}
#we want to see the different models
view(ARMAfor_leader)
#I choose the following model 
#ARMA(1,1)
#ARMA(1,2)
#ARMA(2,2)
#ARMA(0,0)
#SARIMA -----------------------------------------------------
model_leader<- as.data.frame(matrix(0, nrow = 8, ncol = 8))
colnames(model_leader) <- c("AR", "I", "MA","SAR", "SI", "SMA", "Mse", "TotMSe")

#model 1
i <- 0
mse <- 0
totmse <- 0
n <- 300
for (i in 0:48){
  fit.os1 <- Arima(leader[(1:n+i)],  order=c(1,0,1), seasonal = list(order=c(1,1,1),period=12), include.mean = F)
  yh.os <- forecast(object = fit.os1, h=12)
  mse <- mse+mean((yh.os$mean-leader[ix.os+i])^2)
  totmse <- totmse+(sum(yh.os$mean)-sum(leader[ix.os+i]))^2
  print(c(i,mean((yh.os$mean-leader[ix.os+i])^2),(sum(yh.os$mean)-sum(leader[ix.os+i]))^2))}
model_leader[1,1:6] <- c(1,0,1,1,1,1)
model_leader[1,7] <- mse/49
model_leader[1,8] <- totmse/49

#model 2
i <- 0
mse <- 0
totmse <- 0
n <- 300
for (i in 0:48){
  fit.os2 <- Arima(leader[(1:n+i)],  order=c(1,0,1), seasonal = list(order=c(0,1,1),period=12), include.mean = F)
  yh.os2 <- forecast(object = fit.os2, h=12)
  mse <- mse+mean((yh.os2$mean-leader[ix.os+i])^2)
  totmse <- totmse+(sum(yh.os2$mean)-sum(leader[ix.os+i]))^2
  print(c(i,mean((yh.os2$mean-leader[ix.os+i])^2),(sum(yh.os2$mean)-sum(leader[ix.os+i]))^2))}
model_leader[2,1:6] <- c(1,0,1,0,1,1)
model_leader[2,7] <- mse/49
model_leader[2,8] <- totmse/49

#model 3
i <- 0
mse <- 0
totmse <- 0
n <- 300
for (i in 0:48){
  fit.os3 <- Arima(leader[(1:n+i)],  order=c(1,0,1), seasonal = list(order=c(0,1,2),period=12), include.mean = F)
  yh.os3 <- forecast(object = fit.os3, h=12)
  mse <- mse+mean((yh.os3$mean-leader[ix.os+i])^2)
  totmse <- totmse+(sum(yh.os3$mean)-sum(leader[ix.os+i]))^2
  print(c(i,mean((yh.os3$mean-leader[ix.os+i])^2),(sum(yh.os3$mean)-sum(leader[ix.os+i]))^2))}
model_leader[3,1:6] <- c(1,0,1,0,1,2)
model_leader[3,7] <- mse/49
model_leader[3,8] <- totmse/49

#model 4
i <- 0
mse <- 0
totmse <- 0
n <- 300
for (i in 0:48){
  fit.os4 <- Arima(leader[(1:n+i)],  order=c(0,0,0), seasonal = list(order=c(0,1,0),period=12), include.mean = F)
  yh.os4 <- forecast(object = fit.os4, h=12)
  mse <- mse+mean((yh.os4$mean-leader[ix.os+i])^2)
  totmse <- totmse+(sum(yh.os4$mean)-sum(leader[ix.os+i]))^2
  print(c(i,mean((yh.os4$mean-leader[ix.os+i])^2),(sum(yh.os4$mean)-sum(leader[ix.os+i]))^2))}
model_leader[4,1:6] <- c(0,0,0,0,1,0)
model_leader[4,7] <- mse/49
model_leader[4,8] <- totmse/49
#we compare different models in the first example just to see how they behave in 301:312 forecast
plot(ix.os,leader[ix.os],type="l")
lines(yh.os4$mean, col="orange")
lines(yh.os3$mean, col="green")
lines(yh.os2$mean, col="blue")
lines(yh.os$mean, col="red")
# ARMA with deterministic seasonalities -----------------------------------

# Forecast out-of-sample

#model1 ARMA(2,2)
i <- 0
mse <- 0
totmse <- 0
n <- 300
for (i in 0:48){
  fit.is <- Arima(leader[(1:n+i)], xreg=x.leader[(1:n+i),], order=c(2,0,2), include.mean = F)
  yh.os <- forecast(object = fit.is, h=12, xreg = x.leader[ix.os+i,])
  mse <- mse+ mean((yh.os$mean-leader[ix.os+i])^2)
  totmse <- totmse+(sum(yh.os$mean)-sum(leader[ix.os+i]))^2
  print(c(i,mean((yh.os$mean-leader[ix.os+i])^2),(sum(yh.os$mean)-sum(leader[ix.os+i]))^2))}
model_leader[5,1:6] <- c(2,0,2,0,0,0)
model_leader[5,7] <- mse/49
model_leader[5,8] <- totmse/49

#model2 ARMA(0,0)
i <- 0
mse <- 0
totmse <- 0
n <- 300
for (i in 0:48){
  fit.is2 <- Arima(leader[(1:n+i)], xreg=x.leader[(1:n+i),], order=c(0,0,0), include.mean = F)
  yh.os2 <- forecast(object = fit.is2, h=12, xreg = x.leader[ix.os+i,])
  mse <- mse+ mean((yh.os2$mean-leader[ix.os+i])^2)
  totmse <- totmse+(sum(yh.os2$mean)-sum(leader[ix.os+i]))^2
  print(c(i,mean((yh.os2$mean-leader[ix.os+i])^2),(sum(yh.os2$mean)-sum(leader[ix.os+i]))^2))}
model_leader[6,1:6] <- c(0,0,0,0,0,0)
model_leader[6,7] <- mse/49
model_leader[6,8] <- totmse/49

#model3 ARMA(1,1)
i <- 0
mse <- 0
totmse <- 0
n <- 300
for (i in 0:48){
  fit.is3 <- Arima(leader[(1:n+i)], xreg=x.leader[(1:n+i),], order=c(1,0,1), include.mean = F)
  yh.os3 <- forecast(object = fit.is3, h=12, xreg = x.leader[ix.os+i,])
  mse <- mse+ mean((yh.os3$mean-leader[ix.os+i])^2)
  totmse <- totmse+(sum(yh.os3$mean)-sum(leader[ix.os+i]))^2
  print(c(i,mean((yh.os3$mean-leader[ix.os+i])^2),(sum(yh.os3$mean)-sum(leader[ix.os+i]))^2))}
model_leader[7,1:6] <- c(1,0,1,0,0,0)
model_leader[7,7] <- mse/49
model_leader[7,8] <- totmse/49

#model4 ARMA(1,2)
i <- 0
mse <- 0
totmse <- 0
n <- 300
for (i in 0:48){
  fit.is4 <- Arima(leader[(1:n+i)], xreg=x.leader[(1:n+i),], order=c(1,0,2), include.mean = F)
  yh.os4 <- forecast(object = fit.is4, h=12, xreg = x.leader[ix.os+i,])
  mse <- mse+ mean((yh.os4$mean-leader[ix.os+i])^2)
  totmse <- totmse+(sum(yh.os4$mean)-sum(leader[ix.os+i]))^2
  print(c(i,mean((yh.os4$mean-leader[ix.os+i])^2),(sum(yh.os4$mean)-sum(leader[ix.os+i]))^2))}
model_leader[8,1:6] <- c(1,0,2,0,0,0)
model_leader[8,7] <- mse/49
model_leader[8,8] <- totmse/49

#WE compare the different models just to see
plot(ix.os,leader[ix.os],type="l")
lines(yh.os4$mean, col="orange")
lines(yh.os3$mean, col="green")
lines(yh.os2$mean, col="blue")
lines(yh.os$mean, col="red")
#
view(model_leader)
#according to MSE and TOTMSE: only deterministic seasonalities


