library(MLmetrics)
library(randomForest)
library(gbm)

QB2021 <- read.csv("~/Desktop/MGSC 434 Final Project/Regression Data/QB2021.csv")
RB2021 <- read.csv("~/Desktop/MGSC 434 Final Project/Regression Data/RB2021.csv")
WR2021 <- read.csv("~/Desktop/MGSC 434 Final Project/Regression Data/WR2021.csv")
TE2021 <- read.csv("~/Desktop/MGSC 434 Final Project/Regression Data/TE2021.csv")
DST2021 <- read.csv("~/Desktop/MGSC 434 Final Project/Regression Data/DST2021.csv")

QB2020 <- read.csv("~/Desktop/MGSC 434 Final Project/Regression Data/QB2020.csv")
RB2020 <- read.csv("~/Desktop/MGSC 434 Final Project/Regression Data/RB2020.csv")
WR2020 <- read.csv("~/Desktop/MGSC 434 Final Project/Regression Data/WR2020.csv")
TE2020 <- read.csv("~/Desktop/MGSC 434 Final Project/Regression Data/TE2020.csv")
DST2020 <- read.csv("~/Desktop/MGSC 434 Final Project/Regression Data/DST2020.csv")

QB2019 <- read.csv("~/Desktop/MGSC 434 Final Project/Regression Data/QB2019.csv")
RB2019 <- read.csv("~/Desktop/MGSC 434 Final Project/Regression Data/RB2019.csv")
WR2019 <- read.csv("~/Desktop/MGSC 434 Final Project/Regression Data/WR2019.csv")
TE2019 <- read.csv("~/Desktop/MGSC 434 Final Project/Regression Data/TE2019.csv")
DST2019 <- read.csv("~/Desktop/MGSC 434 Final Project/Regression Data/DST2019.csv")

QB <- rbind(QB2019, QB2020)
RB <- rbind(RB2019, RB2020)
WR <- rbind(WR2019, WR2020)
TE <- rbind(TE2019, TE2020)
DST <- rbind(DST2019, DST2020)


########## QB REGRESSION ##########
data = na.omit(QB) # put different position here 
data = data[data$Week != 17,]
set.seed(123)
sample = sort(sample(nrow(data),nrow(data)*0.7))
train = data[sample,]
test = data[-sample,]

### Fantasy Points: Simple linear regressions to determine which average is best
Reg_FPRolling <- lm(FantasyPoints ~ FantasyPointsMean_Rolling, data=train)
summary(Reg_FPRolling)

Reg_FPLast3 <- lm(FantasyPoints ~ FantasyPointsMean_Last3, data=train)
summary(Reg_FPLast3)

Reg_FPLast5 <- lm(FantasyPoints ~ FantasyPointsMean_Last5, data=train)
summary(Reg_FPLast5)
# ROLLING

### Rush Share: Simple linear regressions to determine which average is best
Reg_RSRolling <- lm(FantasyPoints ~ RushShareMean_Rolling, data=train)
summary(Reg_RSRolling)

Reg_RSLast3 <- lm(FantasyPoints ~ RushShareMean_Last3, data=train)
summary(Reg_RSLast3)

Reg_RSLast5 <- lm(FantasyPoints ~ RushShareMean_Last5, data=train)
summary(Reg_RSLast5)
# LAST 5

### Target Share: Simple linear regressions to determine which average is best
Reg_TSRolling <- lm(FantasyPoints ~ PassShareMean_Rolling, data=train)
summary(Reg_TSRolling)

Reg_TSLast3 <- lm(FantasyPoints ~ PassShareMean_Last3, data=train)
summary(Reg_TSLast3)

Reg_TSLast5 <- lm(FantasyPoints ~ PassShareMean_Last5, data=train)
summary(Reg_TSLast5)
# LAST 5

### OPR: Simple linear regression
Reg_OPR <- lm(FantasyPoints ~ OPR, data=train)
summary(Reg_OPR)
# No stars

### QBInjured: Simple linear regressions
Reg_QBInjured <- lm(FantasyPoints ~ QBInjured, data=train)
summary(Reg_QBInjured)
# No Stars

### RBInjured: Simple linear regressions
Reg_RBInjured <- lm(FantasyPoints ~ RBInjured, data=train)
summary(Reg_RBInjured)
# No Stars

### WRInjured: Simple linear regressions
Reg_WRInjured <- lm(FantasyPoints ~ WRInjured, data=train)
summary(Reg_WRInjured)
# No Stars

### TEInjured: Simple linear regressions
Reg_TEInjured <- lm(FantasyPoints ~ TEInjured, data=train)
summary(Reg_TEInjured)
# No Stars

### Injury Interaction
Reg <- lm(FantasyPoints ~ FantasyPointsMean_Rolling*QBInjured, data=train)
summary(Reg)
# yes

Reg <- lm(FantasyPoints ~ FantasyPointsMean_Last5*QBInjured, data=train)
summary(Reg)
# no

Reg <- lm(FantasyPoints ~ FantasyPointsMean_Last3*QBInjured, data=train)
summary(Reg)
# no

Reg <- lm(FantasyPoints ~ PassShareMean_Rolling*QBInjured, data=train)
summary(Reg)
# yes

Reg <- lm(FantasyPoints ~ PassShareMean_Last5*QBInjured, data=train)
summary(Reg)
# yes

Reg <- lm(FantasyPoints ~ PassShareMean_Rolling*QBInjured, data=train)
summary(Reg)
# yes

# Function to evaluate regressions 
evaluate <- function(Reg) {
  y_pred = predict(Reg, newdata = test)
  y_true = test$FantasyPoints
  mse <- MSE(y_pred, y_true)
  dev <- sqrt(mse)
  return(dev)}


### Multiple regression model

# Best combination of FantasyPointsMeam and TargetShareMean

Reg <- lm(FantasyPoints ~ FantasyPointsMean_Rolling + PassShareMean_Rolling, data=train)
summary(Reg)
evaluate(Reg)

Reg <- lm(FantasyPoints ~ FantasyPointsMean_Rolling + PassShareMean_Last3, data=train)
summary(Reg)
evaluate(Reg) 

Reg <- lm(FantasyPoints ~ FantasyPointsMean_Rolling + PassShareMean_Last5, data=train)
summary(Reg)
evaluate(Reg)

Reg <- lm(FantasyPoints ~ FantasyPointsMean_Last3 + PassShareMean_Rolling, data=train)
summary(Reg) # all sig
evaluate(Reg)

Reg <- lm(FantasyPoints ~ FantasyPointsMean_Last3 + PassShareMean_Last3, data=train)
summary(Reg)
evaluate(Reg) # all sig

Reg <- lm(FantasyPoints ~ FantasyPointsMean_Last3 + PassShareMean_Last5, data=train)
summary(Reg)
evaluate(Reg) # all three stars

Reg <- lm(FantasyPoints ~ FantasyPointsMean_Last5 + PassShareMean_Rolling, data=train)
summary(Reg)
evaluate(Reg) # all sig

Reg <- lm(FantasyPoints ~ FantasyPointsMean_Last5 + PassShareMean_Last3, data=train)
summary(Reg) # lowest MSE
evaluate(Reg) # all sig

Reg <- lm(FantasyPoints ~ FantasyPointsMean_Last5 + PassShareMean_Last5, data=train)
summary(Reg)
evaluate(Reg) # all sig

# Add OPR
Reg <- lm(FantasyPoints ~ FantasyPointsMean_Last5 + PassShareMean_Last3 + OPR, data=train)
summary(Reg) # not sig
evaluate(Reg) 

# Add Injuries
Reg <- lm(FantasyPoints ~ FantasyPointsMean_Last5 + PassShareMean_Last3 + QBInjured, data=train)
summary(Reg) # not sig
evaluate(Reg) 
# Try Interaction
Reg <- lm(FantasyPoints ~ FantasyPointsMean_Last5 + PassShareMean_Last3 + QBInjured + FantasyPointsMean_Last5*QBInjured + PassShareMean_Last3*QBInjured, data=train)
summary(Reg) # not sig
evaluate(Reg) 
# Try Interaction - not present anymore

Reg <- lm(FantasyPoints ~ FantasyPointsMean_Last5 + PassShareMean_Last3 + RBInjured, data=train)
summary(Reg) # not sig
evaluate(Reg) 

Reg <- lm(FantasyPoints ~ FantasyPointsMean_Last5 + PassShareMean_Last3 + WRInjured, data=train)
summary(Reg) # not sig
evaluate(Reg) 

Reg <- lm(FantasyPoints ~ FantasyPointsMean_Last5 + PassShareMean_Last3 + TEInjured, data=train)
summary(Reg) # not sig
evaluate(Reg) 

# Add RushShareMean
Reg <- lm(FantasyPoints ~ FantasyPointsMean_Last5 + PassShareMean_Last3 + RushShareMean_Rolling, data=train)
summary(Reg) # not sig
evaluate(Reg) 

Reg <- lm(FantasyPoints ~ FantasyPointsMean_Last5 + PassShareMean_Last3 + RushShareMean_Last3, data=train)
summary(Reg) # not sig
evaluate(Reg)

Reg <- lm(FantasyPoints ~ FantasyPointsMean_Last5 + PassShareMean_Last3 + RushShareMean_Last5, data=train)
summary(Reg) # sig
evaluate(Reg)
# dont include rush share because it will value rushing QBs too much

# Try adding OPR again
Reg <- lm(FantasyPoints ~ FantasyPointsMean_Last5 + PassShareMean_Last3 + OPR, data=train)
summary(Reg) 
evaluate(Reg)
# OPR is very close to being significant and helps reduce MSE, will opt to include it

# Final Model
Reg <- lm(FantasyPoints ~ FantasyPointsMean_Last5 + PassShareMean_Last3 + OPR, data=train)
summary(Reg) 
evaluate(Reg)

# Does this model perform better with Random Forest?
Forest <- randomForest(FantasyPoints ~ FantasyPointsMean_Last5 + PassShareMean_Last3 + OPR, data=train, importance=TRUE, ntree=1000, do.trace=50)
Forest
evaluate(Forest) # No

# Does this model perform better with Gradient Boosting?
Boost <- gbm(FantasyPoints ~ FantasyPointsMean_Last5 + PassShareMean_Last3 + OPR, data=train, n.trees=1000)
summary(Boost)
evaluate(Boost) # No

# Can we do better with different variables in Random Forest/Gradient Boosting?

# All variables 
Forest <- randomForest(FantasyPoints ~ FantasyPointsMean_Rolling + RushShareMean_Rolling + PassShareMean_Rolling + FantasyPointsMean_Last5 + RushShareMean_Last5 + PassShareMean_Last5 + FantasyPointsMean_Last3 + RushShareMean_Last3 + PassShareMean_Last3 + OPR + QBInjured + RBInjured + WRInjured + TEInjured, data=train, importance=TRUE, ntree=1000, do.trace=50)
Forest
importance(Forest)
varImpPlot(Forest)
evaluate(Forest) # Not better than regression

# Only FantasyPoints, PassShares, OPR
Forest <- randomForest(FantasyPoints ~ FantasyPointsMean_Rolling + PassShareMean_Rolling + FantasyPointsMean_Last5 + PassShareMean_Last5 + FantasyPointsMean_Last3 + PassShareMean_Last3 + OPR, data=train, importance=TRUE, ntree=1000, do.trace=50)
Forest
evaluate(Forest) # Not better than regression

Boost <- gbm(FantasyPoints ~ FantasyPointsMean_Rolling + PassShareMean_Rolling + FantasyPointsMean_Last5 + PassShareMean_Last5 + FantasyPointsMean_Last3 + PassShareMean_Last3 + OPR, data=train, n.trees=1000)
summary(Boost)
evaluate(Boost) # Not better than regression

Boost <- gbm(FantasyPoints ~ FantasyPointsMean_Rolling + PassShareMean_Rolling + FantasyPointsMean_Last5 + PassShareMean_Last5 + FantasyPointsMean_Last3 + PassShareMean_Last3 + OPR, data=train, n.trees=10000)
summary(Boost)
evaluate(Boost) # Not better than regression


##### FINAL MODEL #####
Reg <- lm(FantasyPoints ~ FantasyPointsMean_Last5 + PassShareMean_Last3 + OPR, data=train)
summary(Reg) 
evaluate(Reg)

library(stargazer)
stargazer(Reg, type='html', title='QB Regression Model')

data = QB2021
data = na.omit(data) # put different position here 
data = data[data$Week != 18,]

# MSE between real and our predictions for 2021
y_pred = predict(Reg, newdata = data)
y_true = data$FantasyPoints
mse <- MSE(y_pred, y_true)
dev <- sqrt(mse) 

# MSE between DraftKings projections and our predictions 
mse <- MSE(data$FantasyPoints_DraftKingsProjection, predict(Reg, newdata = data))
dev <- sqrt(mse) 

# Export Data
data = data[,c("PlayerID","Name","Position","Week","Team","Opponent","Salary","FantasyPoints","FantasyPoints_DraftKingsProjection")]
data$FantasyPoints_OurPrediction = y_pred
write.csv(data, "~/Desktop/MGSC 434 Final Project/Optimization Data/QB2021.csv")



