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


########## WR MODEL ##########
data = na.omit(WR) # put different position here 
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
# ROLLING

### Target Share: Simple linear regressions to determine which average is best
Reg_TSRolling <- lm(FantasyPoints ~ TargetShareMean_Rolling, data=train)
summary(Reg_TSRolling)

Reg_TSLast3 <- lm(FantasyPoints ~ TargetShareMean_Last3, data=train)
summary(Reg_TSLast3)

Reg_TSLast5 <- lm(FantasyPoints ~ TargetShareMean_Last5, data=train)
summary(Reg_TSLast5)
# ROLLING

### OPR: Simple linear regression
Reg_OPR <- lm(FantasyPoints ~ OPR, data=train)
summary(Reg_OPR)
# 1 star

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
Reg <- lm(FantasyPoints ~ FantasyPointsMean_Rolling*WRInjured, data=train)
summary(Reg)

Reg <- lm(FantasyPoints ~ FantasyPointsMean_Last5*WRInjured, data=train)
summary(Reg)

Reg <- lm(FantasyPoints ~ FantasyPointsMean_Last3*WRInjured, data=train)
summary(Reg)

Reg <- lm(FantasyPoints ~ TargetShareMean_Rolling*WRInjured, data=train)
summary(Reg)

Reg <- lm(FantasyPoints ~ TargetShareMean_Last5*WRInjured, data=train)
summary(Reg)

Reg <- lm(FantasyPoints ~ TargetShareMean_Rolling*WRInjured, data=train)
summary(Reg)
# No interaction found

# Function to evaluate regressions 
evaluate <- function(Reg) {
  y_pred = predict(Reg, newdata = test)
  y_true = test$FantasyPoints
  mse <- MSE(y_pred, y_true)
  dev <- sqrt(mse)
  return(dev)}


### Multiple regression model

# Best combination of FantasyPointsMeam and TargetShareMean

Reg <- lm(FantasyPoints ~ FantasyPointsMean_Rolling + TargetShareMean_Rolling, data=train)
summary(Reg)
evaluate(Reg)

Reg <- lm(FantasyPoints ~ FantasyPointsMean_Rolling + TargetShareMean_Last3, data=train)
summary(Reg)
evaluate(Reg) # Lowest MSE

Reg <- lm(FantasyPoints ~ FantasyPointsMean_Rolling + TargetShareMean_Last5, data=train)
summary(Reg)
evaluate(Reg)

Reg <- lm(FantasyPoints ~ FantasyPointsMean_Last3 + TargetShareMean_Rolling, data=train)
summary(Reg)
evaluate(Reg)

Reg <- lm(FantasyPoints ~ FantasyPointsMean_Last3 + TargetShareMean_Last3, data=train)
summary(Reg)
evaluate(Reg)

Reg <- lm(FantasyPoints ~ FantasyPointsMean_Last3 + TargetShareMean_Last5, data=train)
summary(Reg)
evaluate(Reg)

Reg <- lm(FantasyPoints ~ FantasyPointsMean_Last5 + TargetShareMean_Rolling, data=train)
summary(Reg)
evaluate(Reg)

Reg <- lm(FantasyPoints ~ FantasyPointsMean_Last5 + TargetShareMean_Last3, data=train)
summary(Reg)
evaluate(Reg)

Reg <- lm(FantasyPoints ~ FantasyPointsMean_Last5 + TargetShareMean_Last5, data=train)
summary(Reg)
evaluate(Reg)

# Add OPR
Reg <- lm(FantasyPoints ~ FantasyPointsMean_Rolling + TargetShareMean_Last3 + OPR, data=train)
summary(Reg)
evaluate(Reg) 

# Add Injuries
Reg <- lm(FantasyPoints ~ FantasyPointsMean_Rolling + TargetShareMean_Last3 + OPR + QBInjured, data=train)
summary(Reg) # Sig
evaluate(Reg) 

Reg <- lm(FantasyPoints ~ FantasyPointsMean_Rolling + TargetShareMean_Last3 + OPR + RBInjured, data=train)
summary(Reg) # Not Sig
evaluate(Reg)

Reg <- lm(FantasyPoints ~ FantasyPointsMean_Rolling + TargetShareMean_Last3 + OPR + WRInjured, data=train)
summary(Reg) # Sig
evaluate(Reg)

Reg <- lm(FantasyPoints ~ FantasyPointsMean_Rolling + TargetShareMean_Last3 + OPR + TEInjured, data=train)
summary(Reg) # Not sig
evaluate(Reg) 

# Add RushShareMean_Rolling
Reg <- lm(FantasyPoints ~ FantasyPointsMean_Rolling + TargetShareMean_Last3 + RushShareMean_Rolling + OPR + QBInjured + WRInjured, data=train)
summary(Reg) 
evaluate(Reg)

Reg <- lm(FantasyPoints ~ FantasyPointsMean_Rolling + TargetShareMean_Last3 + RushShareMean_Last3 + OPR + QBInjured + WRInjured, data=train)
summary(Reg) 
evaluate(Reg)

Reg <- lm(FantasyPoints ~ FantasyPointsMean_Rolling + TargetShareMean_Last3 + RushShareMean_Last5 + OPR + QBInjured + WRInjured, data=train)
summary(Reg) 
evaluate(Reg)

# Final Model
Reg <- lm(FantasyPoints ~ FantasyPointsMean_Rolling + TargetShareMean_Last3 + OPR + QBInjured + WRInjured, data=train)
summary(Reg) 
evaluate(Reg)


# Does this model perform better with Random Forest?
Forest <- randomForest(FantasyPoints ~ FantasyPointsMean_Rolling + TargetShareMean_Last3 + OPR + QBInjured + WRInjured, data=train, importance=TRUE, ntree=1000, do.trace=50)
Forest
evaluate(Forest) # No

# Does this model perform better with Gradient Boosting?
Boost <- gbm(FantasyPoints ~ FantasyPointsMean_Rolling + TargetShareMean_Last3 + OPR + QBInjured + WRInjured, data=train, n.trees=1000)
summary(Boost)
evaluate(Boost) # No

# Can we do better with different variables in Random Forest/Gradient Boosting?

# All variables 
Forest <- randomForest(FantasyPoints ~ FantasyPointsMean_Rolling + RushShareMean_Rolling + TargetShareMean_Rolling + FantasyPointsMean_Last5 + RushShareMean_Last5 + TargetShareMean_Last5 + FantasyPointsMean_Last3 + RushShareMean_Last3 + TargetShareMean_Last3 + OPR + QBInjured + RBInjured + WRInjured + TEInjured, data=train, importance=TRUE, ntree=1000, do.trace=50)
Forest
importance(Forest)
varImpPlot(Forest)
evaluate(Forest) # Not better than regression

# Only FantasyPoints, TargetShares, OPR, QBInjured, WRInjured
Forest <- randomForest(FantasyPoints ~ FantasyPointsMean_Rolling + TargetShareMean_Rolling + FantasyPointsMean_Last5 + TargetShareMean_Last5 + FantasyPointsMean_Last3 + TargetShareMean_Last3 + OPR + QBInjured + WRInjured, data=train, importance=TRUE, ntree=1000, do.trace=50)
Forest
evaluate(Forest) # Not better than regression

Boost <- gbm(FantasyPoints  ~ FantasyPointsMean_Rolling + TargetShareMean_Rolling + FantasyPointsMean_Last5 + TargetShareMean_Last5 + FantasyPointsMean_Last3 + TargetShareMean_Last3 + OPR + QBInjured + WRInjured, data=train, n.trees=1000)
summary(Boost)
evaluate(Boost) # Not better than regression

Boost <- gbm(FantasyPoints  ~ FantasyPointsMean_Rolling + TargetShareMean_Rolling + FantasyPointsMean_Last5 + TargetShareMean_Last5 + FantasyPointsMean_Last3 + TargetShareMean_Last3 + OPR + QBInjured + WRInjured, data=train, n.trees=10000)
summary(Boost)
evaluate(Boost) # Not better than regression


##### FINAL MODEL #####
Reg <- lm(FantasyPoints ~ FantasyPointsMean_Rolling + TargetShareMean_Last3 + OPR + QBInjured + WRInjured, data=train)
summary(Reg) 
evaluate(Reg)

library(stargazer)
stargazer(Reg, type='html', title='WR Regression Model')

data = WR2021
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
write.csv(data, "~/Desktop/MGSC 434 Final Project/Optimization Data/WR2021.csv")












