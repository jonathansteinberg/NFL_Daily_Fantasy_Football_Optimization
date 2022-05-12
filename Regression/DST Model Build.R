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


########## DST MODEL ##########
data = na.omit(DST) # put different position here 
data = data[data$Week != 17,]
set.seed(123)
sample = sort(sample(nrow(data),nrow(data)*0.7))
train = data[sample,]
test = data[-sample,]

# Function to evaluate regressions 
evaluate <- function(Reg) {
  y_pred = predict(Reg, newdata = test)
  y_true = test$FantasyPoints
  mse <- MSE(y_pred, y_true)
  dev <- sqrt(mse)
  return(dev)}

### Fantasy Points For: Simple linear regressions to determine which average is best
Reg_FPRolling <- lm(FantasyPoints ~ FantasyPointsFor_MeanRolling, data=train)
summary(Reg_FPRolling)

Reg_FPLast3 <- lm(FantasyPoints ~ FantasyPointsFor_MeanLast3, data=train)
summary(Reg_FPLast3)

Reg_FPLast5 <- lm(FantasyPoints ~ FantasyPointsFor_MeanLast5, data=train)
summary(Reg_FPLast5)
# ROLLING

### Fantasy Points Against: Simple linear regressions to determine which average is best
Reg_FPRolling <- lm(FantasyPoints ~ FantasyPointsAgainst_MeanRolling, data=train)
summary(Reg_FPRolling)

Reg_FPLast3 <- lm(FantasyPoints ~ FantasyPointsAgainst_MeanLast3, data=train)
summary(Reg_FPLast3)

Reg_FPLast5 <- lm(FantasyPoints ~ FantasyPointsAgainst_MeanLast5, data=train)
summary(Reg_FPLast5)
# ROLLING

### QBInjured: Simple linear regressions
Reg_QBInjured <- lm(FantasyPoints ~ QBInjured, data=train)
summary(Reg_QBInjured)
# No stars

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


### Multiple regression model

Reg <- lm(FantasyPoints ~ FantasyPointsFor_MeanRolling + FantasyPointsAgainst_MeanRolling, data=train)
summary(Reg)
evaluate(Reg)

Reg <- lm(FantasyPoints ~ FantasyPointsFor_MeanRolling + FantasyPointsAgainst_MeanLast3, data=train)
summary(Reg)
evaluate(Reg)

Reg <- lm(FantasyPoints ~ FantasyPointsFor_MeanRolling + FantasyPointsAgainst_MeanLast5, data=train)
summary(Reg)
evaluate(Reg)

Reg <- lm(FantasyPoints ~ FantasyPointsFor_MeanLast3 + FantasyPointsAgainst_MeanRolling, data=train)
summary(Reg)
evaluate(Reg) # Lowest MSE

Reg <- lm(FantasyPoints ~ FantasyPointsFor_MeanLast3 + FantasyPointsAgainst_MeanLast3, data=train)
summary(Reg)
evaluate(Reg)

Reg <- lm(FantasyPoints ~ FantasyPointsFor_MeanLast3 + FantasyPointsAgainst_MeanLast5, data=train)
summary(Reg)
evaluate(Reg)

Reg <- lm(FantasyPoints ~ FantasyPointsFor_MeanLast5 + FantasyPointsAgainst_MeanRolling, data=train)
summary(Reg)
evaluate(Reg)

Reg <- lm(FantasyPoints ~ FantasyPointsFor_MeanLast5 + FantasyPointsAgainst_MeanLast3, data=train)
summary(Reg)
evaluate(Reg)

Reg <- lm(FantasyPoints ~ FantasyPointsFor_MeanLast5 + FantasyPointsAgainst_MeanLast5, data=train)
summary(Reg)
evaluate(Reg)


### Try with injuries - worse
Reg <- lm(FantasyPoints ~ FantasyPointsFor_MeanLast3 + FantasyPointsAgainst_MeanRolling + QBInjured, data=train)
summary(Reg) # not sig
evaluate(Reg) 

Reg <- lm(FantasyPoints ~ FantasyPointsFor_MeanLast3 + FantasyPointsAgainst_MeanRolling + RBInjured, data=train)
summary(Reg) # not sig
evaluate(Reg) 

Reg <- lm(FantasyPoints ~ FantasyPointsFor_MeanLast3 + FantasyPointsAgainst_MeanRolling + WRInjured, data=train)
summary(Reg) # not sig
evaluate(Reg) 

Reg <- lm(FantasyPoints ~ FantasyPointsFor_MeanLast3 + FantasyPointsAgainst_MeanRolling + TEInjured, data=train)
summary(Reg) # not sig
evaluate(Reg) 

### Final Model
Reg <- lm(FantasyPoints ~ FantasyPointsFor_MeanLast3 + FantasyPointsAgainst_MeanRolling, data=train)
summary(Reg)
evaluate(Reg)

library(stargazer)
stargazer(Reg, type='html', title='DST Regression Model')

data = DST2021
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
write.csv(data, "~/Desktop/MGSC 434 Final Project/Optimization Data/DST2021.csv")






















