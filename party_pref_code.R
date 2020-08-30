# Part1

# Reading data
party113cong <- read.csv('D:/Zhao/Documents/Spring_2017/426/party113cong.csv', header = TRUE)
head(party113cong)


# Part 2

# Descriptive Statistics

apply(party113cong[, c(-1, -2, -3)], 2, min)
apply(party113cong[, c(-1, -2, -3)], 2, mean)
apply(party113cong[, c(-1, -2, -3)], 2, max)

sd(party113cong$totPop)

sort(summary(party113cong$state))

summary(party113cong$party)

## Part 3

# Make sure `R` is the reference level:

party113cong$party <- relevel(party113cong$party, ref = "R")

## Fitting Main-effect only logistic model

fullmod1 <- glm(party ~ medAge + medHouseIncome + medFamilyIncome + pctUnemp + pctPov + pctHS + pctBach + pctBlack + pctHisp, family = binomial, data = party113cong)

fullmod2 <- glm(party ~ (medAge + medHouseIncome + medFamilyIncome + pctUnemp + pctPov + pctHS + pctBach + pctBlack + pctHisp) ^ 2, family = binomial, data = party113cong)

fullmod3 <- glm(party ~ medAge + medHouseIncome + medFamilyIncome + pctUnemp + pctPov + pctHS + pctBach + pctBlack + pctHisp, family = binomial (link = probit), data = party113cong)

fullmod4 <- glm(party ~ (medAge + medHouseIncome + medFamilyIncome + pctUnemp + pctPov + pctHS + pctBach + pctBlack + pctHisp) ^ 2, family = binomial (link = probit), data = party113cong)

reducedmod1 <- step(fullmod1, direction = "backward", trace = FALSE)
reducedmod2 <- step(fullmod2, direction = "backward", trace = FALSE)
reducedmod3 <- step(fullmod3, direction = "backward", trace = FALSE)
reducedmod4 <- step(fullmod4, direction = "backward", trace = FALSE)

reducedmod1$aic
reducedmod2$aic
reducedmod3$aic
reducedmod4$aic

anova(fullmod1, reducedmod1)
anova(fullmod2, reducedmod2)
anova(fullmod3, reducedmod3)
anova(fullmod4, reducedmod4)

## Part 4

reducedmod2

cor(predict(reducedmod2, type = "response"), as.numeric(party113cong$party))

# Classification Table, sensitivity and specificity
pi0 <- 0.5

table(Actual=party113cong$party, Predicted=as.numeric(fitted(reducedmod2) > pi0))

161 / (161 + 41)
204 / (204 + 30)

# Cross-validated Classification Table

pihatcv <- numeric(nrow(party113cong))

for(i in 1:nrow(party113cong))
  pihatcv[i] <- predict(update(reducedmod2, subset=-i), newdata=party113cong[i,],
                        type="response")

table(Actual=party113cong$party, Predicted=as.numeric(pihatcv > pi0))

145 / (145 + 57)
192 / (192 + 42)

# ROC Curve
png('roccurve.png')

n <- nrow(party113cong)

pihat <- fitted(reducedmod2)

true.pos <- cumsum(as.numeric(party113cong$party)[order(pihat, decreasing=TRUE)]-1)

false.pos <- 1:n - true.pos

plot(false.pos/false.pos[n], true.pos/true.pos[n], type="l",
     main="ROC Curve", xlab="1 - Specificity", ylab="Sensitivity")
abline(a=0, b=1, lty=2, col="blue")
dev.off()

partyhat <- ifelse(fitted(reducedmod2) >= .5, "D", "R")


## An alternative method to calculate sensitivity and specificity, learned from another class
library(caret)
confusionMatrix()
result <- table(predicted=ifelse(fitted(reducedmod2)>=.5, 1, 0), actual=as.numeric(party113cong$party)-1)
confusionMatrix(result)

## COncordance index

mean(outer(pihat[party113cong$party=="D"], pihat[party113cong$party== "R"], ">") +
       0.5 * outer(pihat[party113cong$party=="D"], pihat[party113cong$party== "R"], "=="))


## Part 5
smallstates <- summary(party113cong$state)[summary(party113cong$state) ==1 ]

data2 <- party113cong[party113cong$state  %in% names(smallstates) == FALSE,]

wealth <- ifelse(data2$medHouseIncome > 52000, "Wealthy", "Non-Wealthy" )
wealth <- as.factor(wealth)
wealth <- relevel(wealth, ref = "Non-Wealthy")
data2 <- cbind(data2, wealth)


# Logistic Regression

mod9 <- glm(party ~ state + wealth, family = binomial, data = data2)
summary(mod9)
-1.099 + c(-1, 1) * 1.96 * sqrt(.2943)
exp(-1.099 + c(-1, 1) * 1.96 * sqrt(.2943))

mod10 <- glm(party ~ wealth, family = binomial, data = data2)
summary(mod10)


# Mantel-Haenzsel Analysis




party.array <- xtabs( ~  wealth +party + state, data = data2, drop.unused.levels = TRUE)


mantelhaen.test(party.array, correct=FALSE)
