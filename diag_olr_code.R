library(MASS)
require(foreign)
require(Hmisc)
require(reshape2)
require(GGally)

diag <- read.csv("~/Documents/CPP Grad/SPRING 2020/STA 5900/HW 5/diag_data.csv")

diag$Damage <- factor(diag$Damage)
diag$Crop.History <- factor(diag$Crop.History)

names(diag)
dim(diag)
summary(diag)
cor(diag[,unlist(lapply(diag, is.numeric))])


# test for multicolinearity 
fit2 <- lm(as.numeric(Damage) ~ Rainfall + Temperature + Wind + Crop.History, data = diag)
car::vif(fit2) # since all of our values are small we can assume there is no multicolinearity

attach(diag)

library(ggplot2)
sp <- ggplot(diag, aes(x=Rainfall, y=Wind)) + geom_point(shape=20)
sp + facet_grid(Crop.History ~ Damage)

# PROPORTIONAL ODDS ASSUMPTION

sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)),
    'Y>=4' = qlogis(mean(y >= 4)),
    'Y>=5' = qlogis(mean(y >= 5)),
    'Y>=6' = qlogis(mean(y >= 6)))
}
#'Y>=8' = qlogis(mean(y >= 8)),
#'Y>=9' = qlogis(mean(y >= 9))

(s <- with(diag, summary(as.numeric(Damage) ~ Rainfall + Wind + Crop.History, fun=sf)))

glm(I(as.numeric(Damage) >= 7) ~ Rainfall, family="binomial", data = diag)

glm(I(as.numeric(Damage) >= 8) ~ Rainfall, family="binomial", data = diag)

s[, 4] <- s[, 4] - s[, 3]
s[, 3] <- s[, 3] - s[, 3]

s[, 5] <- s[, 5] - s[, 4]
s[, 4] <- s[, 4] - s[, 4]

s[, 6] <- s[, 6] - s[, 5]
s[, 5] <- s[, 5] - s[, 5]

s[, 7] <- s[, 6] - s[, 5]
s[, 6] <- s[, 6] - s[, 6]

plot(s, which=1:6, xlab='logit', main=' ', xlim=c(-4, 1))


# R function to test for proportional odds ratio
library(brant)
brant(model_fit_2)

# MODEL SELECTION

model_fit <- polr(Damage~Rainfall + Temperature + Wind + Crop.History, data = diag, Hess = TRUE)
summary(model_fit)

summary_table <- coef(summary(model_fit))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table #p-values excpet for Rainfall are all unsignificant, also the std. error is also very bad

# model without Temperature
model_fit_2 <- polr(Damage~Rainfall + Wind + Crop.History, data = diag, Hess = TRUE)
summary(model_fit_2)

summary_table <- coef(summary(model_fit_2))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table
# the summary from this model shows that our model (specfically in standard error) in predicting the damage level of a crop is better when excluding Temperature (no big difference in Residual Deviance and AIC, but they are slightly better)

exp(coef(model_fit_2))

# PREDICTION USING MODEL

# Prediction of Damage Class of Extreme Weather Conditions on the Three Types of Fields

# predict probability of extreme cases with soybean field
new_data_1 <- data.frame("Rainfall"= 5,"Wind"= 12,"Crop.History"="1")
round(predict(model_fit_2,new_data_1,type = "p"), 8) # probability highest for most damage classes (8 and 9)

# predict probability of extreme cases with oats field
new_data_2 <- data.frame("Rainfall"= 5,"Wind"= 12,"Crop.History"="2")
round(predict(model_fit_2,new_data_2,type = "p"), 8) # probability highest for most damage classes (6 and 8)

# predict probability of extreme cases with snap beans field
new_data_3 <- data.frame("Rainfall"= 5,"Wind"= 12,"Crop.History"="3")
round(predict(model_fit_2,new_data_3,type = "p"), 8) # probability highest for most damage classes (6,8 and 9)

# Prediction of Damage Class of Unextreme Weather Conditions on the Three Types of Fields

# predict probability of unextreme cases with soybean field
new_data_4 <- data.frame("Rainfall"= 1.3,"Wind"= 7.5,"Crop.History"="1")
round(predict(model_fit_2,new_data_4,type = "p"), 8) # probability highest for least damage classes (1 and 2)

# predict probability of unextreme cases with oats field
new_data_5 <- data.frame("Rainfall"= 1.3,"Wind"= 7.5,"Crop.History"="2")
round(predict(model_fit_2,new_data_5,type = "p"), 8) # probability highest for least damage classes (1 and 2)

# predict probability of unextreme cases with snap bean field
new_data_6 <- data.frame("Rainfall"= 1.3,"Wind"= 7.5,"Crop.History"="3")
round(predict(model_fit_2,new_data_6,type = "p"), 8) # probability highest for least damage classes (1 and 2)

# Prediction of Damage Class of Moderate Weather Conditions on the Three Types of Fields

# predict probability of unextreme cases with soybean field
new_data_7 <- data.frame("Rainfall"= 3,"Wind"= 10.7,"Crop.History"="1")
round(predict(model_fit_2,new_data_7,type = "p"), 8) # probability highest for moderate damage classes (3,4,and 6)

new_data_8 <- data.frame("Rainfall"= 3,"Wind"= 10.7,"Crop.History"="2")
round(predict(model_fit_2,new_data_8,type = "p"), 8) # probability highest for least damage classes (1 and 2)

new_data_9 <- data.frame("Rainfall"= 3,"Wind"= 10.7,"Crop.History"="3")
round(predict(model_fit_2,new_data_9,type = "p"), 8) # probability highest for moderate classes (2,3,and 4)


# OVERALL PREDICTION

#finds the probability under each group
newdat <- data.frame(
  Rainfall = rep(seq(from = 1.23, to = 5.01, length.out = 9), 4),
  Wind = rep(seq(from=7.4, to = 12.9, length.out = 9), 4),
  Crop.History =  rep(levels(Crop.History), 12)
)

newdat <- cbind(newdat, predict(model_fit_2, newdat, type = "probs"))


# plot the predicted probabilities for the different conditions
lnewdat <- melt(newdat, id.vars = c("Rainfall", "Wind", "Crop.History"),
                variable.name = "Damage", value.name="Probability")
crop_names <- list(
  '<fct>:1'="Soybeans",
  '<fct>:2'="Oats",
  '<fct>:3'="Snap Beans"
)

crop_labeller <- function(variable,value){
  return(crop_names[value])
}

# plot of probabilities for each group with rainfall and crop history 
ggplot(lnewdat, aes(x = Rainfall, y = Probability, colour = Damage)) +
  geom_line() + facet_grid(Crop.History, labeller=crop_labeller)

# plot of probabilites for each group with wind and crop history 
ggplot(lnewdat, aes(x = Wind, y = Probability, colour = Damage)) +
  geom_line() + facet_grid(Crop.History, labeller=crop_labeller)

