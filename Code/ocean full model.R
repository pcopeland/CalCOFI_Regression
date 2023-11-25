rm(list=ls())
ocean_data = read.csv("ocean1.csv")
head(ocean_data)
ocean_data <- ocean_data[, -1]
# Assuming 'ocean_data' is your dataset
model <- lm(Salnty ~ . - Year - Month - Wea + as.factor(Year) + as.factor(Month) + as.factor(Wea), data = ocean_data)
# Print a summary of the regression model
summary(model)
install.packages("lars")
library(lars)
# Assuming 'ocean_data' is already loaded and cleaned as per your previous steps
predictors <- model.matrix(Salnty ~ . - Year - Month - Wea + as.factor(Year) + as.factor(Month) + as.factor(Wea), ocean_data)[, -1] # Predictor variables
sat.scaled <- scale(ocean_data$Salnty) # Response variable, scaled
lasso_object <- lars(x=predictors, y=sat.scaled, type="lasso")
summary(lasso_object)
plot(lasso_object)
plot.lars(lasso_object)
plot.lars(lasso_object, xvar="df", plottype="Cp")
# Assuming your lars object is stored in 'lasso_object'

# Find the step with the minimum Cp
min_cp_step <- which.min(lasso_object$criterion$Cp)

# Extract the coefficients at that step
optimal_coefficients <- coef(lasso_object, s = min_cp_step)

# Print the optimal coefficients
print(optimal_coefficients)
library(glmnet)
ocean_data$Year <- as.factor(ocean_data$Year)
ocean_data$Month <- as.factor(ocean_data$Month)
ocean_data$Wea <- as.factor(ocean_data$Wea)

Xpred <- model.matrix(~ Depthm + T_degC + O2ml_L + STheta + O2Sat + Oxy_µmol.Kg + 
                        Bottom_D + as.factor(Year) + as.factor(Month) + 
                        as.factor(Wea) - 1, data = ocean_data)
sat <- ocean_data$Salnty  # Replace 'Salnty' with the correct response variable name
set.seed(123)  # For reproducibility
satmodel_cv <- cv.glmnet(Xpred, sat, alpha=1, nfolds=10)
satmodel_cv
satmodel <- glmnet(Xpred, sat, alpha = 1, nlambda = 100)
optimal_coefficients <- coef(satmodel, s = satmodel_cv$lambda.min)
optimal_coefficients
plot(satmodel, xvar = "lambda", lwd = 2)
abline(v = log(satmodel_cv$lambda.min), col = 'black', lty = 2)
#Elastic Net Regression
set.seed(123) # For reproducibility
satmodel_cv <- cv.glmnet(Xpred, sat, alpha=0.5, nfolds=10)
satmodel <- glmnet(Xpred, sat, alpha = 0.5, nlambda = 100)
optimal_coefficients <- coef(satmodel, s=satmodel_cv$lambda.min)
plot(satmodel, xvar="lambda", lwd=2)
abline(v=log(satmodel_cv$lambda.min), col='black', lty=2, lwd=2)

