library(glmnet)

# Gets the maximum absolute correlation between
# any two variables in list l_vars, given dataframe
# df
getMaxCorrel <- function(df, l_vars){
  df_corCheck <- df[, names(df) %in% l_vars]
  abs_cor <- abs(cor(df_corCheck))
  # replace all "1.0" values with 0.0
  abs_cor[abs_cor == 1.0] <- 0.0
  return (max(abs_cor))
}

# num. predictor variables needed in the model
num_vars <- 3 

# 'dh' is the dependent variable, and all else are
# predictor variables
df <- read.csv("data.csv")
x_vars <- df[, !names(df) %in% c('dh')]
y_var <- df$dh

lambda_seq <- 10^seq(2, -2, by = -0.05)
lasso.cv <- cv.glmnet(data.matrix(x_vars), y_var, alpha = 1, lambda = lambda_seq, nfolds = 5)
# lasso.cv$nzero gives the number of non-zero coefficients at each
# lambda. Hence, we can choose a suitable one from this list
lambda.select <- min(lasso.cv$lambda[which(lasso.cv$nzero == num_vars)])
l_lc <- coef(lasso.cv, s = lambda.select) # list of lasso coefficients (for lambda.select)   
l_vars <- l_lc@Dimnames[[1]][which(l_lc != 0 ) ] # list of selected variables
l_vars <- l_vars[l_vars != "(Intercept)"] # remove "(Intercept)" (string) from list of variables 

print (paste("Max. correlation: ", getMaxCorrel(df, l_vars)  ))

# Make model formula for lm
model_form <- paste("dh ~", paste(l_vars, collapse = ' + '))
curr_mod <- lm(model_form, data=df)
pv <- predict(curr_mod, newdata=df) # pv: predicted value

# Leave-one-out cross validation, to estimate the RMSE and %RMSE
# associated with the model.
#################### NOW, THE CROSS VALIDATION PART ##################################

# dataframe for predicted and actual values
df_pav <- NULL

# The main loop of the loocv: each time, drop a row, form
# a model with all other rows and test on the dropped row.
# This is essentially the code above, but in a loop
for (cr in 1:nrow(df)) { # cr: current row
  print(cr)
  # df_crd: dataframe, current row dropped.
  # This is the "train" part of the cross validation, used for
  # training/formulating the model
  df_crd <- df[-cr,]
  # df_tr: dataframe, test row (only). Just 1 row.
  # This is the "test" part, used for testing the formulated model
  df_tr <- df[cr,]
  print (paste("Test row values: ", (df_tr[1,])$dh, (df_tr[1,])$zmax_stG18 ))
  
  x_vars <- df_crd[, !names(df_crd) %in% c('dh')]
  y_var <- df_crd$dh
  
  lambda_seq <- 10^seq(2, -2, by = -0.05)
  lasso.cv <- cv.glmnet(data.matrix(x_vars), y_var, alpha = 1, lambda = lambda_seq, nfolds = 5)
  lambda.select <- min(lasso.cv$lambda[which(lasso.cv$nzero == num_vars)])
  l_lc <- coef(lasso.cv, s = lambda.select) # list of lasso coefficients (for lambda.select)   
  l_vars <- l_lc@Dimnames[[1]][which(l_lc != 0 ) ] # list of selected variables
  l_vars <- l_vars[l_vars != "(Intercept)"] # remove "(Intercept)" (string) 
  
  print (paste("Max. correlation: ", getMaxCorrel(df_crd, l_vars)  ))
  
  # Make model formula for lm
  model_form <- paste("dh ~", paste(l_vars, collapse = ' + '))
  print (model_form)
  curr_mod <- lm(model_form, data=df_crd)
  # Use the lm object to predict dep_var for the test row (df_tr)
  # pv: predicted value. Predict for the holdout row
  pv <- predict(curr_mod, newdata=df_tr)
  predicted <- c(pv)
  actual <- c(df_tr[1,]$dh)
  # One line (row) to be added...
  df_line <- data.frame(predicted, actual)
  if (is.null(df_pav)){ # first time
    df_pav <- df_line
  } else {
    df_pav <- rbind(df_pav, df_line) # append the new row
  }
}

# These functions give the RMSE and %RMSE associated with
# the cross validation 
RMSE(df_pav$predicted, df_pav$actual) 
relRMSE(df_pav$predicted, df_pav$actual) 


