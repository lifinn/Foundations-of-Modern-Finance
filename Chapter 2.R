
x = seq(-5, 5, 0.05)
y = 1/(1+exp(-x))
plot(x, y, col="blue", type = "l",)

# 1. Default flag definition and data preparation
# 1.1. Import data
library(readxl)
oneypd <- read_excel(
  "Chapter 2/chap2oneypd.xlsx",
  col_types = c(
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "text",
    "numeric",
    "numeric",
    "date",
    "date",
    "text",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "date",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "text",
    "numeric",
    "numeric",
    "numeric",
    "numeric"
  )
)
oneypd$ltv = as.factor(oneypd$ltv)
oneypd$arrears_status = as.factor(oneypd$arrears_status)
oneypd = data.frame(oneypd)

library(dplyr)
# 1.1.1. Data overview: data content and format
dplyr::glimpse(oneypd)

#| echo: false
# 1.1.2. Date format
library(vars)
oneypd <- dplyr::mutate_at(oneypd, vars(contains('date')), as.Date)
class(oneypd$origination_date)
# 1.1.3. Round arrears count fields
oneypd$max_arrears_12m <- round(oneypd$max_arrears_12m, 4)
oneypd$arrears_months <- round(oneypd$arrears_months, 4)

# 1.2. Default flag definition
oneypd <-
  dplyr::mutate(
    oneypd,
    default_event = if_else(
      oneypd$arrears_event == 1 |
        oneypd$term_expiry_event == 1 |
        oneypd$bankrupt_event == 1,
      1,
      0
    )
  )


# 1.3. Database split in train and test samples
# Recode default event variables for more convenient use
# 0-default, 1-non-default
oneypd$default_flag <-
  dplyr::if_else(oneypd$default_event == 1, 0, 1)

#Bureau score:
oneypd$woe_bureau_score <- rep(NA, length(oneypd$bureau_score))
oneypd$woe_bureau_score[which(is.na(oneypd$bureau_score))] <-
  -0.0910
oneypd$woe_bureau_score[which(oneypd$bureau_score <= 308)] <-
  -0.7994
oneypd$woe_bureau_score[which(oneypd$bureau_score > 308 &
                                oneypd$bureau_score <= 404)] <-
  -0.0545
oneypd$woe_bureau_score[which(oneypd$bureau_score > 404 &
                                oneypd$bureau_score <= 483)] <-
  0.7722
oneypd$woe_bureau_score[which(oneypd$bureau_score > 483)] <-  1.0375

#CC utilization:
oneypd$woe_cc_util <- rep(NA, length(oneypd$cc_util))
oneypd$woe_cc_util[which(is.na(oneypd$cc_util))] <- 0
oneypd$woe_cc_util[which(oneypd$cc_util <= 0.55)] <- 1.8323
oneypd$woe_cc_util[which(oneypd$cc_util > 0.55 &
                           oneypd$cc_util <= 0.70)] <- -0.4867
oneypd$woe_cc_util[which(oneypd$cc_util > 0.70 &
                           oneypd$cc_util <= 0.85)] <- -1.1623
oneypd$woe_cc_util[which(oneypd$cc_util > 0.85)] <- -2.3562

#Number of CCJ events:
oneypd$woe_num_ccj <- rep(NA, length(oneypd$num_ccj))
oneypd$woe_num_ccj[which(is.na(oneypd$num_ccj))] <- -0.0910
oneypd$woe_num_ccj[which(oneypd$num_ccj <= 0)] <- 0.1877
oneypd$woe_num_ccj[which(oneypd$num_ccj > 0 &
                           oneypd$num_ccj <= 1)] <- -0.9166
oneypd$woe_num_ccj[which(oneypd$num_ccj > 1)] <- -1.1322

#Maximum arrears in previous 12 months:
oneypd$woe_max_arrears_12m <-
  rep(NA, length(oneypd$max_arrears_12m))
oneypd$woe_max_arrears_12m[which(is.na(oneypd$max_arrears_12m))] <-
  0
oneypd$woe_max_arrears_12m[which(oneypd$max_arrears_12m <= 0)] <-
  0.7027
oneypd$woe_max_arrears_12m[which(oneypd$max_arrears_12m > 0 &
                                   oneypd$max_arrears_12m <= 1)] <-
  -0.8291
oneypd$woe_max_arrears_12m[which(oneypd$max_arrears_12m > 1 &
                                   oneypd$max_arrears_12m <= 1.4)] <-
  -1.1908
oneypd$woe_max_arrears_12m[which(oneypd$max_arrears_12m > 1.4)] <-
  -2.2223

#Maximum arrears balance in previous 6 months:
oneypd$woe_max_arrears_bal_6m <-
  rep(NA, length(oneypd$max_arrears_bal_6m))
oneypd$woe_max_arrears_bal_6m[which(is.na(oneypd$max_arrears_bal_6m))] <-
  0
oneypd$woe_max_arrears_bal_6m[which(oneypd$max_arrears_bal_6m <= 0)] <-
  0.5771
oneypd$woe_max_arrears_bal_6m[which(oneypd$max_arrears_bal_6m > 0 &
                                      oneypd$max_arrears_bal_6m <= 300)] <-
  -0.7818
oneypd$woe_max_arrears_bal_6m[which(oneypd$max_arrears_bal_6m > 300 &
                                      oneypd$max_arrears_bal_6m <= 600)] <-
  -1.2958
oneypd$woe_max_arrears_bal_6m[which(oneypd$max_arrears_bal_6m > 600 &
                                      oneypd$max_arrears_bal_6m <= 900)] <-
  -1.5753
oneypd$woe_max_arrears_bal_6m[which(oneypd$max_arrears_bal_6m > 900)] <-
  -2.2110

#Employment length (years):
oneypd$woe_emp_length <- rep(NA, length(oneypd$emp_length))
oneypd$woe_emp_length[which(is.na(oneypd$emp_length))] <- 0
oneypd$woe_emp_length[which(oneypd$emp_length <= 2)] <- -0.7514
oneypd$woe_emp_length[which(oneypd$emp_length > 2 &
                              oneypd$emp_length <= 4)] <- -0.3695
oneypd$woe_emp_length[which(oneypd$emp_length > 4 &
                              oneypd$emp_length <= 7)] <-  0.1783
oneypd$woe_emp_length[which(oneypd$emp_length > 7)] <- 0.5827

#Months since recent CC delinquency:
oneypd$woe_months_since_recent_cc_delinq <-
  rep(NA, length(oneypd$months_since_recent_cc_delinq))
oneypd$woe_months_since_recent_cc_delinq[which(is.na(oneypd$months_since_recent_cc_delinq))] <-
  0
oneypd$woe_months_since_recent_cc_delinq[which(oneypd$months_since_recent_cc_delinq <= 6)] <-
  -0.4176
oneypd$woe_months_since_recent_cc_delinq[which(
  oneypd$months_since_recent_cc_delinq > 6 &
    oneypd$months_since_recent_cc_delinq <= 11
)] <- -0.1942
oneypd$woe_months_since_recent_cc_delinq[which(oneypd$months_since_recent_cc_delinq > 11)] <-
  1.3166

#Annual income:
oneypd$woe_annual_income <- rep(NA, length(oneypd$annual_income))
oneypd$woe_annual_income[which(is.na(oneypd$annual_income))] <- 0
oneypd$woe_annual_income[which(oneypd$annual_income <= 35064)] <-
  -1.8243
oneypd$woe_annual_income[which(oneypd$annual_income > 35064 &
                                 oneypd$annual_income <= 41999)] <-
  -0.8272
oneypd$woe_annual_income[which(oneypd$annual_income > 41999 &
                                 oneypd$annual_income <= 50111)] <-
  -0.3294
oneypd$woe_annual_income[which(oneypd$annual_income > 50111 &
                                 oneypd$annual_income <= 65050)] <-
  0.2379
oneypd$woe_annual_income[which(oneypd$annual_income > 65050)] <-
  0.6234

save(oneypd, file = "Chapter 2/oneypd.rdata")

load("Chapter 2/oneypd.rdata")
# Perform a stratified sampling: 70% train and 30% test
library(caret)
set.seed(2122)
train.index <-
  caret::createDataPartition(oneypd$default_event, p = .7, list = FALSE)
train <- oneypd[train.index,]
test <- oneypd[-train.index,]

# Code to use for binning (Section 2. Univariate analysis)
# 2.1 woe based on binning analysis

# 2. Univariate analysis # Information Value (IV) assessment
library(smbinning)
iv_analysis <- smbinning.sumiv(df = train, y = "default_flag")
# Plot IV summary table
par(mfrow = c(1, 1))
smbinning.sumiv.plot(iv_analysis, cex = 1)

# 3. Multivariate analysis
# Compute Spearman rank correlation based on variables' WOE# based on Table 2.2 binning scheme
library(tidyverse)
woe_vars <- train %>% dplyr::select(starts_with("woe"))
woe_corr <- cor(as.matrix(woe_vars), method = 'spearman')
# Graphical inspection
library(corrplot)
corrplot(woe_corr, method = 'number')

# 4. Stepwise regression
# 4.1 Discard highly correlated variable
woe_vars_clean <-  woe_vars %>%
  dplyr::select(-woe_max_arrears_bal_6m)
#Support functions and databases
library(MASS)
attach(train)
# 4.2 Stepwise model fitting
logit_full <- glm(
  default_event ~ woe_bureau_score +
    woe_annual_income + woe_emp_length + woe_max_arrears_12m
  + woe_months_since_recent_cc_delinq + woe_num_ccj + woe_cc_util,
  family = binomial(link = 'logit'),
  data = train
)
logit_stepwise <-
  stepAIC(logit_full,
          k = qchisq(0.05, 1, lower.tail = F),
          direction = 'both')
detach(train)

# 4.2 Check the model
summary(logit_stepwise)
plot(logit_stepwise)
# Create a QQ plot
library(car)
qqPlot(logit_stepwise$resid, ylab="Residuals", xlab="Theoretical Quantiles")

# 1. Define a scaling function
scaled_score <- function(logit,
                         odds,
                         offset = 500,
                         pdo = 20)
{
  b = pdo / log(2)
  a = offset - b * log(odds)
  round(a + b * log((1 - logit) / logit))
}
# 2. Score the entire dataset
library(dplyr)
# 2.1 Use fitted model to score both test and train datasets
predict_logit_test <-
  predict(logit_stepwise, newdata = test, type = 'response')
predict_logit_train <-
  predict(logit_stepwise, newdata = train, type = 'response')
# 2.2 Merge predictions with train/test data
test$predict_logit <-
  predict(logit_stepwise, newdata = test, type = 'response')
train$predict_logit <-
  predict(logit_stepwise, newdata = train, type = 'response')
train$sample = 'train'
test$sample = 'test'
data_whole <- rbind(train, test)
data_score <- data_whole %>%
  dplyr::select(
    id,
    default_event,
    default_flag,
    woe_bureau_score,
    woe_annual_income,
    woe_max_arrears_12m,
    woe_months_since_recent_cc_delinq,
    woe_cc_util,
    sample,
    predict_logit
  )
# 2.3 Define scoring parameters in line with objectives
data_score$score <-
  scaled_score(data_score$predict_logit, 72, 660, 40)

hist(data_score$score)
hist(data_score$predict_logit)

# 1. Upload data
attach(data_score)
# 2. Fit logistic regression
pd_model <- glm(default_event ~ score,
                family = binomial(link = 'logit'),
                data = data_score)
pd_model <- glm(default_event ~ predict_logit,
                family = binomial(link = 'logit'),
                data = data_score)
summary(pd_model)
#               Estimate Std. Error z value    Pr(>|z|)
# (Intercept)  7.1357807  0.1855217   38.46 <0.0000 ***
# score       -0.0173218  0.0003519  -49.23 <0.0000 ***
# ---
# Signif. codes:  0 ‘***' 0.001 ‘**' 0.01 ‘*' 0.05 ‘.' 0.1 ‘ ' 1
# 2.1 Use model coefficients to obtain PDs
plot(pd_model)
# Create a QQ plot
library(car)
qqPlot(pd_model$resid, ylab="Residuals", xlab="Theoretical Quantiles")

predicted.data <- data.frame(
  probability.of.hd=logit_stepwise$fitted.values,
  hd=data_score$default_event)

predicted.data <- predicted.data[
  order(predicted.data$probability.of.hd, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

## Lastly, we can plot the predicted probabilities for each sample having
## heart disease and color by whether or not they actually had heart disease
ggplot(data=predicted.data, aes(x=rank, y=probability.of.hd)) +
  geom_point(aes(color=hd), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of default")

library(pROC)
plot(
  roc(train$default_event, train$predict_logit,
      direction = "<"),
  col = "blue",
  lwd = 3,
  main = "ROC Curve"
)

# 1. Create a validation database
# 1.1. Create score bands
library(smbinning)
score_cust <- smbinning.custom(
  data_score,
  y = 'default_flag',
  x = 'score',
  cuts = c(517, 576, 605, 632, 667, 716, 746, 773)
)
# 1.2. Group by bands
data_score <-
  smbinning.gen(data_score, score_cust, chrname = 'score_band')
# 2. Compare actual against fitted PDs
# 2.1. Compute mean values
data_pd <- data_score %>%
  dplyr::select(score, score_band, pd, default_event) %>%
  dplyr::group_by(score_band) %>%
  dplyr::summarise(mean_dr = round(mean(default_event), 4),
                   mean_pd = round(mean(pd), 4))
# 2.2. Compute rmse
rmse <- sqrt(mean((data_pd$mean_dr - data_pd$mean_pd) ^ 2))
# 0.002732317

# 1. Prepare the cross-validation dataset
data_subset <- data_whole %>%
  dplyr::select(
    id,
    default_event,
    default_flag,
    woe_bureau_score,
    woe_annual_income,
    woe_max_arrears_12m,
    woe_months_since_recent_cc_delinq,
    woe_cc_util,
    sample
  )
# 2. Perform the cross-validation loop
# 2.1 Initialise loop arguments and vectors
j <- 1 #initialise counter

m <- 20 #number of folds
n = floor(nrow(data_subset) / m) #size of each fold
auc_vector <- rep(NA, m)
gini_vector <- rep(NA, m)
ks_vector <- rep(NA, m)
# 2.2 Run the loop
attach(data_subset)
for (j in 1:m)
{
  s1 = ((j - 1) * n + 1) #start of the subset (fold)
  s2 = (j * n) # end of the subset (fold)
  data_cv_subset = s1:s2 #range of the subset (fold)
  train_set <- data_subset[-data_cv_subset,]
  test_set <-  data_subset[data_cv_subset,]
  # Model Fitting
  model <- glm(
    default_event ~ woe_bureau_score +
      woe_annual_income + woe_max_arrears_12m +
      woe_months_since_recent_cc_delinq + woe_cc_util,
    family = binomial(link =  'logit'),
    data = train_set
  )
  # Predict results
  predict_cv <- predict(model, newdata = test_set,
                        type =  'response')
  pred_obj <- ROCR::prediction(predict_cv, test_set[, 2])
  perf_obj <- ROCR::performance(pred_obj,  'tpr',  'fpr')
  # Calculate performance metrics for each fold/run:
  test_auc <- ROCR::performance(pred_obj,  'auc')
  auc_vector[j] <- test_auc@y.values[[1]]
  #gini_vector[j] <- optiRum::giniCoef(predict_cv,
                                      #test_set[, 2])
}
hist(auc_vector)

# 1. Upload data
library(readxl)
def <- read_excel("Chapter 2/chap2ptfregression.xlsx")
# 2. Fit linear regression
lm_DDELAY<- lm(formula=DEF~DDELAY,data = def)
summary(lm_DDELAY)
#              Estimate Std. Error t value Pr(>|t|)
# (Intercept) -0.550165   0.266763  -2.062  0.05391 .
# DDELAY       0.015241   0.004836   3.152  0.00552 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 0.3664 on 18 degrees of freedom
# Multiple R-squared:  0.3556,  Adjusted R-squared:  0.3198
# F-statistic: 9.934 on 1 and 18 DF,  p-value: 0.005516
