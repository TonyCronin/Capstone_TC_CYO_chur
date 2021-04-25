# set up libs
# -------------------------------------------------------------------------

knitr::opts_chunk$set(echo = TRUE)

packages <- c("tidyverse", "gridExtra", "grid", "lattice", "knitr", "lime", "tidyquant", "rsample", "recipes", "yardstick", "corrr", "readr","reshape2", "caret", "mgcv", "earth", "cutpointr",  "flextable", "mlbench", "pROC", "randomForest", "gbm", "rstudioapi", "gridExtra", "corrplot", )

## Install packages From CRAN if they are not installed.
for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p, repos = 'http://cran.us.r-project.org')
  }
}
## Load the packages
for (p in packages) {
  suppressPackageStartupMessages(
    library(p, quietly = TRUE, character.only =TRUE ) 
  )
}

# remove everything
rm(list = c(ls()))

# i prefer to not to have scientific numbers
options(scipen = 999)

# set working directory to current document path
setwd(dirname(getActiveDocumentContext()$path))

# import the data ---------------------------------------------------------

churn_data_raw  <- readr::read_csv("data/WA_Fn-UseC_-Telco-Customer-Churn.csv")

# load bank churn
bank_churn <- readr::read_csv("data/Churn_Modelling.csv")


# clean up data -----------------------------------------------------------

teleco_churn_data_tbl <- churn_data_raw %>%
  select(-customerID) %>%
  drop_na() %>%
  select(Churn, everything()) %>% 
  mutate(Churn = if_else(Churn == "Yes", 1, 0)) %>% 
  mutate(mutate(across(where(is_character),as_factor)))

bank_churn_data_tbl <- bank_churn %>%
  select(-c(RowNumber, CustomerId, Surname)) %>%
  drop_na() %>%
  select(Exited, everything()) %>% 
  mutate(mutate(across(where(is_character),as_factor))) %>% 
  rename(Churn = Exited)#

# An introduction/overview/executive summary ---------------------
# section that describes the dataset and variables, and summarizes the goal of the project and key steps that were performed.
# 
# Goal of this project is to calculate the potential real world monetary impact of a churn prediction algorithm. I will use data available from Santanders bank customer data.
# 
# 
# I will use Crisp DM process - https://en.wikipedia.org/wiki/Cross-industry_standard_process_for_data_mining. 
# 
# 1: Data Understanding -------------------------

glimpse(bank_churn_data_tbl)

summary(bank_churn_data_tbl)

# CreditScore: ranges from 350 to 850, with a mean of 650.
# 
# Geography: customers come from three countries from largest to smallest France, Germany and Spain.
# 
# Gender: 5457 Men and 4543 Women
# 
# Age: the customers age range from 18 to 92, the average is 38
# 
# Tenure: how many year a bank customer range from 0 to 10, the average is 5.
# 
# Balance: how much money does a customer have in the bank, range from Euro 0 to 250,898, the average is 76,486
# 
# NumOfProducts: banking products per customer, range 1 to 4, average is 1.53
# 
# HasCrCard: owns a credit card, 7055 customers of the 10000 total.
# 
# IsActiveMember: presumably, makes regular deposits, 5151 of the 10000 total.
# 
# EstimatedSalary: customer’s annual salary, range from Euro 11.58 to 199992, mean is 100090.
# 
# Churn: the target variable, I've changed the column name from 'Exited' to 'Churn', as the analysis is about churn. A 1 in this cell tells us a customer has churned. 2037 of the total 10000 has churned.
# 
# how many customers churned
# # A tibble: 2 x 2
#   Churn        n
#   <chr>    <int>
#   1 Churned   2037
#   2 Remained  7963
bank_churn_n <- bank_churn_data_tbl %>% 
  group_by(Churn) %>% 
  mutate(Churn = if_else(Churn == 1, "Churned", "Remained")) %>% 
  tally() 

bank_churn_n%>% 
  ggplot(aes(Churn, n, fill = Churn)) +
  geom_col() +
  geom_text(label = bank_churn_n$n) +
  theme(legend.position = 'none') +
  labs(title = "Bank Churn: Number of Customers who have churned", 
       y = "Number of Customers")

# what are the variables in the data set
# set observations and variables
bank_rows <- nrow(bank_churn_data_tbl)
bank_cols <- ncol(bank_churn_data_tbl)

bank_variables <- names(bank_churn_data_tbl)
bank_variables <- paste0(bank_variables, collapse = ", ") 

# lets melt the shit out of that data so we can quickly display the data
melt.bank <- melt(bank_churn_data_tbl)

# 
p2 <- melt.bank %>% 
  ggplot(aes(x = value, fill = variable)) + 
  stat_density(show.legend = FALSE) + 
  facet_wrap( ~variable, scales = "free") +
  labs(title = "Bank Variables")

p2

# I can see that Age, CreditScore and Balance all have a skewed distributions. 
# 
# Lets see how they relate to churn.
# 40 to 50 age range seems to be the age to churn, younger customers seem to be more stable. Could it be older customers have done with saving and now need to invest their money?
age_hist_gg <- bank_churn_data_tbl %>% 
  mutate(Churn = as.factor(Churn)) %>% 
  ggplot(aes(x = Age, fill = Churn)) +
  geom_histogram(binwidth = 5, show.legend = TRUE) +
  scale_x_continuous(breaks = seq(0,100, by=10)) +
  labs(title = "Bank churn: Histogram of churned customers by age")

age_hist_gg


# No major difference here, churned by balance is only significant by 0 balance, customers are much more likely to churn, if they have no deposit.
balance_hist_gg <- bank_churn_data_tbl %>% 
  mutate(Churn = as.factor(Churn)) %>% 
  ggplot(aes(x = Balance, fill = Churn)) +
  geom_histogram(show.legend = TRUE) +
  scale_x_continuous(breaks = seq(0,max(bank_churn_data_tbl$Balance), by=5000)) +
  labs(title = "Bank churn: Histogram of churned customers by balance")

# lets explore the categorical features 
# 
# Germany seems to be the churn capiatil
bank_churn_data_tbl %>%
  mutate(Churn = if_else(Churn == 1, "Churned", "Remained")) %>% 
  dplyr::group_by(Geography, Churn) %>% 
  tally() %>% 
  ggplot(aes(Geography, n, fill = Churn )) + 
  geom_bar(position="fill", stat="identity") +
  labs(title = "Bank churn: Ratio of churned customers by Geography")

# if we measure the balance, germany has the biggest balance that churned
bank_churn_data_tbl %>%
  mutate(Churn = if_else(Churn == 1, "Churned", "Remained")) %>% 
  dplyr::group_by(Geography, Churn) %>% 
  summarise(balance = sum(Balance)) %>% 
  ggplot(aes(Geography, balance, fill = Churn )) + 
  geom_bar(position="dodge", stat="identity") +
  labs(title = "Bank churn: Ratio of churned customers by Geography and Balance")

# 25 percent of females churned, a higher proprtion of the males
bank_churn_data_tbl %>%
  mutate(Churn = if_else(Churn == 1, "Churned", "Remained")) %>% 
  dplyr::group_by(Gender, Churn) %>% 
  tally() %>% 
  ggplot(aes(Gender, n, fill = Churn )) + 
  geom_bar(position="fill", stat="identity") +
  labs(title = "Bank churn: Ratio of churned customers by gender")

# does the balance effect the churn?
bank_churn_data_tbl %>%
  mutate(Churn = if_else(Churn == 1, "Churned", "Remained")) %>% 
  mutate(BalanceCuts = cut(Balance, breaks = 5,
                           labels = c("First", "Second", "Third", "Fourth", "Fifth"))) %>%
  dplyr::group_by(BalanceCuts, Churn) %>% 
  tally() %>% 
  ggplot(aes(BalanceCuts, n, fill = Churn )) + 
  geom_bar(position="fill", stat="identity") +
  labs(title = "Bank churn: Ratio of churned customers by Balance (stratified)")

# customers with more products churn more. 
bank_churn_data_tbl %>%
  mutate(Churn = if_else(Churn == 1, "Churned", "Remained")) %>% 
  dplyr::group_by(NumOfProducts, Churn) %>% 
  tally() %>% 
  ggplot(aes(NumOfProducts, n, fill = Churn )) + 
  geom_bar(position="fill", stat="identity") +
  labs(title = "Bank churn: Ratio of churned customers by NumOfProducts")

# credits card has no effect 
bank_churn_data_tbl %>%
  mutate(Churn = if_else(Churn == 1, "Churned", "Remained")) %>% 
  dplyr::group_by(HasCrCard, Churn) %>% 
  tally() %>% 
  ggplot(aes(HasCrCard, n, fill = Churn )) + 
  geom_bar(position="fill", stat="identity") +
  labs(title = "Bank churn: Ratio of churned customers by HasCrCard")

# less active members seen to churn more
bank_churn_data_tbl %>%
  mutate(Churn = if_else(Churn == 1, "Churned", "Remained")) %>% 
  mutate(IsActiveMember = if_else(IsActiveMember == 1, "Active", "Less Active")) %>% 
  dplyr::group_by(IsActiveMember, Churn) %>% 
  tally() %>% 
  ggplot(aes(IsActiveMember, n, fill = Churn )) + 
  geom_bar(position="fill", stat="identity") +
  labs(title = "Bank churn: Ratio of churned customers by IsActiveMember")



balance_hist_gg

# lets correlate the numeric factors to see if multi-colinaritly
# correlation analysis ----------------------------
# analyze the bank influencing factors
cor_bank <- bank_churn_data_tbl %>% 
  keep(is.numeric) %>% 
  cor()
  
corrplot(cor_bank, method = "circle")

# split data --------------------------------------------------------------

# factorize the telco churn data churn target variable

# create sample index,  
telco_train_Index <- teleco_churn_data_tbl$Churn %>% 
  caret::createDataPartition(p = .8, list = FALSE)

teleco_train <- teleco_churn_data_tbl[c(telco_train_Index), ]
teleco_test  <- teleco_churn_data_tbl[-c(telco_train_Index), ] 

# factorize the bank churn data churn target variable
bank_data <- bank_churn_data_tbl 

# create sample index,  
bank_train_Index <-  bank_churn_data_tbl $Churn %>% 
  caret::createDataPartition(p = .8, list = FALSE)

bank_train <-  bank_churn_data_tbl[c(bank_train_Index), ]
bank_test  <-  bank_churn_data_tbl[-c(bank_train_Index), ] 

# caret pre process
PP <- c('center', 'scale')

# a helper function to help us understand the impact of our cut off values later
cutoff_fun <- function(x, df){
  # we are returning a list each run from 1: 10
  datatable <- c(
    # we are running from 1 to 9 using ldply, so divide x by 10 to get a decimal
    x <- x/10,
    # Compute the percentage of correctly classified customers who stayed
    as.numeric( sum(( df$predicted <= x ) & ( df$Churn == 0) )/ sum( df$Churn == 0 )),
    # customer has not churned rate of prediction is greater that x
    as.numeric( sum(if_else( df$Churn == 0, df$predicted <= x, FALSE ))),
    # Compute the percentage of correctly classified customers who left
    sum(( df$predicted > x ) & ( df$Churn == 1 ))/sum( df$Churn == 1 ),
    # customer has churned and prediction is greater than x
    as.numeric( sum(if_else( df$Churn == 1, df$predicted > x, FALSE ))),
    # sum( df$prediction > x ),
    # a precantage of correctly exited customers
    mean(( df$predicted > x ) == ( df$Churn == 1 )) 
  )
  # rename the datatable
  names( datatable ) <- c( "cutoff/prediction", "stayers_classified ", "stayers_classifed_number",
                           "leavers_classified", "leavers_classified_number", "percentage_correct" )
  return( datatable )
}

# telco knn ---------------------------------------------------------------

# train the model
telco_knn_model <- train(
  Churn ~., data = teleco_train, method = "knn",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneLength = 20
)

# Plot model accuracy vs different values of k
plot(telco_knn_model, main = "Telco Churn: Best value for K")

# Print the best tuning parameter k that
# maximizes model accuracy
# 41 for telco
telco_knn_best_tune <- telco_knn_model$bestTune

teleco_test_knn <- teleco_test

# Add a new column, to hold the predictions on the test data
teleco_test_knn$predicted <- telco_knn_model %>% 
  predict( teleco_test_knn )

# Compute model accuracy rate 
knn_roc_plot <- pROC::roc( Churn ~ predicted, teleco_test_knn )

plot(knn_roc_plot, print.thres=TRUE, main = "Telco Churn: ROC ")

# my threshold is 
knn_threshold = 0.313

bank_cutoff_knn_df <- plyr::ldply( 1:9, teleco_test_knn, .fun = cutoff_fun)
# 
# bank_cutoff_knn_df

teleco_test_knn %>% 
  ggplot( aes( predicted, color = as.factor(Churn) ) ) + 
  geom_density( size = 1 ) +
  ggtitle( "GLM Training Set's Predicted Score" )

churn_bars_gg <- function(data_df = teleco_test_knn, 
                          threshold_num = 0.313, 
                          text_str = "Telco Churn: KNN" ) {
  
 data_df %>% 
    mutate(predicted = predicted >= threshold_num ) %>% 
    mutate(Churn = as.factor(Churn)) %>% 
    group_by(Churn) %>% 
    summarise(`predicted to churn`= sum(predicted),
              `not predicted` = n() - `predicted to churn`) %>% 
    melt(id.vars = "Churn") %>% 
    ggplot( aes( Churn, value, fill = variable  )) + 
    geom_col( ) +
    labs( title = paste0(text_str, " Training Set's Predicted Score"),
          subtitle = paste0("Cutoff Value of ", threshold_num)) 
  
}

churn_density <- function(data_df = teleco_test_knn, 
                          threshold_num = 0.313, 
                          text_str = "Telco Churn: KNN") {
  
  data_df %>% 
    ggplot( aes( predicted, color = as.factor(Churn) ) ) + 
    geom_density( size = 1 ) +
    geom_vline( xintercept = threshold_num, linetype = "dashed" ) +
    labs( title = paste0( text_str, " Training Set's Predicted Score, density plot" ),
          subtitle = paste0( "Cuttoff line added at ", threshold_num )) 
  
}

# lets see how our model preforms predicting true positives and false positives
churn_bars_gg( )

# distribution of the prediction score grouped by known outcome
# Thus for a ideal double density plot you want the distribution of scores to be 
# separated, with the score of the negative instances to be on the left and 
# the score of the positive instance to be on the right.
churn_density( )

# teleco gbm --------------------------------------------------------------

set.seed(123)

# train the model
telco_gbm_model <- train(
  Churn ~., data = teleco_train, method = "gbm",
  trControl = trainControl("cv", number = 10),
  bag.fraction=0.75
)

# Plot model accuracy vs different values of k
plot(telco_gbm_model)

teleco_test_gbm <- teleco_test

# Add a new column, to hold the predictions on the test data
teleco_test_gbm$predicted <- telco_gbm_model %>% 
  predict( teleco_test_gbm )

gbm_roc_plot <- pROC::roc( Churn ~ predicted, teleco_test_gbm )

plot(gbm_roc_plot, print.thres=TRUE, main = "Telco Churn: GBM ROC ")

# my threshold is 
threshold_telco_gbm = 0.239

telco_churn_gbm <- churn_bars_gg(data_df = teleco_test_gbm, 
                          threshold_num = threshold_telco_gbm, 
                          text_str = "Telco Churn: GBM " )
  
telco_churn_gbm

telco_churn_density_gbm <- churn_density(data_df = teleco_test_gbm, 
                          threshold_num = threshold_telco_gbm, 
                          text_str = "Telco Churn: GBM " )

telco_churn_density_gbm

# teleco earth  ------------------------------------------------------------

set.seed(123)

# train the model
telco_earth_model <- train(
  Churn ~., data = teleco_train, method = "earth",
  trControl = trainControl("cv", number = 10)
)

# Plot model accuracy vs different values of k
plot(telco_earth_model)


teleco_test_earth <- teleco_test

# Add a new column, to hold the predictions on the test data
teleco_test_earth$predicted <- telco_earth_model %>% 
  predict( teleco_test_earth )


earth_roc_plot <- pROC::roc( Churn ~ predicted, teleco_test_earth )

plot(earth_roc_plot, print.thres=TRUE, main = "Telco Churn: Earth ROC ")

# my threshold is 
threshold_telco_earth = 0.285

telco_churn_earth <- churn_bars_gg(data_df = teleco_test_earth, 
                                 threshold_num = threshold_telco_earth, 
                                 text_str = "Telco Churn: Earth " )

telco_churn_earth

telco_churn_density_earth <- churn_density(data_df = teleco_test_gbm, 
                                         threshold_num = threshold_telco_earth, 
                                         text_str = "Telco Churn: Earth " )

telco_churn_density_earth

# teleco glmnet -----------------------------------------------------------

set.seed(123)

# train the model
telco_glmnet_model <- train(
  Churn ~., data = teleco_train, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center", "scale"),
  tuneGrid=expand.grid(
    .alpha=1,
    .lambda=seq(0, 100, by = 0.1))
)

# Plot model accuracy vs different values of k
plot(telco_glmnet_model)


teleco_test_glmnet <- teleco_test

# Add a new column, to hold the predictions on the test data
teleco_test_glmnet$predicted <- telco_glmnet_model %>% 
  predict( teleco_test_glmnet )

teleco_test_glmnet_roc_plot <- pROC::roc( Churn ~ predicted, teleco_test_glmnet )

plot(teleco_test_glmnet_roc_plot, print.thres=TRUE, main = "Telco Churn: GLMNET ROC ")

# my threshold is 
threshold_telco_glmnet = 0.337

telco_churn_glmnet <- churn_bars_gg(data_df = teleco_test_glmnet, 
                                   threshold_num = threshold_telco_glmnet, 
                                   text_str = "Telco Churn: GLMNET " )

telco_churn_glmnet

telco_churn_density_glmnet <- churn_density(data_df = teleco_test_glmnet, 
                                           threshold_num = threshold_telco_glmnet, 
                                           text_str = "Telco Churn: GLMNET " )

telco_churn_density_glmnet

# bank knn -------------------------------------------------------------
# 
set.seed(123)

# knn tut is here: http://www.sthda.com/english/articles/35-statistical-machine-learning-essentials/142-knn-k-nearest-neighbors-essentials/

# train the model
bank_knn_model <- train(
  Churn ~., data = bank_train, method = "knn",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center", "scale"),
  tuneLength = 20
)

# Plot model accuracy vs different values of k
plot(bank_knn_model)

# Print the best tuning parameter k that
# maximizes model accuracy
# 21
bank_knn_model$bestTune

bank_test_knn <- bank_test

# Add a new column, to hold the predictions on the test data
bank_test_knn$predicted <- bank_knn_model %>% 
  predict( bank_test_knn )


# find the best cut off value
# Compute model accuracy rate 
bank_knn_roc_plot <- pROC::roc( Churn ~ predicted, bank_test_knn )

plot(bank_knn_roc_plot, print.thres=TRUE, main = "Bank Churn: KNN ")

# my threshold is 
threshold_bank_knn = 0.186

bank_churn_knn <- churn_bars_gg(data_df = bank_test_knn, 
                                    threshold_num = threshold_bank_knn, 
                                    text_str = "Bank Churn: KNN " )

bank_churn_knn

bank_churn_density_knn <- churn_density(data_df = bank_test_knn, 
                                            threshold_num = threshold_bank_knn, 
                                            text_str = "Bank Churn: KNN " )

bank_churn_density_knn


# bank gbm --------------------------------------

set.seed(123)

# train the model
bank_gbm_model <- train(
  Churn ~., data = bank_train, method = "gbm",
  trControl = trainControl("cv", number = 10),
  bag.fraction=0.75
)

# Plot model accuracy vs different values of k
plot(bank_gbm_model)

bank_test_gbm <- bank_test

# Add a new column, to hold the predictions on the test data
bank_test_gbm$predicted <- bank_gbm_model %>% 
  predict( bank_test_gbm )

# Compute model accuracy rate 
bank_gbm_roc_plot <- pROC::roc( Churn ~ predicted, bank_test_gbm )

plot(bank_gbm_roc_plot, print.thres=TRUE, main = "Bank Churn: GBM ")

# my threshold is 
threshold_bank_gbm = 0.226

bank_churn_gbm <- churn_bars_gg(data_df = bank_test_gbm, 
                                threshold_num = threshold_bank_gbm, 
                                text_str = "Bank Churn: GBM " )

bank_churn_gbm

telco_churn_density_gbm <- churn_density(data_df = bank_test_gbm, 
                                         threshold_num = threshold_bank_gbm, 
                                         text_str = "Bank Churn: GBM " )

telco_churn_density_gbm

# bank earth  ------------------------------------------------------------

set.seed(123)

# train the model
bank_earth_model <- train(
  Churn ~., data = bank_train, method = "earth",
  trControl = trainControl("cv", number = 10)
)

# Plot model accuracy vs different values of k
plot(bank_earth_model)

bank_test_earth <- bank_test

# Add a new column, to hold the predictions on the test data
bank_test_earth$predicted <- bank_earth_model %>% 
  predict( bank_test_earth )


bank_earth_roc_plot <- pROC::roc( Churn ~ predicted, bank_test_earth )

plot(bank_earth_roc_plot, print.thres=TRUE, main = "Bank Churn: Earth ROC ")

# my threshold is 
threshold_bank_earth = 0.219

bank_churn_earth <- churn_bars_gg(data_df = bank_test_earth, 
                                   threshold_num = threshold_bank_earth, 
                                   text_str = "Bank Churn: Earth " )

bank_churn_earth

bank_churn_density_earth <- churn_density(data_df = bank_test_gbm, 
                                           threshold_num = threshold_bank_earth, 
                                           text_str = "Bank Churn: Earth " )

bank_churn_density_earth

# bank glmnet -----------------------------

set.seed(123)

# train the model
bank_glmnet_model <- train(
  Churn ~., data = bank_train, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center", "scale"),
  tuneGrid=expand.grid(
    .alpha=1,
    .lambda=seq(0, 100, by = 0.1))
)

bank_test_glmnet <- bank_test

# Add a new column, to hold the predictions on the test data
bank_test_glmnet$predicted <- bank_glmnet_model %>% 
  predict( bank_test_glmnet )

# Compute model accuracy rate 
bank_glmnet_roc_plot <- pROC::roc( Churn ~ predicted, bank_test_glmnet )

plot(bank_glmnet_roc_plot, print.thres=TRUE, main = "Bank Churn: glmnet ")

# my threshold is 
threshold_bank_glmnet = 0.267

bank_churn_glmnet <- churn_bars_gg(data_df = bank_test_glmnet, 
                                threshold_num = threshold_bank_glmnet, 
                                text_str = "Bank Churn: glmnet " )

bank_churn_glmnet

telco_churn_density_glmnet <- churn_density(data_df = bank_test_glmnet, 
                                         threshold_num = threshold_bank_glmnet, 
                                         text_str = "Bank Churn: glmnet " )

telco_churn_density_glmnet


# now join all bank predictions to compare ---------------------------

# duplicate the bank df
data_bank_preds <- bank_data

# apply each of our models
data_bank_preds$predicted_knn <- bank_knn_model %>% 
  predict( data_bank_preds )

data_bank_preds$predicted_gbm <- bank_gbm_model %>% 
  predict( data_bank_preds )

data_bank_preds$predicted_mars <- bank_earth_model %>% 
  predict( data_bank_preds )

data_bank_preds$predicted_glmnet <- bank_glmnet_model %>% 
  predict( data_bank_preds )

# clv calculation -------------------------------------------

# Customer Lifetime Value (Benoit and Poel, 2009). CLV is viewed as the present value of the future cash flows associated with a customer (Pfeifer et al, 2005). Knowing the CLV of individual customers enables the decision maker to improve the customer segmentation and marketing resource allocation efforts [-@kahreh2014analyzing]


# what is the churn rate [1] 0.2037
data_bank_churn_rate <- sum(data_bank_preds$Churn==1)/nrow(data_bank_preds)
bank_charge_per_anum <- 250
interest_rate <- .04
cost_of_keeping_customer <- 250

# simplified clv formula to calculate a value to each customer
data_bank_preds <- data_bank_preds %>% 
  # customer balance by interest rate charged by bank of 4%
  # charges for each customer per anum
  mutate(clv = ( Balance * interest_rate ) + (bank_charge_per_anum * Tenure))

# bank customer prediction --------------------------------------
# 
# average clv of a customer
clv_average <- mean(data_bank_preds$clv)
# 

#' This function returns a dataframe that contains the cost of each customer
#' predicted to churn, but did not. We can then add a cost to each customer
#' and rate each algorithm by cost.
#'
#' @param df  a data frame, specifically one called data_bank_preds
#' @param churn value to match Churn column 1 or 0
#' @param pred value to match the predicted churn column, either 1 or 0
#'
#' @return a dataframe, comparing each algorithm by cost per customer
#' @export
#'
#' @examples
#' 
cost_matrix <- function(df = data_bank_preds, 
                        churn = 1, 
                        pred = 1) {
  
  # first filter everything
  df <- df %>% 
    filter( Churn == churn)
  
  df_1 <- df %>% 
    # i'm casting prediction to 1 or 0 here
    mutate(predicted_knn = predicted_knn >= threshold_bank_knn ) %>% 
    filter(predicted_knn == pred ) %>% 
    summarise(customers = n(),
              type = "KNN") %>% 
    melt()
  
  df_2 <- df %>% 
    # i'm casting prediction to 1 or 0 here
    mutate(predicted_gbm = predicted_gbm >= threshold_bank_gbm ) %>%
    filter(predicted_gbm == pred ) %>% 
    summarise(customers = n(),
              type = "GBM") %>% 
    melt()
  
  df_3 <- df %>% 
    # i'm casting prediction to 1 or 0 here
    mutate(predicted_mars = predicted_mars >= threshold_bank_earth ) %>%
    filter(predicted_mars == pred ) %>%  
    summarise(customers = n(),
              type = "MARS") %>% 
    melt()
  
  df_4 <- df %>% 
    # i'm casting prediction to 1 or 0 here
    mutate(predicted_glmnet = predicted_glmnet >= threshold_bank_glmnet ) %>%
    filter(predicted_glmnet == pred ) %>% 
    summarise(customers = n(),
              type = "GLMNET") %>% 
    melt()
  
  df_ret <- df_1 %>% 
    full_join( df_2 ) %>% 
    full_join( df_3 ) %>% 
    full_join( df_4 )
    
    return( df_ret )
  
}

# cost of incorrectly predicted customers: 
# predicted not to churn but did not, 
# assuming the retention initiative
# costs the bank 250 per customer
churned_F_predicted_T <- cost_matrix(churn = 0, pred = 1)

# cost of incorrectly predicted churners:
# not predicted to churn but did,
# assuming loss of a mean CLV per churner
churned_T_predicted_F <- cost_matrix(churn = 1, pred = 0)

# cost of incorrectly predicted churners:
# not predicted to churn but did,
# assuming loss of a mean CLV per churner
churned_T_predicted_T <- cost_matrix(churn = 1, pred = 1)


ft <- churned_F_predicted_T %>% 
  ggplot(aes(type, value, fill = type)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 2500)) +  
  geom_text(label = churned_F_predicted_T$value) +
  labs(title = "Bank Churn: customers \nincorrectly predicted to churn",
       x ="", y = "Customers")

tf <- churned_T_predicted_F %>% 
  ggplot(aes(type, value, fill = type)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 2500)) +
  geom_text(label = churned_T_predicted_F$value) +
  labs(title = "Bank Churn: customers not \npredicted to churn, but did",
       x ="Algorithm", y = "")

tt <- churned_T_predicted_T %>% 
  ggplot(aes(type, value, fill = type)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 2500)) +
  geom_text(label = churned_T_predicted_T$value) +
  labs(title = "Churn: customers predicted to\n churn and did churn",
       x ="", y = "")

compar_cust <- grid.arrange(ft, tf, tt, ncol = 3)

compar_cust

# bank cost prediction ------------------------------------------
# 
# # Customer Lifetime Value (Benoit and Poel, 2009). CLV is viewed as the present value of the future cash flows associated with a customer (Pfeifer et al, 2005). Knowing the CLV of individual customers enables the decision maker to improve the customer segmentation and marketing resource allocation efforts [-@kahreh2014analyzing]


# what is the churn rate [1] 0.2037
data_bank_churn_rate <- sum(data_bank_preds$Churn==1)/nrow(data_bank_preds)
bank_charge_per_anum <- 200
interest_rate <- .04
cost_of_keeping_customer <- 250

# simplified clv formula to calculate a value to each customer
data_bank_preds <- data_bank_preds %>% 
  # customer balance by interest rate charged by bank of 4%
  # charges for each customer per anum
  mutate(clv = ( Balance * interest_rate ) + (bank_charge_per_anum * Tenure))

# bank customer prediction --------------------------------------
# 
# average clv of a customer
clv_average <- round(mean(data_bank_preds$clv))

# 
# in this case a customer predicted to churn, 
# would be offered a discount, or free gift
# to the value of 250
churned_F_predicted_T_cost <- churned_F_predicted_T %>% 
  mutate(cost_of_keeping = value * cost_of_keeping_customer)

# in this case a customer who churned, but was not predicted would
# cost clv lost, this is money lost to the bank
churned_T_predicted_F_cost <-  churned_T_predicted_F %>% 
  mutate(clv_lost = value * clv_average)


churned_T_predicted_T_saving <- churned_T_predicted_T %>% 
  mutate(clv_saved = value * clv_average)

# plot that

ft <- churned_F_predicted_T_cost %>% 
  ggplot(aes(type, cost_of_keeping, fill = type)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(limits = c(0, max(churned_T_predicted_T_saving$clv_saved))) + 
  geom_text(label = churned_F_predicted_T_cost$cost_of_keeping) +
  labs(title = "Bank Churn: cost of \nincorrectly predicted to churn",
       x ="", y = "Cost in Euro")

tf <- churned_T_predicted_F_cost %>% 
  ggplot(aes(type, clv_lost, fill = type)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(limits = c(0, max(churned_T_predicted_T_saving$clv_saved))) + 
  geom_text(label = churned_T_predicted_F_cost$clv_lost) +
  labs(title = "Bank Churn: costs of not \npredicted to churn, but did",
       x ="Algorithm", y = "")

tt <- churned_T_predicted_T_saving %>% 
  ggplot(aes(type, clv_saved, fill = type)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(limits = c(0, max(churned_T_predicted_T_saving$clv_saved))) +
  geom_text(label = churned_T_predicted_T_saving$clv_saved) +
  labs(title = "Churn: savings of predicted to\n churn and did churn",
       x ="", y = "")

compar_cost <- grid.arrange(ft, tf, tt, ncol = 3)

compar_cost

# type clv_saved
# 1    KNN   6052380
# 2    GBM   6357030
# 3   MARS   6665742
# 4 GLMNET   5260290
bank_cost <- churned_T_predicted_T_saving[c(1,4)]

bank_cost$clv_lost <-  churned_T_predicted_F_cost$clv_lost

bank_cost$cost_of_keeping <- churned_F_predicted_T_cost$cost_of_keeping


bank_cost <- bank_cost %>% 
  mutate(potential_all_up_saving = clv_saved - clv_lost - cost_of_keeping)


# even though mars is not the most effective, it could potentially save a bank most money. 
bank_cost %>% 
  ggplot(aes(type, potential_all_up_saving, fill = type)) +
  geom_col() +
  geom_text(label = format(bank_cost$potential_all_up_saving, big.mark = ","))