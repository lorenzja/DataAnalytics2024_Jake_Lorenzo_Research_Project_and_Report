# Load the necessary library
install.packages("zoo")  # If not already installed
install.packages("randomForest")  # Install package if not installed
install.packages("tseries")
install.packages("lmtest")
install.packages("e1071")
install.packages("ggfortify")
install.packages("class")
install.packages("FNN")
install.packages("xgboost")
install.packages("keras")
install.packages("tensorflow")
library(keras)
library(tensorflow)
library(FNN)
library(zoo)
library(ggplot2)
library(dplyr)
library(tseries)
library(lmtest)
library(randomForest)
library(e1071)
library(ggfortify)
library(class)
library(xgboost)
library(readr)
library(readr)

###########################################################################################################################################
###########################################Ethereum INITIAL PREPROCESSING##################################################################
###########################################################################################################################################

ETH_data <- read.csv("C:/Users/Jake Lorenzo/Desktop/Data Analytics/Research Project/Ethereum Historical Dataset.csv")

# Convert DATE column to Date format
ETH_data$DATE <- as.Date(ETH_data$DATE, format = "%Y-%m-%d")

# Convert the second column to numeric
ETH_data[, 2] <- as.numeric(ETH_data[, 2])

# Change column name from "DATE" to "Date"
colnames(ETH_data)[colnames(ETH_data) == "DATE"] <- "Date"

colnames(ETH_data)[2] <- "ETH"

###########################################################################################################################################
###########################################BITCOIN INITIAL PREPROCESSING###################################################################
###########################################################################################################################################

BTC_data <- read.csv("C:/Users/Jake Lorenzo/Desktop/Data Analytics/Research Project/Bitcoin Historical Dataset.csv")

# Convert DATE column to Date format
BTC_data$DATE <- as.Date(BTC_data$DATE, format = "%Y-%m-%d")

# Convert the second column to numeric
BTC_data[, 2] <- as.numeric(BTC_data[, 2])

# Change column name from "DATE" to "Date"
colnames(BTC_data)[colnames(BTC_data) == "DATE"] <- "Date"

colnames(BTC_data)[2] <- "BTC"


###########################################################################################################################################
###########################################S&P 500 INITIAL PREPROCESSING###################################################################
###########################################################################################################################################
# S&P 500 HISTORICAL DATA
SnP_data <- read.csv("C:/Users/Jake Lorenzo/Desktop/Data Analytics/Research Project/S&P 500 Historical Dataset.csv")

# Convert DATE column to Date format
SnP_data$DATE <- as.Date(SnP_data$DATE, format = "%Y-%m-%d")

# Convert the second column to numeric
SnP_data[, 2] <- as.numeric(SnP_data[, 2])

# Change column name from "DATE" to "Date"
colnames(SnP_data)[colnames(SnP_data) == "DATE"] <- "Date"

# S&P 500 VOLATILITY INDEX
SnP_volatility <- read.csv("C:/Users/Jake Lorenzo/Desktop/Data Analytics/Research Project/S&P 500 Volatility Index.csv")


###########################################################################################################################################
###########################################RUSSELL 3000 INITIAL PREPROCESSING##############################################################
###########################################################################################################################################
# RUSSELL 3000 HISTORICAL DATA - Covers about 96% of the Investable US Stock Market
R3000_data <- read.csv("C:/Users/Jake Lorenzo/Desktop/Data Analytics/Research Project/Russell 3000 Historical Dataset.csv")

# Convert DATE column to Date format with the correct format
R3000_data$Date <- as.Date(R3000_data$Date, format = "%m/%d/%Y")

colnames(R3000_data)[2] <- "R3000"

R3000_data <- R3000_data[, -c(3:5)]

###########################################################################################################################################
###########################################CRSP INITIAL PREPROCESSING#############################################################
###########################################################################################################################################
# CRSP US Total Market Index (includes all U.S. publicly traded stocks)
CRSP_data <- read.csv("C:/Users/Jake Lorenzo/Desktop/Data Analytics/Research Project/CRSP US Total Market Historical Dataset.csv")

# Convert DATE column to Date format with the correct format
CRSP_data$Date <- as.Date(CRSP_data$Date, format = "%m/%d/%Y")

# Remove commas from the second column and convert to numeric
CRSP_data[, 2] <- as.numeric(gsub(",", "", CRSP_data[, 2]))

colnames(CRSP_data)[2] <- "CRSP"

CRSP_data <- CRSP_data[, -c(3:7)]

###########################################################################################################################################
###########################################################################################################################################
###########################################################################################################################################
##Data import from FRED
##Federal Funds Effective Rate
DFF <- read_csv("DFF.csv")

colnames(DFF)[colnames(DFF) == "DATE"] <- "Date"

Gold_vol <- read_csv("GVZCLS.csv")

# Rename columns
colnames(Gold_vol)[colnames(Gold_vol) == "DATE"] <- "Date"
colnames(Gold_vol)[colnames(Gold_vol) == "GVZCLS"] <- "Gold_VIX"


###########################################################################################################################################
###########################################################################################################################################
###########################################################################################################################################

#Aligning the datasets by Date

#BTC_data: 2014-12-01 to 2024-11-10
#CRSP_data: 2012-01-12 to 2024-11-12
#ETH_data: 2016-05-18 to 2024-11-10
#R3000_data: 2014-11-12 to 2024-11-11
#SnP_data: 2014-11-10 to 2024-11-08


# Define the date threshold range
start_date <- as.Date("2016-05-18")
end_date <- as.Date("2024-11-08")
complete_dates <- seq.Date(from = start_date, to = end_date, by = "day")

# Function to fill missing rows for datasets
fill_missing_dates <- function(data, date_column, value_column) {
  # Create a data frame with the complete sequence of dates
  complete_data <- data.frame(Date = complete_dates)
  
  # Merge the complete dates with the original data
  merged_data <- merge(complete_data, data, by = "Date", all.x = TRUE)
  
  # Carry forward the last available value for missing dates
  merged_data[[value_column]] <- na.locf(merged_data[[value_column]])
  
  return(merged_data)
}

# Fill missing dates and values for each dataset
SnP_data_filled <- fill_missing_dates(SnP_data, "Date", "SP500")
R3000_data_filled <- fill_missing_dates(R3000_data, "Date", "R3000")
CRSP_data_filled <- fill_missing_dates(CRSP_data, "Date", "CRSP")
Gold_vol_filled <- fill_missing_dates(Gold_vol, "Date", "Gold_VIX")

SnP_data <- SnP_data_filled
R3000_data <- R3000_data_filled
CRSP_data <- CRSP_data_filled
Gold_vol <- Gold_vol_filled

# Filter the dataset to keep rows only before or on '2024-11-08'
ETH_data <- ETH_data[ETH_data$Date <= as.Date("2024-11-08"), ]
BTC_data <- BTC_data[BTC_data$Date >= as.Date("2016-05-18"), ]
BTC_data <- BTC_data[BTC_data$Date <= as.Date("2024-11-08"), ]


##Initial Plots
# Plot for Ethereum (ETH)
ggplot(ETH_data, aes(x = Date, y = ETH)) +
  geom_line() +
  ggtitle("Ethereum Time Series") +
  xlab("Date") + ylab("Ethereum Price") +
  theme_minimal()

# Plot for Bitcoin (BTC)
ggplot(BTC_data, aes(x = Date, y = BTC)) +
  geom_line() +
  ggtitle("Bitcoin Time Series") +
  xlab("Date") + ylab("Bitcoin Price") +
  theme_minimal()

# Plot for S&P 500 (SnP)
ggplot(SnP_data, aes(x = Date, y = SnP_data[, 2])) + # Replace 'SnP_data[, 2]' with actual column name for S&P 500
  geom_line() +
  ggtitle("S&P 500 Time Series") +
  xlab("Date") + ylab("S&P 500 Index Value") +
  theme_minimal()

# Plot for Russell 3000 (R3000)
ggplot(R3000_data, aes(x = Date, y = R3000)) +
  geom_line() +
  ggtitle("Russell 3000 Time Series") +
  xlab("Date") + ylab("Russell 3000 Index Value") +
  theme_minimal()

# Plot for CRSP US Total Market Index (CRSP)
ggplot(CRSP_data, aes(x = Date, y = CRSP)) +
  geom_line() +
  ggtitle("CRSP US Total Market Index Time Series") +
  xlab("Date") + ylab("CRSP Index Value") +
  theme_minimal()

# Plot for Gold_vol (Gold_VIX)
ggplot(Gold_vol, aes(x = Date, y = Gold_VIX)) +
  geom_line() +
  ggtitle("Gold Volatility Index Time Series") +
  xlab("Date") + ylab("VIX Price") +
  theme_minimal()

# Merge the BTC and ETH datasets by Date
merged_data <- merge(BTC_data, ETH_data, by = "Date", suffixes = c("_BTC", "_ETH"))

# Plot Bitcoin and Ethereum on the same graph
ggplot(merged_data) +
  geom_line(aes(x = Date, y = BTC, color = "Bitcoin")) +
  geom_line(aes(x = Date, y = ETH, color = "Ethereum")) +
  ggtitle("Bitcoin vs Ethereum Time Series") +
  xlab("Date") + ylab("Price") +
  scale_color_manual(values = c("Bitcoin" = "blue", "Ethereum" = "green")) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Merge the BTC, ETH, and SnP datasets by Date
merged_data <- merge(BTC_data, ETH_data, by = "Date", suffixes = c("_BTC", "_ETH"))
merged_data <- merge(merged_data, SnP_data, by = "Date", suffixes = c("_ETH", "_SnP"))

# Plot Bitcoin, Ethereum, and S&P 500 on the same graph
ggplot(merged_data) +
  geom_line(aes(x = Date, y = BTC, color = "Bitcoin")) +
  geom_line(aes(x = Date, y = ETH, color = "Ethereum")) +
  geom_line(aes(x = Date, y = SP500, color = "S&P 500")) +
  ggtitle("Bitcoin, Ethereum, and S&P 500 Time Series") +
  xlab("Date") + ylab("Price / Index Value") +
  scale_color_manual(values = c("Bitcoin" = "blue", "Ethereum" = "green", "S&P 500" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Merge the BTC, ETH, SnP, R3000, and CRSP datasets by Date
merged_data <- merge(merged_data, R3000_data, by = "Date", suffixes = c("_SnP", "_R3000"))
merged_data <- merge(merged_data, CRSP_data, by = "Date", suffixes = c("_R3000", "_CRSP"))
merged_data <- merge(merged_data, DFF, by = "Date", all.x = TRUE)
merged_data <- merge(merged_data, Gold_vol, by = "Date", all.x = TRUE)

# Plot Bitcoin, Ethereum, S&P 500, Russell 3000, and CRSP on the same graph
ggplot(merged_data) +
  geom_line(aes(x = Date, y = BTC, color = "Bitcoin")) +
  geom_line(aes(x = Date, y = ETH, color = "Ethereum")) +
  geom_line(aes(x = Date, y = SP500, color = "S&P 500")) +
  geom_line(aes(x = Date, y = R3000, color = "Russell 3000")) +
  geom_line(aes(x = Date, y = CRSP, color = "CRSP US Total Market")) +
  ggtitle("Bitcoin, Ethereum, S&P 500, Russell 3000, and CRSP Time Series") +
  xlab("Date") + ylab("Price / Index Value") +
  scale_color_manual(values = c("Bitcoin" = "blue", 
                                "Ethereum" = "green", 
                                "S&P 500" = "red", 
                                "Russell 3000" = "purple", 
                                "CRSP US Total Market" = "orange")) +
  theme_minimal() +
  theme(legend.title = element_blank())


#######################################################################################################
# Replace '.' with NA in the Gold_VIX column
merged_data$Gold_VIX[merged_data$Gold_VIX == "."] <- NA

# Verify if there are any remaining '.' or missing values
sum(merged_data$Gold_VIX == ".", na.rm = TRUE)  # Check for remaining '.'
sum(is.na(merged_data$Gold_VIX))  # Check for NA values

# Convert Gold_VIX column to numeric
merged_data$Gold_VIX <- as.numeric(merged_data$Gold_VIX)

#NA removal
merged_data <- na.omit(merged_data)

#Linear model (Bitcoin and gold)
btc_goldlm_model <- lm(BTC ~ Gold_VIX, data = merged_data)
summary(btc_goldlm_model)

#Multiple R-squared:  0.03537,	Adjusted R-squared:  0.03505

##Linear model (SP500 and gold)
snp_goldlm_model <- lm(SP500 ~ Gold_VIX, data = merged_data)
summary(snp_goldlm_model)

#Multiple R-squared:  0.02959,	Adjusted R-squared:  0.02927

##Linear model (Bitcoin with SP500 and gold and dff)
BTC_gspdfflm_model <- lm(BTC ~ SP500 + Gold_VIX + DFF, data = merged_data)
summary(BTC_gspdfflm_model)

#Multiple R-squared:  0.8792,	Adjusted R-squared:  0.879 

##Linear model (Bitcoin with SP500 and gold and dff)
BTC_sp_dfflm_model <- lm(BTC ~ SP500 + R3000 + CRSP + DFF, data = merged_data)
summary(BTC_sp_dfflm_model)
#Multiple R-squared:  0.8883,	Adjusted R-squared:  0.8882


#Linear model (Bitcoin)
btc_lm_model <- lm(BTC ~ SP500 + R3000 + CRSP, data = merged_data)
summary(btc_lm_model)

#Multiple R-squared:  0.8874,	Adjusted R-squared:  0.8873 

#Linear model (Bitcoin with DFF)
btc_lm_modeldff <- lm(BTC ~ SP500 + R3000 + CRSP + DFF, data = merged_data)
#Multiple R-squared:  0.8884,	Adjusted R-squared:  0.8883 
summary(btc_lm_modeldff)

#Linear model (Ethereum)
eth_lm_model <- lm(ETH ~ SP500 + R3000 + CRSP, data = merged_data)
summary(eth_lm_model)

#Multiple R-squared:  0.8232,	Adjusted R-squared:  0.823 

#Linear model (Ethereum with DFF)
eth_lm_modeldff <- lm(ETH ~ SP500 + R3000 + CRSP + DFF, data = merged_data)
summary(eth_lm_modeldff)

#Multiple R-squared:  0.8293,	Adjusted R-squared:  0.8291

#Linear model (Bitcoin2)
btc_lm_model2 <- lm(BTC ~ ETH + SP500 + R3000 + CRSP, data = merged_data)
summary(btc_lm_model2)

#Multiple R-squared:  0.9237,	Adjusted R-squared:  0.9236 

#Linear model (Bitcoin200)
btc_lm_model200 <- lm(BTC ~ ETH + SP500 + R3000 + CRSP + Gold_VIX, data = merged_data)
summary(btc_lm_model200)

#Multiple R-squared:  0.9248,	Adjusted R-squared:  0.9247 

#Linear model (Bitcoin2000)
btc_lm_model2000 <- lm(BTC ~ ETH + SP500 + R3000 + CRSP + Gold_VIX + DFF, data = merged_data)
summary(btc_lm_model2000)

#Multiple R-squared:  0.926,	Adjusted R-squared:  0.9259 

#Linear model (Ethereum2)
eth_lm_model2 <- lm(ETH ~ BTC + SP500 + R3000 + CRSP, data = merged_data)
summary(eth_lm_model2)

#Multiple R-squared:  0.8802,	Adjusted R-squared:   0.88 

#Linear model (Bitcoin with dff and eth)
btc_lm_model22 <- lm(BTC ~ ETH + SP500 + R3000 + CRSP + DFF, data = merged_data)
summary(btc_lm_model22)

#Multiple R-squared:  0.9237,	Adjusted R-squared:  0.9236

#Linear model (Ethereum with btc and dff)
eth_lm_model23 <- lm(ETH ~ BTC + SP500 + R3000 + CRSP + DFF, data = merged_data)
summary(eth_lm_model23)

#Multiple R-squared:  0.8833,	Adjusted R-squared:  0.8831

#Correlation Analysis
cor(merged_data[, c("BTC", "ETH", "SP500", "R3000", "CRSP")])

            #BTC       ETH     SP500     R3000      CRSP
#BTC   1.0000000 0.9344353 0.9259464 0.9334471 0.9341360
#ETH   0.9344353 1.0000000 0.8866925 0.8951407 0.8959537
#SP500 0.9259464 0.8866925 1.0000000 0.9985448 0.9983198
#R3000 0.9334471 0.8951407 0.9985448 1.0000000 0.9999807
#CRSP  0.9341360 0.8959537 0.9983198 0.9999807 1.0000000

#Correlation Analysis with DFF
cor(merged_data[, c("BTC", "ETH", "SP500", "R3000", "CRSP", "DFF")])

           # BTC       ETH     SP500     R3000      CRSP       DFF
#BTC   1.0000000 0.9344353 0.9259464 0.9334471 0.9341360 0.3765725
#ETH   0.9344353 1.0000000 0.8866925 0.8951407 0.8959537 0.3170681
#SP500 0.9259464 0.8866925 1.0000000 0.9985448 0.9983198 0.5385608
#R3000 0.9334471 0.8951407 0.9985448 1.0000000 0.9999807 0.5062017
#CRSP  0.9341360 0.8959537 0.9983198 0.9999807 1.0000000 0.5032332
#DFF   0.3765725 0.3170681 0.5385608 0.5062017 0.5032332 1.0000000

#Stronger correlation with stock indices (S&P 500, Russell 3000, CRSP) indicates these markets move more in sync with changes in the federal funds rate.
#Weaker correlation with cryptocurrencies (BTC, ETH) suggests they are less directly influenced by interest rate changes.
#Overall moderate positive correlation shows that as DFF increases, both equities and crypto may rise, but stocks respond more consistently.

#Correlation Analysis with DFF and gold VIX
cor(merged_data[, c("BTC", "ETH", "SP500", "R3000", "CRSP", "DFF", "Gold_VIX")])


#Granger Casuality Test (Testing to see if stock market trends influence crypto prices)
grangertest(BTC ~ SP500, order = 1, data = merged_data)

#The p-value (0.01124) is below 0.05, indicating that past S&P 500 values have predictive power over 
#Bitcoin prices at a 5% significance level. This reflects a time-lagged predictive relationship, 
#not causation.

grangertest(ETH ~ SP500, order = 1, data = merged_data)

#The p-value (0.001593) is below 0.01, indicating strong evidence that past S&P 500 values 
#have predictive power over Ethereum prices at a 1% significance level. 
#This reflects a time-lagged predictive relationship, not causation.

#Granger Casuality Test (Testing to see if stock market trends influence crypto prices)
grangertest(SP500 ~ DFF, order = 1, data = merged_data)
#not significant

#Granger Casuality Test (Testing to see if stock market trends influence crypto prices)
grangertest(BTC ~ DFF, order = 1, data = merged_data)
#not significant

grangertest(BTC ~ Gold_VIX, order = 1, data = merged_data)
#Inconclusive

grangertest(SP500 ~ Gold_VIX, order = 1, data = merged_data)
#Gold Volatility might influence SP500 but not a strong relationship

#Machine Learning Methods (Random Forrest, K-means clustering, etc.)
#Random Forrest
set.seed(123)  # For reproducibility
sample_index <- sample(1:nrow(merged_data), size = 0.7 * nrow(merged_data))
train_data <- merged_data[sample_index, ]
test_data <- merged_data[-sample_index, ]

# Train Random Forest model - btc
rf_model <- randomForest(BTC ~ ETH + SP500 + R3000 + CRSP + DFF + Gold_VIX, data = train_data, ntree = 500, importance = TRUE)


#MAE: 360.6366
#MSE: 509703.8
#RMSE: 713.9354

# Check model summary
print(rf_model)
summary(rf_model)
predicted_values <- predict(rf_model, train_data)
actual_values <- train_data$BTC
residuals <- actual_values - predicted_values
mse <- mean(residuals^2)
mae <- mean(abs(residuals))

print(mae)
print(mse)
rmse <- sqrt(mse)
print(rmse)

cat("MAE:", mae, "\nMSE:", mse, "\nRMSE:", rmse, "\n")

#MAE: 790.8611 
#MSE: 1977636 
#RMSE: 1406.284 

plot_data <- data.frame(
  Actual = test_data$BTC,
  Predicted = predict(rf_model, test_data)
)

# Create the plot
ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Actual vs Predicted BTC Prices (Random Forest Model)",
    x = "Actual BTC Prices",
    y = "Predicted BTC Prices"
  ) +
  theme_minimal()

#% Var explained(without DFF): 97.64
#% Var explained(with DFF): 98.45
#% Var explained (with DFF and Gold(VIX): 98.71

rf_model2 <- randomForest(BTC ~ Gold_VIX, data = train_data, ntree = 500, importance = TRUE)

# Check model summary
print(rf_model2)
summary(rf_model2)
predicted_values <- predict(rf_model2, train_data)
actual_values <- train_data$BTC
residuals <- actual_values - predicted_values
mse <- mean(residuals^2)
mae <- mean(abs(residuals))
rmse <- sqrt(mse)
print(rmse)

cat("MAE:", mae, "\nMSE:", mse, "\nRMSE:", rmse, "\n")

#MAE: 8877.477 
#MSE: 166055381 
#RMSE: 12886.25 




# Train Random Forest model - eth
rf_eth_model <- randomForest(ETH ~ SP500 + R3000 + CRSP, data = train_data, ntree = 500, importance = TRUE)

# Check model summary
print(rf_eth_model)
summary(rf_eth_model)
predicted_values <- predict(rf_eth_model, train_data)
actual_values <- train_data$BTC
residuals <- actual_values - predicted_values
mse <- mean(residuals^2)
mae <- mean(abs(residuals))
print(mae)

#% Var explained: 96.43

#KMeans Clustering was not effective due to lack of features in the dataset

########## KNN Model #################

# Set seed for reproducibility
set.seed(123)

# Normalize the data
normalize <- function(x) { (x - min(x)) / (max(x) - min(x)) }
normalized_data <- merged_data
normalized_data[, 3:8] <- as.data.frame(lapply(merged_data[, 3:8], normalize))

# Split the normalized data into training and testing datasets (70% train, 30% test)
sample_indices <- sample(nrow(normalized_data), size = floor(0.7 * nrow(normalized_data)))
normalized_train <- normalized_data[sample_indices, ]
normalized_test <- normalized_data[-sample_indices, ]

# Determine k (e.g., square root of training size, rounded to the nearest integer)
k <- 3

# Perform KNN regression
KNNpred <- knn.reg(
  train = normalized_train[, 2:8], 
  test = normalized_test[, 2:8], 
  y = normalized_train$BTC, 
  k = k
)$pred

# Calculate error metrics for the test data
err <- KNNpred - normalized_test$BTC

# Calculate MAE (Mean Absolute Error)
mean_abs_err <- mean(abs(err))

# Calculate MSE (Mean Squared Error)
mean_sq_err <- mean(err^2)

# Calculate RMSE (Root Mean Squared Error)
root_mean_sq_err <- sqrt(mean_sq_err)

# Print the results
cat("MAE:", mean_abs_err, "\nMSE:", mean_sq_err, "\nRMSE:", root_mean_sq_err, "\n")

#MAE: 19.72659 
#MSE: 1496.12 
#RMSE: 38.67971  

#addition of Gold_VIX did not affect output

library(ggplot2)

# Create a data frame for the test set predictions and actual values
plot_data <- data.frame(
  Date = normalized_test$Date,
  Actual = normalized_test$BTC,
  Predicted = KNNpred
)

# Plot actual vs predicted BTC prices
ggplot(plot_data, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"), linetype = "dashed", size = 1) +
  ggtitle("Actual vs Predicted BTC Prices (KNN)") +
  xlab("Date") +
  ylab("BTC Price") +
  scale_color_manual(
    values = c("Actual" = "blue", "Predicted" = "red"),
    name = "Legend"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "top"
  )

#######Neural Network Set-up
install_keras()

# Separate features and target variable for training and testing
x_train <- as.matrix(normalized_train[, c("SP500", "R3000", "CRSP", "DFF")])  # Features
y_train <- normalized_train$BTC  # Target variable

x_test <- as.matrix(normalized_test[, c("SP500", "R3000", "CRSP", "DFF")])  # Features
y_test <- normalized_test$BTC  # Target variable

# Define the FNN model
model_fnn <- keras_model_sequential() %>%
  layer_dense(units = 128, activation = 'relu', input_shape = ncol(x_train)) %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dense(units = 1)

# Compile the FNN model
model_fnn %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(),
  metrics = c('mean_absolute_error')
)

# Train the FNN model
history_fnn <- model_fnn %>% fit(
  x_train, y_train,
  epochs = 50,
  batch_size = 32,
  validation_split = 0.2
)

# Evaluate the FNN model
model_fnn %>% evaluate(x_test, y_test)

# Predict on the test data using the FNN model
fnn_predictions <- model_fnn %>% predict(x_test)

# Convert predictions to a numeric vector if needed
fnn_predictions <- as.numeric(fnn_predictions)

# Calculate residuals
fnn_residuals <- y_test - fnn_predictions

# Calculate Mean Absolute Error (MAE)
fnn_mae <- mean(abs(fnn_residuals))

# Calculate Mean Squared Error (MSE)
fnn_mse <- mean(fnn_residuals^2)

# Calculate Root Mean Squared Error (RMSE)
fnn_rmse <- sqrt(fnn_mse)

# Print the results
cat("FNN Model Error Metrics:\n")
cat("MAE:", fnn_mae, "\n")
cat("MSE:", fnn_mse, "\n")
cat("RMSE:", fnn_rmse, "\n")

#Feedforward Neural Network (FNN)
#MAE: 9042.492 
#MSE: 109509399 
#RMSE: 10464.67

# Reshape data for RNN (samples, timesteps, features)
x_train_rnn <- array_reshape(x_train, dim = c(nrow(x_train), 1, ncol(x_train)))
x_test_rnn <- array_reshape(x_test, dim = c(nrow(x_test), 1, ncol(x_test)))

# Define the RNN model
model_rnn <- keras_model_sequential() %>%
  layer_simple_rnn(units = 64, input_shape = c(1, ncol(x_train))) %>%
  layer_dense(units = 1)

# Compile and train the RNN model
model_rnn %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(),
  metrics = c('mean_absolute_error')
)

history_rnn <- model_rnn %>% fit(
  x_train_rnn, y_train,
  epochs = 50,
  batch_size = 32,
  validation_split = 0.2
)

# Reshape x_test to match the input shape expected by the model
x_test_rnn <- array_reshape(x_test, dim = c(nrow(x_test), 1, ncol(x_test)))

# Predict on the reshaped test data
rnn_predictions <- model_rnn %>% predict(x_test_rnn)


# Convert predictions to a numeric vector if necessary
rnn_predictions <- as.numeric(rnn_predictions)

# Calculate residuals
rnn_residuals <- y_test - rnn_predictions

# Calculate Mean Absolute Error (MAE)
rnn_mae <- mean(abs(rnn_residuals))

# Calculate Mean Squared Error (MSE)
rnn_mse <- mean(rnn_residuals^2)

# Calculate Root Mean Squared Error (RMSE)
rnn_rmse <- sqrt(rnn_mse)

# Print the results
cat("RNN Model Error Metrics:\n")
cat("MAE:", rnn_mae, "\n")
cat("MSE:", rnn_mse, "\n")
cat("RMSE:", rnn_rmse, "\n")

#Recurrent Neural Network (RNN) Model Error Metrics:
#MAE: 22196.95 
#MSE: 915927505 
#RMSE: 30264.29 

# Reshape data for LSTM
x_train <- array_reshape(as.matrix(train_data[, -1]), dim = c(nrow(train_data), 1, ncol(train_data) - 1))
x_test <- array_reshape(as.matrix(test_data[, -1]), dim = c(nrow(test_data), 1, ncol(test_data) - 1))

# Define the LSTM model
model_lstm <- keras_model_sequential() %>%
  layer_lstm(units = 64, input_shape = c(1, ncol(train_data) - 1)) %>%
  layer_dense(units = 1)

# Compile and train the model
model_lstm %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(),
  metrics = c('mean_absolute_error')
)

history <- model_lstm %>% fit(
  x_train, train_data$BTC,
  epochs = 50,
  batch_size = 32,
  validation_split = 0.2
)

# Evaluate the model
model_lstm %>% evaluate(x_test, test_data$BTC)

# Predict on the test data
lstm_predictions <- model_lstm %>% predict(x_test)

# Convert predictions to a numeric vector if necessary
lstm_predictions <- as.numeric(lstm_predictions)

# Calculate residuals
lstm_residuals <- test_data$BTC - lstm_predictions

# Calculate Mean Absolute Error (MAE)
lstm_mae <- mean(abs(lstm_residuals))

# Calculate Mean Squared Error (MSE)
lstm_mse <- mean(lstm_residuals^2)

# Calculate Root Mean Squared Error (RMSE)
lstm_rmse <- sqrt(lstm_mse)

# Print the results
cat("LSTM Model Error Metrics:\n")
cat("MAE:", lstm_mae, "\n")
cat("MSE:", lstm_mse, "\n")
cat("RMSE:", lstm_rmse, "\n")

#LSTM Model Error Metrics:
#MAE: 22326.49 
#MSE: 921769924 
#RMSE: 30360.66 

# Define the GRU model
model_gru <- keras_model_sequential() %>%
  layer_gru(units = 64, input_shape = c(1, ncol(train_data) - 1)) %>%
  layer_dense(units = 1)

# Compile and train the model
model_gru %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(),
  metrics = c('mean_absolute_error')
)

history <- model_gru %>% fit(
  x_train, train_data$BTC,
  epochs = 50,
  batch_size = 32,
  validation_split = 0.2
)

# Evaluate the model
model_gru %>% evaluate(x_test, test_data$BTC)


# Predict on the test data
gru_predictions <- model_gru %>% predict(x_test)

# Convert predictions to a numeric vector if necessary
gru_predictions <- as.numeric(gru_predictions)

# Calculate residuals
gru_residuals <- test_data$BTC - gru_predictions

# Calculate Mean Absolute Error (MAE)
gru_mae <- mean(abs(gru_residuals))

# Calculate Mean Squared Error (MSE)
gru_mse <- mean(gru_residuals^2)

# Calculate Root Mean Squared Error (RMSE)
gru_rmse <- sqrt(gru_mse)

# Print the results
cat("GRU Model Error Metrics:\n")
cat("MAE:", gru_mae, "\n")
cat("MSE:", gru_mse, "\n")
cat("RMSE:", gru_rmse, "\n")

#Gated Recurrent Unit (GRU) Model Error Metrics:
#MAE: 22281.65 
#MSE: 919714983 
#RMSE: 30326.8 

# Reshape data into 3D for CNN
x_train <- array_reshape(as.matrix(train_data[, -1]), dim = c(nrow(train_data), ncol(train_data) - 1, 1))
x_test <- array_reshape(as.matrix(test_data[, -1]), dim = c(nrow(test_data), ncol(test_data) - 1, 1))

# Define the CNN model
model_cnn <- keras_model_sequential() %>%
  layer_conv_1d(filters = 32, kernel_size = 3, activation = 'relu', input_shape = c(ncol(train_data) - 1, 1)) %>%
  layer_flatten() %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dense(units = 1)

# Compile and train the model
model_cnn %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(),
  metrics = c('mean_absolute_error')
)

history <- model_cnn %>% fit(
  x_train, train_data$BTC,
  epochs = 50,
  batch_size = 32,
  validation_split = 0.2
)

# Evaluate the model
model_cnn %>% evaluate(x_test, test_data$BTC)

# Predict on the test set
predictions <- model_cnn %>% predict(x_test)

# Calculate MAE, MSE, and RMSE
actuals <- test_data$BTC
mae <- mean(abs(predictions - actuals))
mse <- mean((predictions - actuals)^2)
rmse <- sqrt(mse)

# Print the results
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

#Convolution Neural Network (CNN)
#Mean Absolute Error (MAE): 15.15001 
#Mean Squared Error (MSE): 468.1019 
#Root Mean Squared Error (RMSE): 21.63566 

# Reshape data for CNN + RNN model (samples, timesteps, features)
x_train <- array_reshape(as.matrix(train_data[, -1]), dim = c(nrow(train_data), 1, ncol(train_data) - 1))
x_test <- array_reshape(as.matrix(test_data[, -1]), dim = c(nrow(test_data), 1, ncol(test_data) - 1))

# Define the CNN + RNN model with adjustments
model_cnn_rnn <- keras_model_sequential() %>%
  # CNN Layers
  layer_conv_1d(filters = 32, kernel_size = 3, activation = 'relu', input_shape = c(1, ncol(train_data) - 1), padding = 'same') %>%
  # Avoid pooling or use a pooling size of 1
  layer_max_pooling_1d(pool_size = 1) %>%
  
  # RNN Layer
  layer_lstm(units = 64) %>%
  
  # Fully connected dense layer
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dense(units = 1) # Output layer for regression

# Compile the model
model_cnn_rnn %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(),
  metrics = c('mean_absolute_error')
)

# Train the model
history <- model_cnn_rnn %>% fit(
  x_train, train_data$BTC,
  epochs = 50,
  batch_size = 32,
  validation_split = 0.2
)

# Evaluate and calculate error metrics as before
cnn_rnn_eval <- model_cnn_rnn %>% evaluate(x_test, test_data$BTC)


# Predict on the test data
cnn_rnn_predictions <- model_cnn_rnn %>% predict(x_test)

# Convert predictions to a numeric vector if necessary
cnn_rnn_predictions <- as.numeric(cnn_rnn_predictions)

# Calculate residuals
cnn_rnn_residuals <- test_data$BTC - cnn_rnn_predictions

# Calculate Mean Absolute Error (MAE)
cnn_rnn_mae <- mean(abs(cnn_rnn_residuals))

# Calculate Mean Squared Error (MSE)
cnn_rnn_mse <- mean(cnn_rnn_residuals^2)

# Calculate Root Mean Squared Error (RMSE)
cnn_rnn_rmse <- sqrt(cnn_rnn_mse)

# Print the results
cat("CNN + RNN Model Error Metrics:\n")
cat("MAE:", cnn_rnn_mae, "\n")
cat("MSE:", cnn_rnn_mse, "\n")
cat("RMSE:", cnn_rnn_rmse, "\n")

#Hybrid Model (CNN + RNN) Model Error Metrics:
#MAE: 14869.45 
#MSE: 552350834 
#RMSE: 23502.15 

#########PCA Analysis###############

merged_data.pc <- prcomp(normalized_data[, -c(1, 2)], center = TRUE, scale. = TRUE)

attributes(merged_data.pc)
summary(merged_data.pc)
merged_data.pc$rotation

# plotting the abalone.pc using the a line in plot() functions 
plot(merged_data.pc, type = "l")

# Extract the rotation matrix (loadings)
loadings <- merged_data.pc$rotation

# Select the principal component to analyze (e.g., PC1)
pc1_loadings <- abs(loadings[, 1])  # Absolute values of loadings for PC1

# Create a data frame for plotting
loading_data <- data.frame(
  Variable = rownames(loadings),
  Loading = pc1_loadings
)

# Sort the data by loading values in descending order
loading_data <- loading_data[order(-loading_data$Loading), ]

# Plot the loadings for PC1
library(ggplot2)
ggplot(loading_data, aes(x = reorder(Variable, -Loading), y = Loading)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip coordinates for better readability
  xlab("Variables") + ylab("Absolute Loadings") +
  ggtitle("Variable Contributions to PC1") +
  theme_minimal()

############ReFit KNN Model with Principal Components#################

# Set k for KNN regression
k <- 3

# Perform KNN regression using the first two principal components
KNNpred_pc <- knn.reg(
  train = merged_data.pc$x[, c(1, 2)],  # Use the first two principal components for training
  test = merged_data.pc$x[, c(1, 2)],   # Use the first two principal components for testing
  y = merged_data$BTC,                  # Target variable (BTC)
  k = k
)$pred

# Calculate error metrics for PCA-based KNN predictions
err_pc <- KNNpred_pc - merged_data$BTC

# Calculate MAE, MSE, and RMSE for the PCA-based model
mean_abs_err_pc <- mean(abs(err_pc))
mean_sq_err_pc <- mean(err_pc^2)
root_mean_sq_err_pc <- sqrt(mean_sq_err_pc)

# Print the results for PCA-based KNN model
cat("PCA MAE:", mean_abs_err_pc, "\nPCA MSE:", mean_sq_err_pc, "\nPCA RMSE:", root_mean_sq_err_pc, "\n")

# Plot Actual vs Predicted BTC Prices
plot(
  merged_data$BTC,         # Actual BTC values
  KNNpred_pc,              # Predicted BTC values from KNN regression
  xlab = "Actual BTC Prices", 
  ylab = "Predicted BTC Prices",
  main = "Actual vs Predicted BTC Prices (PCA-based KNN)",
  pch = 16,                # Point style
  col = "blue"             # Point color
)

# Add a reference line (y = x) for perfect prediction
abline(a = 0, b = 1, col = "red", lwd = 2)

#PCA MAE: 670.8867 
#PCA MSE: 2810115 
#PCA RMSE: 1676.34

#PCA MAE(with DFF): 661.6412 
#PCA MSE(with DFF): 2522305 
#PCA RMSE(with DFF): 1588.177 

#with gold and dff PCA MAE: 1012.97 
#with gold and dff PCA MSE: 5919761 
#with gold and dff PCA RMSE: 2433.056 

######SVM#############
###### SVM #############

# Train the SVM model to predict BTC price - linear kernel
svm.mod0 <- svm(BTC ~ SP500 + R3000 + CRSP + DFF + Gold_VIX, data = train_data, kernel = 'linear')

# Check the trained model
svm.mod0

# Predict on the training data (to see the model performance on training set)
train.pred <- predict(svm.mod0, train_data)

# Confusion matrix on the training set (for classification tasks; for regression, use other metrics)
cm = as.matrix(table(Actual = train_data$BTC, Predicted = train.pred))
cm

# Calculate error on the training set
train.err <- train.pred - train_data$BTC

# Now, predict on the test data
test.pred <- predict(svm.mod0, test_data)

# Calculate error on the test data
test.err <- test.pred - test_data$BTC

# Calculate error metrics for the test data
# MAE (Mean Absolute Error)
mae_test <- mean(abs(test.err))

# MSE (Mean Squared Error)
mse_test <- mean(test.err^2)

# RMSE (Root Mean Squared Error)
rmse_test <- sqrt(mse_test)

# Print the error metrics for the test set
cat("Test MAE:", mae_test, "\nTest MSE:", mse_test, "\nTest RMSE:", rmse_test, "\n")


#Test MAE(With All): 5296.489 
#Test MSE(With All): 51051693 
#Test RMSE(With All): 7145.047 

#Test MAE: 5312.441 
#Test MSE: 48539428 
#Test RMSE: 6967.024 

#Test MAE(with DFF): 5074.613 
#Test MSE(with DFF): 47853509 
#Test RMSE(with DFF): 6917.623 

# Plot predicted vs. actual values for training data
ggplot(data.frame(Actual = train_data$BTC, Predicted = train.pred), aes(x = Actual, y = Predicted)) +
  geom_point(color = "red") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Predicted vs. Actual Prices (Training Data)", x = "Actual Prices", y = "Predicted Prices")

##SVM - radial
svm.mod1 <- svm(BTC ~ SP500 + R3000 + CRSP + DFF + Gold_VIX, data = train_data, kernel = 'radial')

svm.mod1

# Predict on the training data (to see the model performance on training set)
train.pred <- predict(svm.mod1, train_data)

# Confusion matrix on the training set (for classification tasks; for regression, use other metrics)
cm = as.matrix(table(Actual = train_data$BTC, Predicted = train.pred))
cm

# Calculate error on the training set
train.err <- train.pred - train_data$BTC

# Now, predict on the test data
test.pred <- predict(svm.mod0, test_data)

# Calculate error on the test data
test.err <- test.pred - test_data$BTC

# Calculate error metrics for the test data
# MAE (Mean Absolute Error)
mae_test <- mean(abs(test.err))

# MSE (Mean Squared Error)
mse_test <- mean(test.err^2)

# RMSE (Root Mean Squared Error)
rmse_test <- sqrt(mse_test)

# Print the error metrics for the test set
cat("Test MAE:", mae_test, "\nTest MSE:", mse_test, "\nTest RMSE:", rmse_test, "\n")

#Test MAE(with all): 5296.489 
#Test MSE(with all): 51051693 
#Test RMSE(with all): 7145.047 

#Test MAE: 5312.441 
#Test MSE: 48539428 
#Test RMSE: 6967.024 

#Test MAE(with DFF): 5074.613 
#Test MSE(with DFF): 47853509 
#Test RMSE(with DFF): 6917.623 

## Tuned SVM - Radial Kernel
# Use tune.svm() to tune the hyperparameters (gamma and cost)
tuned.svm <- tune.svm(
  BTC ~ SP500 + R3000 + CRSP,  # Make sure the same features are used
  data = train_data,           # Use the training data
  kernel = 'radial',           # Radial kernel
  gamma = seq(1 / (2^ncol(train_data[-1])), 1, 0.01),  # Adjusted gamma range based on the number of features
  cost = 2^seq(-6, 4, 2)       # Range of cost parameter values
)

# View the best parameters and performance
summary(tuned.svm)

#View the optimal gamma and cost values - stored in best parameters in tuned.svm
tuned.svm$best.parameters

svm.mod2 <- svm(BTC ~ SP500 + R3000 + CRSP, data = train_data, kernel = 'radial', gamma = 0.99125, cost = 16)

svm.mod2

# Predict on the training data (to see the model performance on training set)
train.pred <- predict(svm.mod2, train_data)

# Confusion matrix on the training set (for classification tasks; for regression, use other metrics)
cm = as.matrix(table(Actual = train_data$BTC, Predicted = train.pred))
cm

# Calculate error on the training set
train.err <- train.pred - train_data$BTC

# Now, predict on the test data
test.pred <- predict(svm.mod0, test_data)

# Calculate error on the test data
test.err <- test.pred - test_data$BTC

# Calculate error metrics for the test data
# MAE (Mean Absolute Error)
mae_test <- mean(abs(test.err))

# MSE (Mean Squared Error)
mse_test <- mean(test.err^2)

# RMSE (Root Mean Squared Error)
rmse_test <- sqrt(mse_test)

# Print the error metrics for the test set
cat("Test MAE:", mae_test, "\nTest MSE:", mse_test, "\nTest RMSE:", rmse_test, "\n")

#Test MAE: 5312.441 
#Test MSE: 48539428 
#Test RMSE: 6967.024 

# Load necessary library
library(e1071)
library(ggplot2)

# Train the SVM model with a polynomial kernel
svm.mod3 <- svm(BTC ~ SP500 + R3000 + CRSP + DFF, data = train_data, kernel = 'polynomial')

# Predict on the training data
train.pred <- predict(svm.mod3, train_data)

# Create a data frame for plotting
plot_data <- data.frame(
  Date = train_data$Date,  # Assuming train_data has a Date column
  Actual = train_data$BTC,
  Predicted = train.pred
)

# Plot the actual vs predicted BTC values
ggplot(plot_data, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"), linetype = "dashed", size = 1) +
  ggtitle("Actual vs Predicted BTC Prices (SVM Polynomial Kernel)") +
  xlab("Date") +
  ylab("BTC Price") +
  scale_color_manual(
    values = c("Actual" = "blue", "Predicted" = "red"),
    name = "Legend"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "top"
  )



# Calculate error on the training set
train.err <- train.pred - train_data$BTC

# Now, predict on the test data
test.pred <- predict(svm.mod0, test_data)

# Calculate error on the test data
test.err <- test.pred - test_data$BTC

# Calculate error metrics for the test data
# MAE (Mean Absolute Error)
mae_test <- mean(abs(test.err))

# MSE (Mean Squared Error)
mse_test <- mean(test.err^2)

# RMSE (Root Mean Squared Error)
rmse_test <- sqrt(mse_test)

# Print the error metrics for the test set
cat("Test MAE:", mae_test, "\nTest MSE:", mse_test, "\nTest RMSE:", rmse_test, "\n")

#Test MAE: 5312.441 
#Test MSE: 48539428 
#Test RMSE: 6967.024 

# Prepare data for XGBoost: Convert train_data and test_data to matrix format
train_matrix <- as.matrix(train_data[, c("SP500", "R3000", "CRSP", "DFF", "Gold_VIX")])
test_matrix <- as.matrix(test_data[, c("SP500", "R3000", "CRSP","DFF", "Gold_VIX")])

# Prepare the target variable (BTC) as a numeric vector
train_label <- train_data$BTC
test_label <- test_data$BTC

# Set XGBoost parameters with regularization
params <- list(
  objective = "reg:squarederror",  # Regression objective
  eval_metric = "rmse",           # Evaluation metric
  eta = 0.5,                      # Learning rate
  max_depth = 6,                  # Maximum depth of trees
  subsample = 1,                # Fraction of samples per tree
  colsample_bytree = 1,         # Fraction of features per tree
  lambda = 2,                     # L2 regularization
  alpha = 1                       # L1 regularization
)

# Train the XGBoost model
xgb.mod0 <- xgboost(
  params = params,                # Pass the parameters
  data = train_matrix,            # Feature matrix
  label = train_label,            # Target variable (BTC)
  nrounds = 1500,                 # Number of boosting rounds
  verbose = 1                     # Display progress during training
)

# Check the trained model
print(xgb.mod0)

#XGBoost Test MAE: 1065.575 
#XGBoost Test MSE: 4964896 
#XGBoost Test RMSE: 2228.205 

# Predict on the training data
train_pred_xgb <- predict(xgb.mod0, train_matrix)

# Calculate error on the training set
train_err_xgb <- train_pred_xgb - train_label

# Predict on the test data
test_pred_xgb <- predict(xgb.mod0, test_matrix)

# Calculate error on the test set
test_err_xgb <- test_pred_xgb - test_label

# Create a time-series plot for predicted vs expected BTC values
plot(test_label, type = "l", 
     col = "blue", 
     lwd = 2,
     main = "XGBoost: Predicted vs Expected BTC Values",
     xlab = "Time (Index)", 
     ylab = "BTC Values")

# Add predicted BTC values as a red line
lines(test_pred_xgb, col = "red", lwd = 2)

# Add a legend to distinguish between the lines
legend("topright", legend = c("Expected BTC", "Predicted BTC"), 
       col = c("blue", "red"), 
       lwd = 2, 
       bty = "n")



# Calculate error metrics for the test data
# MAE (Mean Absolute Error)
mae_test_xgb <- mean(abs(test_err_xgb))

# MSE (Mean Squared Error)
mse_test_xgb <- mean(test_err_xgb^2)

# RMSE (Root Mean Squared Error)
rmse_test_xgb <- sqrt(mse_test_xgb)

# Print the error metrics for the test set
cat("XGBoost Test MAE:", mae_test_xgb, "\nXGBoost Test MSE:", mse_test_xgb, "\nXGBoost Test RMSE:", rmse_test_xgb, "\n")

#XGBoost Test MAE: 1095.679 
#XGBoost Test MSE: 5022822 
#XGBoost Test RMSE: 2241.165 

####################################################################################################################################
###################Best Performing Model by Type####################################################################################
####################################################################################################################################

##################################################
#############################Linear Regression Model:
##################################################
#Linear model (Bitcoin predicted off all factors)
btc_lm_model2000 <- lm(BTC ~ ETH + SP500 + R3000 + CRSP + Gold_VIX + DFF, data = merged_data)
summary(btc_lm_model2000)

#MAE: 4058.785 
#MSE: 30906671 
#RMSE: 5559.377 

# Calculate leverage values
hat_values <- hatvalues(btc_lm_model2000)

# Define threshold for high leverage
n <- nrow(merged_data)
p <- length(coef(btc_lm_model2000))
leverage_threshold <- 2 * (p / n)

# Identify high leverage points
high_leverage_points <- which(hat_values > leverage_threshold)

# Create a new dataset excluding high leverage points
merged_data_cleaned <- merged_data[-high_leverage_points, ]

# Rerun the linear model on the cleaned dataset
btc_lm_model_cleaned <- lm(BTC ~ ETH + SP500 + R3000 + CRSP + Gold_VIX + DFF, data = merged_data_cleaned)

# Summary of the new model
summary(btc_lm_model_cleaned)

# Evaluate the new model
predictions_cleaned <- predict(btc_lm_model_cleaned, newdata = merged_data_cleaned)
actual_cleaned <- merged_data_cleaned$BTC
mae <- mean(abs(predictions_cleaned - actual_cleaned))
mse <- mean((predictions_cleaned - actual_cleaned)^2)
rmse <- sqrt(mse)

# Print performance metrics
cat("MAE:", mae, "\nMSE:", mse, "\nRMSE:", rmse)

#(Linear model after removing high leverage points. still wasn't much better)MAE: 3793.237 
#MSE: 28736906 
#RMSE: 5360.682
plot(btc_lm_model_cleaned)


#> cor(merged_data[, c("BTC", "ETH", "SP500", "R3000", "CRSP", "DFF", "Gold_VIX")])

              #BTC       ETH     SP500     R3000      CRSP        DFF   Gold_VIX
#BTC      1.0000000 0.9343030 0.9256411 0.9332224 0.9339170  0.3743626  0.1880798
#ETH      0.9343030 1.0000000 0.8863276 0.8950246 0.8958538  0.3131773  0.1395515
#SP500    0.9256411 0.8863276 1.0000000 0.9985495 0.9983236  0.5378356  0.1720261
#R3000    0.9332224 0.8950246 0.9985495 1.0000000 0.9999805  0.5054523  0.1687286
#CRSP     0.9339170 0.8958538 0.9983236 0.9999805 1.0000000  0.5024670  0.1686403
#DFF      0.3743626 0.3131773 0.5378356 0.5054523 0.5024670  1.0000000 -0.2291665
#Gold_VIX 0.1880798 0.1395515 0.1720261 0.1687286 0.1686403 -0.2291665  1.0000000

##################################################
#############################Random Forrest Regression Model:
##################################################
# Train Random Forest model - BTC Based of All Factors
rf_model <- randomForest(BTC ~ ETH + SP500 + R3000 + CRSP + DFF + Gold_VIX, data = train_data, ntree = 500, importance = TRUE)


#MAE: 360.6366
#MSE: 509703.8
#RMSE: 713.9354
##################################################
#############################KNN Regression Model:
##################################################

k <- 3

# Perform KNN regression
KNNpred <- knn.reg(
  train = normalized_train[, 2:8], 
  test = normalized_test[, 2:8], 
  y = normalized_train$BTC, 
  k = k
)$pred

#MAE: 19.72659 
#MSE: 1496.12 
#RMSE: 38.67971 

##################################################
#############################SVM Model (Linear Kernal)l:
##################################################

# Train the SVM model to predict BTC price - linear kernel
svm.mod0 <- svm(BTC ~ SP500 + R3000 + CRSP + DFF + Gold_VIX, data = train_data, kernel = 'linear')

#Test MAE(With All): 5296.489 
#Test MSE(With All): 51051693 
#Test RMSE(With All): 7145.047 

##################################################
#############################XGBOOST Model:
##################################################

# Set XGBoost parameters with regularization
params <- list(
  objective = "reg:squarederror",  # Regression objective
  eval_metric = "rmse",           # Evaluation metric
  eta = 0.5,                      # Learning rate
  max_depth = 6,                  # Maximum depth of trees
  subsample = 1,                # Fraction of samples per tree
  colsample_bytree = 1,         # Fraction of features per tree
  lambda = 2,                     # L2 regularization
  alpha = 1                       # L1 regularization
)

# Train the XGBoost model
xgb.mod0 <- xgboost(
  params = params,                # Pass the parameters
  data = train_matrix,            # Feature matrix
  label = train_label,            # Target variable (BTC)
  nrounds = 1500,                 # Number of boosting rounds
  verbose = 1                     # Display progress during training
)


#XGBoost Test MAE: 1065.575 
#XGBoost Test MSE: 4964893 
#XGBoost Test RMSE: 2228.204

##################################################
#############################Neural Network Model:
##################################################

#Convolutional Neural Network

# Define the CNN model
model_cnn <- keras_model_sequential() %>%
  layer_conv_1d(filters = 32, kernel_size = 3, activation = 'relu', input_shape = c(ncol(train_data) - 1, 1)) %>%
  layer_flatten() %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dense(units = 1)

# Compile and train the model
model_cnn %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(),
  metrics = c('mean_absolute_error')
)

history <- model_cnn %>% fit(
  x_train, train_data$BTC,
  epochs = 50,
  batch_size = 32,
  validation_split = 0.2
)

# Plot the training history
plot(history)


#Convolution Neural Network (CNN)
#Mean Absolute Error (MAE): 15.15001 
#Mean Squared Error (MSE): 468.1019 
#Root Mean Squared Error (RMSE): 21.63566



