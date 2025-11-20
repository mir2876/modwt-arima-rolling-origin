# ──────────────────────────────────────────────
# 🔧 1. Load Library
# ──────────────────────────────────────────────
library(waveslim)
library(forecast)
library(Metrics)
library(tseries)

# Custom sMAPE function
smape <- function(actual, forecast) {
  return(mean(2 * abs(forecast - actual) / (abs(actual) + abs(forecast)) * 100))
}
#setting directory yang digunakan 
setwd("D:/Dosen/Riset/MODWT-ARIMA-new")

# ──────────────────────────────────────────────
# 📥 2. Load Data
# ──────────────────────────────────────────────
data_raw <- read.delim("data/simulation.csv", header = TRUE) #bisa diganti dengan data yang ingin dianalisis
data_series <- as.numeric(data_raw[[1]])

n <- length(data_series)
initial_window <- floor(0.8 * n)  # expanding window start
h <- 1                             # 1-step-ahead forecast

# ──────────────────────────────────────────────
# 📦 3. Vectors to Store All Errors
# ──────────────────────────────────────────────
rmse_all <- c()
mae_all  <- c()
mape_all <- c()
smape_all <- c()

# ──────────────────────────────────────────────
# 🔄 4. Rolling-Origin Evaluation
# ──────────────────────────────────────────────
for (origin in initial_window:(n - h)) {
  
  cat("Origin:", origin, "\n")
  
  train_data <- data_series[1:origin]
  test_value <- data_series[origin + h]
  
  # MODWT
  wt <- mra(train_data, wf = "bl20", 1, method = "modwt") #sesuaikan nama filter dan levelnya
  
  # Reconstruct forecast from all levels
  forecast_sum <- 0
  
  for (i in 1:length(wt)) {
    model <- auto.arima(wt[[i]])
    fc <- forecast(model, h = h)
    forecast_sum <- forecast_sum + fc$mean[1]
  }
  
  # Store errors for this origin
  rmse_all  <- c(rmse_all, rmse(test_value, forecast_sum))
  mae_all   <- c(mae_all, mae(test_value, forecast_sum))
  mape_all  <- c(mape_all, mape(test_value, forecast_sum))
  smape_all <- c(smape_all, smape(test_value, forecast_sum))
}

# ──────────────────────────────────────────────
# 📊 5. Summary Statistics
# ──────────────────────────────────────────────

results <- data.frame(
  Metric = c("RMSE", "MAE", "MAPE", "sMAPE"),
  Mean   = c(mean(rmse_all), mean(mae_all), mean(mape_all), mean(smape_all)),
  Median = c(median(rmse_all), median(mae_all), median(mape_all), median(smape_all)),
  SD     = c(sd(rmse_all), sd(mae_all), sd(mape_all), sd(smape_all)),
  Min    = c(min(rmse_all), min(mae_all), min(mape_all), min(smape_all)),
  Max    = c(max(rmse_all), max(mae_all), max(mape_all), max(smape_all))
)

print(results)

# Save summary, akan tersimpan di directory yang sama
write.csv(results, "bl20_1_airlines_summary.csv", row.names = FALSE) #sesuaikan nama file


