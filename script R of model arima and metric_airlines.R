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

# Setting directory
setwd("D:/Dosen/Riset/MODWT-ARIMA-new")

# ──────────────────────────────────────────────
# 📥 2. Load Data
# ──────────────────────────────────────────────
data_raw <- read.delim("data/airlines.csv", header = TRUE)
data_series <- as.numeric(data_raw[[1]])

n <- length(data_series)
initial_window <- floor(0.8 * n)
h <- 1   

# ──────────────────────────────────────────────
# 📦 3. Vectors to Store Errors + SARIMA orders
# ──────────────────────────────────────────────
rmse_all <- c()
mae_all  <- c()
mape_all <- c()
smape_all <- c()

sarima_orders_all <- list()   # NEW: menyimpan order SARIMA untuk tiap origin

# ──────────────────────────────────────────────
# 🔄 4. Rolling-Origin Evaluation
# ──────────────────────────────────────────────
for (origin in initial_window:(n - h)) {
  
  cat("\n=========================\n")
  cat("Origin:", origin, "\n")
  cat("=========================\n")
  
  train_data <- data_series[1:origin]
  test_value <- data_series[origin + h]
  
  # MODWT — ganti J sesuai kebutuhan, misalnya 3
  wt <- mra(train_data, wf = "haar", J = 3, method = "modwt")
  
  # NEW: simpan order untuk origin ini
  origin_orders <- list()
  
  # Reconstruct forecast
  forecast_sum <- 0
  
  for (i in 1:length(wt)) {
    
    wt_ts <- ts(wt[[i]], frequency = 12)  # seasonal
    
    model <- auto.arima(
      wt_ts,
      seasonal = TRUE,
      stepwise = FALSE,
      approximation = FALSE
    )
    
    # 🔍 PRINT model SARIMA yang keluar
    cat("\nLevel:", names(wt)[i], " → Model SARIMA:\n")
    print(model)
    
    # 🔥 NEW: Simpan order SARIMA untuk level ini
    ord <- model$arma
    origin_orders[[names(wt)[i]]] <- sprintf(
      "(%d,%d,%d)(%d,%d,%d)[%d]",
      ord[1], ord[6], ord[2],   # p,d,q
      ord[3], ord[7], ord[4],   # P,D,Q
      ord[5]                    # s (seasonal period)
    )
    
    fc <- forecast(model, h = h)
    forecast_sum <- forecast_sum + fc$mean[1]
  }
  
  # Simpan semua order SARIMA untuk origin ini
  sarima_orders_all[[paste0("origin_", origin)]] <- origin_orders
  
  # Store errors
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

# Save summary
write.csv(results, "haar_3_airlines_summary.csv", row.names = FALSE)

# 🔥 NEW: simpan SARIMA orders ke file
saveRDS(sarima_orders_all, "sarima_orders_by_origin.rds")

cat("\nSARIMA order telah disimpan ke sarima_orders_by_origin.rds\n")
