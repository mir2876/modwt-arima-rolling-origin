Description of R Scripts
1. script_R_of_model_arima_and_metric_airlines.R
This script is specifically used for the Airlines dataset, where the time series frequency is set to 12 (monthly data).
The script:
fits ARIMA models for the Airlines data,
computes the forecasting errors,
produces RMSE, MAE, MAPE, and SMAPE.
This script is not used for the other datasets.

2. MODWT_ARIMA_rolling_origin_dg_metric_lengkap.R
This script is used for all other datasets.
It performs the complete workflow:
MODWT decomposition using different filters and levels,
rolling-origin forecasting,
ARIMA modeling on decomposed series,
reconstruction of forecasts,
calculation of RMSE, MAE, MAPE, and SMAPE for each combination of dataset × filter × level.
The script automatically returns the average RMSE, MAE, MAPE, and SMAPE for each experimental setting.

3. Scripts for Statistical Testing
The Kruskal–Wallis (KW) and Dunn post-hoc tests are conducted using separate scripts:
KW_Dunn_MAE.R
KW_Dunn_SMAPE.R

These scripts run the KW and Dunn tests for MAE and SMAPE, respectively.
Before performing these tests, the MAE or SMAPE values from all datasets, filters, and decomposition levels must be combined into a single table.

Example for mae.csv:

dataset	filter	level	MAE
