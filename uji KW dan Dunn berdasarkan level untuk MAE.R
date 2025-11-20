# ────────────────────────────────
# 1. Load packages
# ────────────────────────────────
library(dplyr)
library(FSA)
library(tidyr)
setwd("D:/Dosen/Riset/MODWT-ARIMA-new")
df <- read_csv("mae.csv")
# ────────────────────────────────
# 2. Normalisasi MAE per dataset
# ────────────────────────────────
# df harus punya kolom: Dataset, Level, RMSE

df_norm <- df %>%
  group_by(dataset) %>%
  mutate(MAE_norm = as.numeric(scale(MAE))) %>%
  ungroup()

# Cek hasil
head(df_norm)

# ────────────────────────────────
# 3. Uji Kruskal–Wallis untuk 7 level
# ────────────────────────────────
kw_result <- kruskal.test(MAE_norm ~ factor(level), data = df_norm)
kw_result

# ────────────────────────────────
# 4. Post-hoc Dunn Test
# ────────────────────────────────
dunn_res <- dunnTest(MAE_norm ~ factor(level),
                     data = df_norm,
                     method = "holm")

# Lihat hasil lengkap
dunn_res

# ────────────────────────────────
# 5. Tabel hasil Dunn Test (rapi)
# ────────────────────────────────
dunn_table <- dunn_res$res %>%
  arrange(P.adj)

dunn_table

# ────────────────────────────────
# 6. Simpan file hasil (opsional)
# ────────────────────────────────
#write.csv(df_norm, "MAE_normalized.csv", row.names = FALSE)
write.csv(dunn_table, "Dunn_test_levels.csv", row.names = FALSE)

