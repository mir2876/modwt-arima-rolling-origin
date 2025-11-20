# ────────────────────────────────
# 1. Load Packages
# ────────────────────────────────
setwd("D:/Dosen/Riset/MODWT-ARIMA-new")
library(dplyr)
library(FSA)       # untuk dunnTest()
library(readr)

# ────────────────────────────────
# 2. Import Data
# ────────────────────────────────
# Misal file CSV berisi kolom:
# Dataset | Level | Filter | sMAPE
# (Jika sudah long format seperti ini, langsung lanjut ke langkah 3)

df <- read_csv("smape.csv")

# Cek struktur
glimpse(df)
df_norm <- df %>%
  group_by(dataset) %>%
  mutate(SMAPE_norm = as.numeric(scale(SMAPE))) %>% 
  ungroup()

# ────────────────────────────────
# 3. Kruskal–Wallis Test
# ────────────────────────────────
# Uji beda SMAPE antar filter
kw_result <- kruskal.test(SMAPE_norm ~ filter, data = df_norm)
kw_result

# ────────────────────────────────
# 4. Post-hoc Dunn Test 
# ────────────────────────────────
# Holm recommended (lebih powerful dari Bonferroni)
dunn_result <- dunnTest(SMAPE_norm ~ filter, data = df_norm, method = "holm")
dunn_result

# ────────────────────────────────
# 5. Tabel ringkas hasil Dunn
# ────────────────────────────────
posthoc_table <- dunn_result$res
posthoc_table

# ────────────────────────────────
# 6. Interpretasi otomatis (opsional)
# ────────────────────────────────
if (kw_result$p.value < 0.05) {
  cat("\nKruskal-Wallis signifikan (p < 0.05). Ada perbedaan SMAPE antar filter.\n",
      "Periksa tabel Dunn untuk pasangan mana yang berbeda.\n")
} else {
  cat("\nKruskal-Wallis tidak signifikan (p >= 0.05). ",
      "Tidak ada bukti perbedaan antar filter.\n")
}
#hasil dunn
dunn_result$res
write.csv(dunn_result$res, "Dunn_Test_Results_smape.csv", row.names = FALSE)
#simpan hasil normalisasi
df_norm_selected <- df_norm %>%
  select(dataset, filter, level, SMAPE, SMAPE_norm)

write.csv(df_norm_selected, "smape_norm_selected.csv", row.names = FALSE)
