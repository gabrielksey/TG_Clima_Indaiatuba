#-----------------------------------------------------------------------------
# Script 02B - Estatística Descritiva (Gráficos) - Ajustado
#-----------------------------------------------------------------------------

library(tidyverse)

# Caminhos
resultados_path <- "C:/Users/gabri/OneDrive/Documents/TG_Clima_Indaiatuba/Resultados/"
data_path <- "C:/Users/gabri/OneDrive/Documents/TG_Clima_Indaiatuba/Dados/"
path_consolidado <- file.path(data_path, "dados_consolidados_2023.csv")

# Carregar dados
dados_finais <- read_csv(path_consolidado, show_col_types = FALSE)

# Reorganizar dados em formato longo
dados_long <- dados_finais %>%
  pivot_longer(
    cols = -Data,
    names_to = c("Variavel", "Fonte"),
    names_sep = "_"
  ) %>%
  # Filtrar apenas valores válidos (sem NA, Inf ou -Inf)
  filter(!is.na(value), is.finite(value))

#===============================
# Gráfico 1 - Média e Mediana
#===============================
estat_medias <- dados_long %>%
  group_by(Variavel, Fonte) %>%
  summarise(
    Media = mean(value, na.rm = TRUE),
    Mediana = median(value, na.rm = TRUE),
    .groups = "drop"
  )

g1 <- ggplot(estat_medias, aes(x = Fonte, y = Media, fill = Variavel)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  geom_point(aes(y = Mediana), color = "black", size = 3,
             position = position_dodge(width = 0.8)) +
  labs(title = "Média (barras) e Mediana (pontos)",
       subtitle = "Comparação entre Observado (OBS) e Modelos de Previsão",
       y = "Valor") +
  theme_minimal()

ggsave(filename = file.path(resultados_path, "Grafico_Media_Mediana.png"),
       plot = g1, width = 10, height = 6, dpi = 300)

#===============================
# Gráfico 2 - Desvio Padrão e Variância
#===============================
estat_disp <- dados_long %>%
  group_by(Variavel, Fonte) %>%
  summarise(
    DP = sd(value, na.rm = TRUE),
    Variancia = var(value, na.rm = TRUE),
    .groups = "drop"
  )

g2 <- ggplot(estat_disp, aes(x = Fonte, y = DP, fill = Variavel)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  labs(title = "Desvio Padrão",
       subtitle = "Quantificação da dispersão em torno da média",
       y = "Valor") +
  theme_minimal()

ggsave(filename = file.path(resultados_path, "Grafico_DesvioPadrao.png"),
       plot = g2, width = 10, height = 6, dpi = 300)

#===============================
# Gráfico 3 - Boxplot com Quartis e Percentis
#===============================
g3 <- ggplot(dados_long, aes(x = Fonte, y = value, fill = Fonte)) +
  geom_boxplot(outlier.size = 1, alpha = 0.7) +
  facet_wrap(~Variavel, scales = "free_y") +
  labs(title = "Boxplot com Quartis (Q1, Mediana, Q3)",
       subtitle = "Exibe também outliers. Percentis 10% e 90% podem ser destacados.") +
  theme_minimal()

ggsave(filename = file.path(resultados_path, "Grafico_Boxplot_Quartis.png"),
       plot = g3, width = 12, height = 6, dpi = 300)

#===============================
# Gráfico 4 - Percentis 10% e 90%
#===============================
estat_percentis <- dados_long %>%
  group_by(Variavel, Fonte) %>%
  summarise(
    P10 = quantile(value, 0.10, na.rm = TRUE),
    P90 = quantile(value, 0.90, na.rm = TRUE),
    .groups = "drop"
  )

g4 <- ggplot(estat_percentis, aes(x = Fonte, fill = Variavel)) +
  geom_col(aes(y = P90), position = position_dodge(width = 0.8),
           width = 0.6, alpha = 0.6) +
  geom_point(aes(y = P10), color = "red", size = 3,
             position = position_dodge(width = 0.8)) +
  labs(title = "Percentis 10% (pontos vermelhos) e 90% (barras)",
       subtitle = "Mede dispersão e extremos da distribuição",
       y = "Valor") +
  theme_minimal()

ggsave(filename = file.path(resultados_path, "Grafico_Percentis.png"),
       plot = g4, width = 10, height = 6, dpi = 300)
