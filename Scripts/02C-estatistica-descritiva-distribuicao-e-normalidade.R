#-----------------------------------------------------------------------------
# Script 02C - Estatística Descritiva (Distribuições e Caudas)
#-----------------------------------------------------------------------------

library(tidyverse)

# Caminhos
resultados_path <- "C:/Users/gabri/OneDrive/Documents/TG_Clima_Indaiatuba/Resultados/"
data_path <- "C:/Users/gabri/OneDrive/Documents/TG_Clima_Indaiatuba/Dados/"
path_consolidado <- file.path(data_path, "dados_consolidados_2023.csv")

# Carregar dados
dados_finais <- read_csv(path_consolidado, show_col_types = FALSE)

# Reorganizar dados em formato longo + filtro para evitar avisos
dados_long <- dados_finais %>%
  pivot_longer(
    cols = -Data,
    names_to = c("Variavel", "Fonte"),
    names_sep = "_"
  ) %>%
  filter(!is.na(value), is.finite(value))

#===============================
# Gráfico 1 - Histogramas por variável e fonte
#===============================
g1 <- ggplot(dados_long, aes(x = value, fill = Fonte)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  facet_wrap(~Variavel, scales = "free") +
  labs(title = "Distribuição das Variáveis (Histograma)",
       subtitle = "Mostra a forma da distribuição dos dados por variável e modelo",
       x = "Valor", y = "Frequência") +
  theme_minimal()

ggsave(filename = file.path(resultados_path, "Grafico_Histograma.png"),
       plot = g1, width = 12, height = 6, dpi = 300)

#===============================
# Gráfico 2 - Densidade (curva suave de distribuição)
#===============================
g2 <- ggplot(dados_long, aes(x = value, color = Fonte, fill = Fonte)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~Variavel, scales = "free") +
  labs(title = "Distribuição de Densidade",
       subtitle = "Mostra se há assimetria (caudas longas) ou concentração",
       x = "Valor", y = "Densidade") +
  theme_minimal()

ggsave(filename = file.path(resultados_path, "Grafico_Densidade.png"),
       plot = g2, width = 12, height = 6, dpi = 300)

#===============================
# Gráfico 3 - QQ Plot (comparação com Normalidade)
#===============================
g3 <- ggplot(dados_long, aes(sample = value, color = Fonte)) +
  stat_qq(alpha = 0.5) +
  stat_qq_line(color = "red") +
  facet_wrap(~Variavel, scales = "free") +
  labs(title = "QQ Plot",
       subtitle = "Compara a distribuição dos dados com a Normal teórica",
       x = "Quantis Teóricos", y = "Quantis Amostrais") +
  theme_minimal()

ggsave(filename = file.path(resultados_path, "Grafico_QQPlot.png"),
       plot = g3, width = 12, height = 6, dpi = 300)

#===============================
# Gráfico 4 - Boxplot detalhado com caudas longas e outliers
#===============================
g4 <- ggplot(dados_long, aes(x = Fonte, y = value, fill = Fonte)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 4, alpha = 0.7) +
  facet_wrap(~Variavel, scales = "free_y") +
  labs(title = "Boxplot Detalhado",
       subtitle = "Mostra assimetria, caudas longas e valores extremos (outliers)",
       x = "Fonte", y = "Valor") +
  theme_minimal()

ggsave(filename = file.path(resultados_path, "Grafico_Boxplot_Caudas.png"),
       plot = g4, width = 12, height = 6, dpi = 300)
