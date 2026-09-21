# -------------------------------------------------------------------------
# SCRIPT: 03-analise-grafica-e-visual.R
# OBJETIVO: Criar visualizações para comparar os dados observados com as
#           previsões dos modelos (BRAMS, ETA, WRF) para o ano de 2023.
# -------------------------------------------------------------------------


# 1. PACOTES E DADOS -----------------------------------------------------
library(tidyverse)
library(lubridate)

# Carregar os dados preparados pelo script 01
caminho_dados <- "Relatórios/dados_preparados.rds"
if (!file.exists(caminho_dados)) {
  stop("Arquivo 'dados_preparados.rds' não encontrado. Execute o script 01 primeiro.")
}
dados_preparados <- readRDS(caminho_dados)

# Criar a pasta 'Resultados/Graficos' se ela não existir
if (!dir.exists("Resultados/Graficos")) {
  dir.create("Resultados/Graficos", recursive = TRUE)
}

# 2. GRÁFICO DE SÉRIES TEMPORAIS -----------------------------------------
# Essencial para observar e comparar o desempenho histórico dos modelos[cite: 161].

grafico_series_temporais <- dados_preparados %>%
  pivot_longer(cols = c(temperatura, umidade, precipitacao), names_to = "variavel", values_to = "valor") %>%
  ggplot(aes(x = data, y = valor, color = modelo)) +
  geom_line(alpha = 0.7) +
  facet_wrap(~variavel, scales = "free_y", strip.position = "left", 
             labeller = as_labeller(c(temperatura = "Temperatura (°C)", umidade = "Umidade (%)", precipitacao = "Precipitação (mm)"))) +
  labs(
    title = "Séries Temporais Diárias em 2023: Observado vs. Modelos",
    subtitle = "Comparação do comportamento diário das variáveis climáticas",
    x = "Data",
    y = NULL,
    color = "Fonte dos Dados"
  ) +
  theme_light() +
  theme(strip.placement = "outside", legend.position = "bottom")

# Exibir o gráfico
print(grafico_series_temporais)

# Salvar o gráfico
ggsave("Resultados/Graficos/series_temporais_comparativas.png", plot = grafico_series_temporais, width = 12, height = 8)


# 3. GRÁFICOS DE DISPERSÃO (SCATTER PLOTS) --------------------------------
# Mostram a correlação entre o previsto e o observado, fundamental para a Análise de Correlações (PTG 2.4.4).

# Reorganizar os dados para o formato "largo"
dados_largos <- dados_preparados %>%
  pivot_wider(names_from = modelo, values_from = c(temperatura, umidade, precipitacao)) %>%
  filter(if_all(everything(), ~ !is.na(.))) # Remove linhas com qualquer NA para a correlação

# Função para criar os gráficos de dispersão
criar_grafico_dispersao <- function(dados, var_prefix, titulo_var, unidade) {
  var_obs_col <- paste0(var_prefix, "_OBS")
  
  dados_longos_modelos <- dados %>%
    select(all_of(var_obs_col), starts_with(paste0(var_prefix, "_")) & !ends_with("_OBS")) %>%
    pivot_longer(
      cols = -all_of(var_obs_col),
      names_to = "modelo",
      names_prefix = paste0(var_prefix, "_"),
      values_to = "previsto"
    )
  
  ggplot(dados_longos_modelos, aes(x = .data[[var_obs_col]], y = previsto)) +
    geom_point(alpha = 0.4) +
    geom_abline(color = "red", linetype = "dashed", size = 1) + # Linha de previsão perfeita
    facet_wrap(~modelo) +
    labs(
      title = paste("Dispersão entre Previsto vs. Observado para", titulo_var),
      subtitle = "A linha vermelha representa a previsão perfeita (y = x)",
      x = paste("Observado", unidade),
      y = paste("Previsto pelo Modelo", unidade)
    ) +
    theme_minimal()
}

# Criar e salvar os gráficos de dispersão
grafico_disp_temp <- criar_grafico_dispersao(dados_largos, "temperatura", "Temperatura", "(°C)")
grafico_disp_umid <- criar_grafico_dispersao(dados_largos, "umidade", "Umidade", "(%)")
grafico_disp_prec <- criar_grafico_dispersao(dados_largos, "precipitacao", "Precipitação", "(mm)")

print(grafico_disp_temp)
ggsave("Resultados/Graficos/dispersao_temperatura.png", plot = grafico_disp_temp, width = 10, height = 4)
print(grafico_disp_umid)
ggsave("Resultados/Graficos/dispersao_umidade.png", plot = grafico_disp_umid, width = 10, height = 4)
print(grafico_disp_prec)
ggsave("Resultados/Graficos/dispersao_precipitacao.png", plot = grafico_disp_prec, width = 10, height = 4)


# 4. BOXPLOTS -------------------------------------------------------------
# Comparam a distribuição dos dados (média, mediana, quartis), apoiando a Estatística Descritiva (PTG 2.4.1).

grafico_boxplots <- dados_preparados %>%
  pivot_longer(cols = c(temperatura, umidade, precipitacao), names_to = "variavel", values_to = "valor") %>%
  ggplot(aes(x = modelo, y = valor, fill = modelo)) +
  geom_boxplot() +
  facet_wrap(~variavel, scales = "free_y") +
  labs(
    title = "Distribuição das Variáveis por Fonte de Dados em 2023",
    x = "Fonte",
    y = "Valor",
    fill = "Fonte"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

print(grafico_boxplots)
ggsave("Resultados/Graficos/boxplots_distribuicao.png", plot = grafico_boxplots, width = 10, height = 6)

message("\nAnálise gráfica concluída! Gráficos salvos na pasta 'Resultados/Graficos/'.")