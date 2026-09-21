#-----------------------------------------------------------------------------
# Script 04 - Análise de Erros (RMSE e BIAS) — versão final com gráficos
# Autor: Gabriel de Lima Marins
# Data: 09/10/2025
#-----------------------------------------------------------------------------

library(tidyverse)
library(gt)
library(webshot2)

# Caminhos
data_path <- "C:/Users/gabri/OneDrive/Documents/TG_Clima_Indaiatuba/Dados/"
resultados_path <- "C:/Users/gabri/OneDrive/Documents/TG_Clima_Indaiatuba/Resultados/"
path_consolidado <- file.path(data_path, "dados_consolidados_2023.csv")

# Carregar dados
dados <- read_csv(path_consolidado, show_col_types = FALSE)

#-----------------------------------------------------------------------------
# Funções de cálculo
#-----------------------------------------------------------------------------

calc_rmse <- function(obs, est) sqrt(mean((est - obs)^2, na.rm = TRUE))
calc_bias <- function(obs, est) mean(est - obs, na.rm = TRUE)

#-----------------------------------------------------------------------------
# Estrutura das comparações
#-----------------------------------------------------------------------------

comparisons <- tribble(
  ~variavel, ~obs,        ~model,
  "Temperatura", "Temp_OBS", "Temp_WRF",
  "Temperatura", "Temp_OBS", "Temp_ETA",
  "Temperatura", "Temp_OBS", "Temp_BRAMS",
  "Umidade Relativa", "UR_OBS", "UR_WRF",
  "Umidade Relativa", "UR_OBS", "UR_ETA",
  "Umidade Relativa", "UR_OBS", "UR_BRAMS",
  "Precipitação", "Prec_OBS", "Prec_WRF",
  "Precipitação", "Prec_OBS", "Prec_ETA",
  "Precipitação", "Prec_OBS", "Prec_BRAMS"
) %>%
  filter(obs %in% names(dados), model %in% names(dados))

#-----------------------------------------------------------------------------
# Cálculo das métricas
#-----------------------------------------------------------------------------

erros_tbl <- comparisons %>%
  mutate(
    RMSE = map2_dbl(obs, model, ~ calc_rmse(dados[[.x]], dados[[.y]])),
    BIAS = map2_dbl(obs, model, ~ calc_bias(dados[[.x]], dados[[.y]]))
  )

#-----------------------------------------------------------------------------
# Tabelas compactas
#-----------------------------------------------------------------------------

criar_tabela_erros <- function(df, modelo_nome) {
  tabela <- df %>%
    filter(str_detect(model, modelo_nome)) %>%
    select(Variável = variavel, Modelo = model, RMSE, BIAS) %>%
    gt() %>%
    tab_header(
      title = md(paste0("**Análise de Erros — Modelo ", modelo_nome, " (2023)**")),
      subtitle = "Métricas RMSE e BIAS em relação aos dados observados"
    ) %>%
    fmt_number(columns = c(RMSE, BIAS), decimals = 3, dec_mark = ",", sep_mark = ".") %>%
    tab_source_note(source_note = md("Fonte: Estação meteorológica de Indaiatuba  \nModelos de previsão: BRAMS, ETA e WRF.")) %>%
    tab_options(
      table.font.size = 13,
      data_row.padding = px(2),
      column_labels.padding = px(2),
      table.width = pct(55),
      heading.align = "center"
    ) %>%
    cols_width(
      Variável ~ px(140),
      Modelo ~ px(110),
      RMSE ~ px(90),
      BIAS ~ px(90)
    ) %>%
    cols_align(align = "center", columns = everything())
  
  output_file <- file.path(resultados_path, paste0("Tabela_Erros_", modelo_nome, ".png"))
  gtsave(tabela, output_file, vwidth = 700, vheight = 480)
}

criar_tabela_erros(erros_tbl, "WRF")
criar_tabela_erros(erros_tbl, "ETA")
criar_tabela_erros(erros_tbl, "BRAMS")

#-----------------------------------------------------------------------------
# Gráfico 1 - Barras agrupadas (RMSE e BIAS)
#-----------------------------------------------------------------------------

erros_long <- erros_tbl %>%
  mutate(Modelo = case_when(
    str_detect(model, "WRF") ~ "WRF",
    str_detect(model, "ETA") ~ "ETA",
    str_detect(model, "BRAMS") ~ "BRAMS"
  )) %>%
  pivot_longer(cols = c(RMSE, BIAS), names_to = "Métrica", values_to = "Valor")

grafico_barras <- ggplot(erros_long, aes(x = Modelo, y = Valor, fill = Modelo)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Métrica + variavel, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c("BRAMS" = "#F8766D", "ETA" = "#00BA38", "WRF" = "#619CFF")) +
  labs(
    title = "Métricas de Erro (RMSE e BIAS) — Modelos de Previsão (2023)",
    subtitle = "Comparação entre BRAMS, ETA e WRF para Precipitação, Temperatura e Umidade Relativa",
    x = "Modelo de Previsão",
    y = "Valor da Métrica",
    caption = "Fonte: Estação meteorológica de Indaiatuba  \nModelos: BRAMS, ETA e WRF."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

print(grafico_barras)

ggsave(
  filename = file.path(resultados_path, "Grafico_Barras_RMSE_BIAS_Modelos.png"),
  plot = grafico_barras,
  width = 10,
  height = 6,
  dpi = 300
)

#-----------------------------------------------------------------------------
# Gráfico 2 - Dispersão (Previsões vs Observações) — Temperatura
#-----------------------------------------------------------------------------

dados_temp <- dados %>%
  select(Data, Temp_OBS, Temp_WRF, Temp_ETA, Temp_BRAMS) %>%
  pivot_longer(cols = c(Temp_WRF, Temp_ETA, Temp_BRAMS),
               names_to = "Modelo", values_to = "Previsao") %>%
  mutate(Modelo = str_remove(Modelo, "Temp_"))

grafico_disp <- ggplot(dados_temp, aes(x = Temp_OBS, y = Previsao, color = Modelo)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  coord_equal() +
  scale_color_manual(values = c("BRAMS" = "#F8766D", "ETA" = "#00BA38", "WRF" = "#619CFF")) +
  labs(
    title = "Relação entre Previsões e Observações — Temperatura (2023)",
    subtitle = "Comparação dos modelos BRAMS, ETA e WRF em relação aos dados observados",
    x = "Temperatura Observada (°C)",
    y = "Temperatura Prevista (°C)",
    color = "Modelo",
    caption = "Fonte: Estação meteorológica de Indaiatuba  \nModelos: BRAMS, ETA e WRF."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

print(grafico_disp)

ggsave(
  filename = file.path(resultados_path, "Grafico_Dispersao_Temperatura.png"),
  plot = grafico_disp,
  width = 8,
  height = 6,
  dpi = 300
)
