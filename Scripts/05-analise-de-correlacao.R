#-----------------------------------------------------------------------------
# Script 05 - Análise de Correlações (Coeficiente de Pearson)
# Autor: Gabriel de Lima Marins
# Data: 14/10/2025
#-----------------------------------------------------------------------------

library(tidyverse)
library(gt)
library(webshot2)
library(patchwork)

#-----------------------------------------------------------------------------
# Caminhos
#-----------------------------------------------------------------------------
data_path <- "C:/Users/gabri/OneDrive/Documents/TG_Clima_Indaiatuba/Dados/"
resultados_path <- "C:/Users/gabri/OneDrive/Documents/TG_Clima_Indaiatuba/Resultados/"
path_consolidado <- file.path(data_path, "dados_consolidados_2023.csv")

#-----------------------------------------------------------------------------
# Carregar dados
#-----------------------------------------------------------------------------
dados <- read_csv(path_consolidado, show_col_types = FALSE)

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
# Cálculo do Coeficiente de Correlação de Pearson (r)
#-----------------------------------------------------------------------------
calc_pearson <- function(obs, est) {
  cor(obs, est, method = "pearson", use = "complete.obs")
}

correl_tbl <- comparisons %>%
  mutate(`Correlação (r)` = map2_dbl(obs, model, ~ calc_pearson(dados[[.x]], dados[[.y]])))

#-----------------------------------------------------------------------------
# Tabelas por modelo
#-----------------------------------------------------------------------------
criar_tabela_cor <- function(df, modelo_nome) {
  tabela <- df %>%
    filter(str_detect(model, modelo_nome)) %>%
    select(Variável = variavel, Modelo = model, `Correlação (r)`) %>%
    gt() %>%
    tab_header(
      title = md(paste0("**Análise de Correlações — Modelo ", modelo_nome, " (2023)**")),
      subtitle = "Coeficiente de Correlação de Pearson (r) entre valores observados e previstos"
    ) %>%
    fmt_number(columns = `Correlação (r)`, decimals = 3, dec_mark = ",", sep_mark = ".") %>%
    tab_source_note(source_note = md("Fonte: Estação meteorológica de Indaiatuba  \nModelos de previsão: BRAMS, ETA e WRF.")) %>%
    tab_options(
      table.font.size = 13,
      data_row.padding = px(2),
      column_labels.padding = px(2),
      table.width = pct(55),
      heading.align = "center"
    ) %>%
    cols_width(
      Variável ~ px(150),
      Modelo ~ px(120),
      `Correlação (r)` ~ px(120)
    ) %>%
    cols_align(align = "center", columns = everything())
  
  output_file <- file.path(resultados_path, paste0("Tabela_Correlacao_", modelo_nome, ".png"))
  gtsave(tabela, output_file, vwidth = 700, vheight = 480)
}

criar_tabela_cor(correl_tbl, "WRF")
criar_tabela_cor(correl_tbl, "ETA")
criar_tabela_cor(correl_tbl, "BRAMS")

#-----------------------------------------------------------------------------
# Gráfico 1 - Barras comparativas
#-----------------------------------------------------------------------------
correl_long <- correl_tbl %>%
  mutate(Modelo = case_when(
    str_detect(model, "WRF") ~ "WRF",
    str_detect(model, "ETA") ~ "ETA",
    str_detect(model, "BRAMS") ~ "BRAMS"
  ))

grafico_barras_cor <- ggplot(correl_long, aes(x = Modelo, y = `Correlação (r)`, fill = Modelo)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~variavel, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c("BRAMS" = "#F8766D", "ETA" = "#00BA38", "WRF" = "#619CFF")) +
  labs(
    title = "Coeficiente de Correlação de Pearson (r) — Modelos de Previsão (2023)",
    subtitle = "Comparação entre BRAMS, ETA e WRF para Precipitação, Temperatura e Umidade Relativa",
    x = "Modelo de Previsão",
    y = "Correlação de Pearson (r)",
    caption = "Fonte: Estação meteorológica de Indaiatuba  \nModelos: BRAMS, ETA e WRF."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

ggsave(
  filename = file.path(resultados_path, "Grafico_Barras_Correlacao_Modelos.png"),
  plot = grafico_barras_cor,
  width = 10,
  height = 6,
  dpi = 300
)

#-----------------------------------------------------------------------------
# Gráfico 3 — Dispersão (Observado vs Previsto) — Centralizado e Ajustado
#-----------------------------------------------------------------------------

variaveis <- list(
  Temperatura = list(obs = "Temp_OBS", modelos = c("Temp_WRF", "Temp_ETA", "Temp_BRAMS")),
  Umidade = list(obs = "UR_OBS", modelos = c("UR_WRF", "UR_ETA", "UR_BRAMS")),
  Precipitação = list(obs = "Prec_OBS", modelos = c("Prec_WRF", "Prec_ETA", "Prec_BRAMS"))
)

graficos <- list()

for (var in names(variaveis)) {
  info <- variaveis[[var]]
  
  dados_plot <- dados %>%
    select(Data, OBS = all_of(info$obs), all_of(info$modelos)) %>%
    pivot_longer(cols = all_of(info$modelos),
                 names_to = "Modelo", values_to = "Previsao") %>%
    mutate(Modelo = str_remove(Modelo, "Temp_|UR_|Prec_"))
  
  grafico <- ggplot(dados_plot, aes(x = OBS, y = Previsao, color = Modelo)) +
    geom_point(alpha = 0.7, size = 2.4) +
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "gray40") +
    coord_equal() +
    scale_color_manual(values = c("BRAMS" = "#F8766D", "ETA" = "#00BA38", "WRF" = "#619CFF")) +
    labs(
      title = paste("Correlação —", var),
      x = paste(var, "Observada"),
      y = paste(var, "Prevista"),
      color = "Modelo"
    ) +
    theme_minimal(base_size = 12.5) +
    theme(
      plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
      axis.title.x = element_text(margin = margin(t = 6)),
      axis.title.y = element_text(margin = margin(r = 6)),
      legend.position = "none",
      plot.margin = margin(8, 6, 8, 6)
    )
  
  graficos[[var]] <- grafico
}

# Combina os três gráficos horizontalmente com espaçamento reduzido e tamanhos iguais
grafico_combinado <- wrap_plots(
  graficos$Temperatura, graficos$Umidade, graficos$Precipitação,
  ncol = 3,
  widths = c(1.1, 1.1, 1.1)  # ligeiramente maiores
) +
  plot_layout(guides = "collect", widths = c(1, 1, 1)) &
  theme(legend.position = "bottom")

# Adiciona títulos e legenda centralizada
grafico_final <- grafico_combinado +
  plot_annotation(
    title = "Correlação entre Valores Observados e Previstos — Modelos BRAMS, ETA e WRF (2023)",
    subtitle = "Análise comparativa da relação linear entre observações e previsões de Temperatura, Umidade e Precipitação",
    caption = "Fonte: Estação meteorológica de Indaiatuba",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 6)),
      plot.caption = element_text(size = 9, hjust = 1),
      plot.margin = margin(6, 6, 6, 6)
    )
  )

# Exibir
print(grafico_final)

# Salvar imagem com dimensões ajustadas e espaçamento menor
ggsave(
  filename = file.path(resultados_path, "Grafico_Correlacao_Combinado_Ajustado.png"),
  plot = grafico_final,
  width = 13.5,   # ligeiramente mais largo
  height = 5,     # um pouco mais alto
  dpi = 300
)

