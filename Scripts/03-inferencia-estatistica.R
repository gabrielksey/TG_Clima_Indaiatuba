#-----------------------------------------------------------------------------
# Script 03 - Inferência Estatística (Teste de Wilcoxon pareado - aproximação normal)
# Autor: Gabriel de Lima Marins
# Data: 30/09/2025
#-----------------------------------------------------------------------------

library(tidyverse)
library(gt)
library(stats)
library(webshot2)  # necessário para salvar a tabela em PNG

# Caminhos
data_path <- "C:/Users/gabri/OneDrive/Documents/TG_Clima_Indaiatuba/Dados/"
resultados_path <- "C:/Users/gabri/OneDrive/Documents/TG_Clima_Indaiatuba/Resultados/"
path_consolidado <- file.path(data_path, "dados_consolidados_2023.csv")

# Carregar dados
dados <- read_csv(path_consolidado, show_col_types = FALSE)

# Função para calcular estatística Z do Wilcoxon
wilcoxon_large_paired <- function(df, obs_col, model_col, continuity = FALSE) {
  obs <- df[[obs_col]]
  mod <- df[[model_col]]
  dif <- mod - obs
  
  nonzero_idx <- which(!is.na(dif) & dif != 0)
  dif_nz <- dif[nonzero_idx]
  n <- length(dif_nz)
  if (n == 0) return(NULL)
  
  abs_dif <- abs(dif_nz)
  postos <- rank(abs_dif, ties.method = "average")
  T_pos <- sum(postos[dif_nz > 0])
  
  expected_T <- n * (n + 1) / 4
  sd_T <- sqrt(n * (n + 1) * (2 * n + 1) / 24)
  cc <- ifelse(continuity, 0.5 * sign(T_pos - expected_T), 0)
  Z <- (T_pos - expected_T - cc) / sd_T
  p_value <- 2 * (1 - pnorm(abs(Z)))
  
  tie_flag <- any(duplicated(abs_dif))
  
  wt <- tryCatch(
    wilcox.test(mod, obs, paired = TRUE, exact = FALSE, correct = FALSE),
    error = function(e) NULL
  )
  
  tibble(
    Observado = obs_col,
    Modelo = model_col,
    n = n,
    T_pos = T_pos,
    Esperado = expected_T,
    DP = sd_T,
    Z = Z,
    `p-valor (Z)` = p_value,
    Ties = tie_flag,
    `W (wilcox)` = if (!is.null(wt)) wt$statistic else NA_real_,
    `p-valor (wilcox)` = if (!is.null(wt)) wt$p.value else NA_real_
  )
}

# Comparações
comparisons <- tribble(
  ~obs,        ~model,
  "Temp_OBS",  "Temp_WRF",
  "Temp_OBS",  "Temp_ETA",
  "Temp_OBS",  "Temp_BRAMS",
  "UR_OBS",    "UR_WRF",
  "UR_OBS",    "UR_ETA",
  "UR_OBS",    "UR_BRAMS",
  "Prec_OBS",  "Prec_WRF",
  "Prec_OBS",  "Prec_ETA",
  "Prec_OBS",  "Prec_BRAMS"
) %>%
  filter(obs %in% names(dados), model %in% names(dados))

# Rodar testes
results_tbl <- map2_df(comparisons$obs, comparisons$model,
                       ~ wilcoxon_large_paired(dados, .x, .y))

# Função para criar e salvar tabela de cada modelo
criar_tabela_modelo <- function(dados, modelo_nome) {
  tabela <- dados %>%
    filter(str_detect(Modelo, modelo_nome)) %>%
    gt() %>%
    tab_header(
      title = md(paste0("**Teste de Wilcoxon — Modelo ", modelo_nome, " (2023)**")),
      subtitle = "Comparação entre Observações (OBS) e previsões do modelo"
    ) %>%
    fmt_number(
      columns = c(n, T_pos, Esperado, DP, Z, `p-valor (Z)`, `W (wilcox)`, `p-valor (wilcox)`),
      decimals = 3, dec_mark = ",", sep_mark = "."
    ) %>%
    tab_source_note(source_note = "Fonte: Dados da estação meteorológica de Indaiatuba e modelos BRAMS, ETA e WRF.")
  
  # 🔧 Ajuste da largura/altura para não cortar legenda
  output_file <- file.path(resultados_path, paste0("Tabela_Wilcoxon_", modelo_nome, ".png"))
  gtsave(tabela, output_file, vwidth = 1800, vheight = 1000)
}

# Criar e salvar tabelas separadas
criar_tabela_modelo(results_tbl, "WRF")
criar_tabela_modelo(results_tbl, "ETA")
criar_tabela_modelo(results_tbl, "BRAMS")

#-----------------------------------------------------------------------------
# Gráfico 2: Boxplots comparativos das variáveis observadas e previstas
#-----------------------------------------------------------------------------

# Reorganizar os dados para formato longo
dados_long <- dados %>%
  select(Prec_OBS, Prec_BRAMS, Prec_ETA, Prec_WRF,
         Temp_OBS, Temp_BRAMS, Temp_ETA, Temp_WRF,
         UR_OBS, UR_BRAMS, UR_ETA, UR_WRF) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variavel", "Modelo"),
    names_pattern = "(.*)_(.*)",
    values_to = "Valor"
  ) %>%
  mutate(
    Variavel = recode(Variavel,
                      "Prec" = "Precipitação",
                      "Temp" = "Temperatura",
                      "UR" = "Umidade Relativa"),
    Modelo = recode(Modelo,
                    "OBS" = "Observado",
                    "BRAMS" = "BRAMS",
                    "ETA" = "ETA",
                    "WRF" = "WRF"),
    Modelo = factor(Modelo, levels = c("Observado", "BRAMS", "ETA", "WRF"))
  )

# 🔧 Novo gráfico com legenda e cores definidas
grafico_boxplot <- ggplot(dados_long, aes(x = Modelo, y = Valor, fill = Modelo)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.75, color = "black") +
  facet_wrap(~Variavel, scales = "free_y", ncol = 1) +
  scale_fill_manual(
    name = "Fonte de Dados",
    values = c(
      "Observado" = "#FFD700",
      "BRAMS" = "#F8766D",
      "ETA" = "#00BA38",
      "WRF" = "#619CFF"
    ),
    labels = c("Observado", "BRAMS", "ETA", "WRF")
  ) +
  labs(
    title = "Comparação entre Dados Observados e Modelos de Previsão (2023)",
    subtitle = "Distribuição das variáveis Precipitação, Temperatura e Umidade Relativa — Modelos BRAMS, ETA, WRF e Observado",
    x = "Fonte de Dados",
    y = "Valor da Variável",
    caption = "Fonte: Dados observados da estação meteorológica de Indaiatuba e previsões dos modelos BRAMS, ETA e WRF."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(fill = "white", color = "gray80")
  )

# Exibir no Viewer
print(grafico_boxplot)

# Salvar gráfico final
ggsave(
  filename = file.path(resultados_path, "Grafico_Boxplot_Modelos_vs_Observado.png"),
  plot = grafico_boxplot,
  width = 10,
  height = 10,
  dpi = 300
)
