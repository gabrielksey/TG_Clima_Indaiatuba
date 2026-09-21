#-----------------------------------------------------------------------------
# Script 02 - Estatística Descritiva (Tabelas no Viewer + Exportação PNG)
#-----------------------------------------------------------------------------

library(tidyverse)
library(gt)

# Caminhos
data_path <- "C:/Users/gabri/OneDrive/Documents/TG_Clima_Indaiatuba/Dados/"
resultados_path <- "C:/Users/gabri/OneDrive/Documents/TG_Clima_Indaiatuba/Resultados/"
path_consolidado <- file.path(data_path, "dados_consolidados_2023.csv")

# Carregar dados
dados_finais <- read_csv(path_consolidado, show_col_types = FALSE)

# Estatísticas
dados_para_tabela <- dados_finais %>%
  pivot_longer(cols = -Data, names_to = c(".value", "Fonte"), names_sep = "_") %>%
  group_by(Fonte) %>%
  summarise(
    Temp_Media = mean(Temp, na.rm = TRUE),
    Temp_Mediana = median(Temp, na.rm = TRUE),
    Temp_Variancia = var(Temp, na.rm = TRUE),
    Temp_DP = sd(Temp, na.rm = TRUE),
    Temp_Q1 = quantile(Temp, 0.25, na.rm = TRUE),
    Temp_Q3 = quantile(Temp, 0.75, na.rm = TRUE),
    Temp_P10 = quantile(Temp, 0.10, na.rm = TRUE),
    Temp_P90 = quantile(Temp, 0.90, na.rm = TRUE),
    Temp_Min = min(Temp, na.rm = TRUE),
    Temp_Max = max(Temp, na.rm = TRUE),
    
    UR_Media = mean(UR, na.rm = TRUE),
    UR_Mediana = median(UR, na.rm = TRUE),
    UR_Variancia = var(UR, na.rm = TRUE),
    UR_DP = sd(UR, na.rm = TRUE),
    UR_Q1 = quantile(UR, 0.25, na.rm = TRUE),
    UR_Q3 = quantile(UR, 0.75, na.rm = TRUE),
    UR_P10 = quantile(UR, 0.10, na.rm = TRUE),
    UR_P90 = quantile(UR, 0.90, na.rm = TRUE),
    UR_Min = min(UR, na.rm = TRUE),
    UR_Max = max(UR, na.rm = TRUE),
    
    Prec_Total = sum(Prec, na.rm = TRUE),
    Prec_Media = mean(Prec, na.rm = TRUE),
    Prec_Mediana = median(Prec, na.rm = TRUE),
    Prec_Variancia = var(Prec, na.rm = TRUE),
    Prec_DP = sd(Prec, na.rm = TRUE),
    Prec_Q1 = quantile(Prec, 0.25, na.rm = TRUE),
    Prec_Q3 = quantile(Prec, 0.75, na.rm = TRUE),
    Prec_P10 = quantile(Prec, 0.10, na.rm = TRUE),
    Prec_P90 = quantile(Prec, 0.90, na.rm = TRUE),
    Prec_Min = min(Prec, na.rm = TRUE),
    Prec_Max = max(Prec, na.rm = TRUE),
    Prec_Dias_Chuva = sum(Prec >= 1, na.rm = TRUE)
  ) %>%
  mutate(Fonte = fct_relevel(Fonte, "BRAMS", "ETA", "OBS", "WRF")) %>%
  arrange(Fonte)

# Função para criar e salvar tabela
criar_tabela <- function(dados, variavel, nome_arquivo) {
  colunas <- dados %>%
    select(Fonte, starts_with(variavel))
  
  nomes <- colnames(colunas)
  nomes[2:length(nomes)] <- gsub(paste0(variavel, "_"), "", nomes[2:length(nomes)])
  colnames(colunas) <- nomes
  
  tabela <- colunas %>%
    gt(rowname_col = "Fonte") %>%
    tab_stubhead(label = "Fonte") %>%
    tab_header(
      title = md(paste0("**Estatísticas Descritivas - ", variavel, " (2023)**")),
      subtitle = "Comparativo entre Dados Observados (OBS) e Modelos de Previsão"
    ) %>%
    fmt_number(columns = where(is.numeric), decimals = 2, dec_mark = ",", sep_mark = ".") %>%
    tab_source_note(source_note = "Fonte: Dados da estação meteorológica de Indaiatuba e modelos BRAMS, ETA e WRF.")
  
  # Caminho de saída
  output_file <- file.path(resultados_path, paste0(nome_arquivo, ".png"))
  
  # Salvar como PNG
  gtsave(tabela, output_file, vwidth = 1600, vheight = 900)
  
  return(tabela)
}

# Criar tabelas
tabela_temp <- criar_tabela(dados_para_tabela, "Temp", "Tabela_Temperatura")
tabela_ur   <- criar_tabela(dados_para_tabela, "UR", "Tabela_Umidade")
tabela_prec <- criar_tabela(dados_para_tabela, "Prec", "Tabela_Precipitacao")

# Exibir última tabela no Viewer
tabela_prec
