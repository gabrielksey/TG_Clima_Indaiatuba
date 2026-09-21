# Título: Script 01 - Preparação e Unificação de Dados Climáticos
# Autor: Gabriel de Lima Marins
# Data: 17/09/2025
# Descrição: Este script carrega os dados climáticos de diferentes fontes (modelos e observações),
#            realiza a limpeza, padroniza as datas e une tudo em um único dataframe para análise.

#-----------------------------------------------------------------------------
# PASSO 0: CARREGAR PACOTES NECESSÁRIOS
#-----------------------------------------------------------------------------
# Certifique-se de que os pacotes estão instalados com: install.packages("tidyverse")
library(tidyverse) # Um conjunto de pacotes para ciência de dados (inclui dplyr, readr, etc.)
library(lubridate) # Pacote para facilitar o trabalho com datas e horas

#-----------------------------------------------------------------------------
# PASSO 1: DEFINIR CAMINHOS DOS ARQUIVOS CSV
#-----------------------------------------------------------------------------
# Defina o caminho para a pasta onde seus dados estão armazenados
data_path <- "C:/Users/gabri/OneDrive/Documents/TG_Clima_Indaiatuba/Dados/"

# Crie os caminhos completos para cada arquivo
path_wrf <- file.path(data_path, "resumo_diario_WRF.csv")
path_eta <- file.path(data_path, "resumo_diario_ETA.csv")
path_brams <- file.path(data_path, "resumo_diario_BRAMS.csv")
path_obs <- file.path(data_path, "dados_indaiatuba_2023_v2.csv")

#-----------------------------------------------------------------------------
# PASSO 2: LER E PREPARAR OS DADOS DOS MODELOS (WRF, ETA, BRAMS)
#-----------------------------------------------------------------------------
# Os arquivos dos modelos já possuem um formato de data padrão (YYYY-MM-DD),
# então podemos lê-los e converter a coluna 'Data' para o tipo Date.

dados_WRF <- read_csv(path_wrf, show_col_types = FALSE) %>%
  mutate(Data = as.Date(Data))

dados_ETA <- read_csv(path_eta, show_col_types = FALSE) %>%
  mutate(Data = as.Date(Data))

dados_BRAMS <- read_csv(path_brams, show_col_types = FALSE) %>%
  mutate(Data = as.Date(Data))

#-----------------------------------------------------------------------------
# PASSO 3: LER E PREPARAR OS DADOS OBSERVADOS
#-----------------------------------------------------------------------------
# O arquivo de dados observados requer uma atenção especial para padronizar a coluna de data.

# Mapeamento para converter abreviações de meses de Português para Inglês
month_map <- c(
  "jan" = "Jan", "fev" = "Feb", "mar" = "Mar", "abr" = "Apr",
  "mai" = "May", "jun" = "Jun", "jul" = "Jul", "ago" = "Aug",
  "set" = "Sep", "out" = "Oct", "nov" = "Nov", "dez" = "Dec"
)

dados_OBS <- read_csv(path_obs, show_col_types = FALSE) %>%
  # A função distinct() remove linhas duplicadas, como a entrada repetida para "30/nov"
  distinct() %>%
  # Padroniza a coluna 'Data'
  mutate(
    # Extrai o dia e a abreviação do mês da coluna original
    dia = str_extract(Data, "^\\d+"),
    mes_pt = str_extract(Data, "(?<=/)[a-z]+"),
    
    # Converte a abreviação do mês para o padrão em inglês usando o mapeamento
    mes_en = str_replace_all(mes_pt, month_map),
    
    # Cria uma string de data completa (assumindo que os dados são de 2023)
    date_str = paste(dia, mes_en, "2023", sep = "-"),
    
    # Converte a string para o formato Date
    Data = dmy(date_str)
  ) %>%
  # Seleciona apenas as colunas necessárias para a análise
  select(Data, Temp_OBS, UR_OBS, Prec_OBS)

#-----------------------------------------------------------------------------
# PASSO 4: UNIR TODOS OS DADOS EM UM ÚNICO DATAFRAME (VERSÃO DIRETA)
#-----------------------------------------------------------------------------
# Lista dos dataframes dos modelos para a junção
lista_dados_modelos <- list(dados_WRF, dados_ETA, dados_BRAMS)

# Une os dataframes dos modelos e o de dados observados em um único comando.
dados_finais <- lista_dados_modelos %>%
  reduce(full_join, by = "Data") %>%
  full_join(dados_OBS, by = "Data") %>%
  arrange(Data) # Opcional: Ordena o dataframe final por data

#-----------------------------------------------------------------------------
# PASSO 5: VERIFICAR E SALVAR OS DADOS PREPARADOS
#-----------------------------------------------------------------------------
# Exibe as primeiras linhas do dataframe final para verificação
print("Amostra dos dados finais:")
head(dados_finais)

# Exibe um resumo estatístico das colunas para uma verificação rápida
print("Resumo dos dados finais:")
summary(dados_finais)

# Opcional: Salvar o dataframe consolidado em um novo arquivo CSV para uso futuro
write_csv(dados_finais, file.path(data_path, "dados_consolidados_2023.csv"))