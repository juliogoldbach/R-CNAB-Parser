# function to parse CNAB 240 retorno file into dataframe.
#
# github: @juliogoldbach
#
# janitor package can be used to clean column names, if desired.
# check: https://github.com/sfirke/janitor
# 
# fields displayed based on authoritarian decision :) 

rm(list = ls())
library(readtext)
library(tidyverse)
library(lubridate)
library(svDialogs)
options(digits = 10)


parse_cnab240_ret <- function() {
  # Expected return for this function:
  # ---------------------------------------------------------------------------------------
  # | Warning messages:                                                                   |
  # | 1: In get_source(x, text_field = text_field, encoding = e, source = source,  :      |
  # | Unsupported extension ‘rem’ of file ____.RET treating as plain text.                |
  # | *. _ failed to parse.  (possible)                                                   |
  # ---------------------------------------------------------------------------------------
  
  
  first <- readtext(dlg_open(title = "Upload CNAB", filters = dlg_filters[c("All"), ])$res)[2]
  temp_df <- as.data.frame(strsplit(first$text,"\n"))
  names(temp_df)[1] <- paste("info")
  temp_df <- as_tibble(temp_df, skip = 1)[-1:-2,]
  
  cnab_T <- 
    temp_df[-seq(2, nrow(temp_df), by = 2),] %>%
    rename(cnab_T = info)
  
  cnab_U <- 
    temp_df[seq(2, nrow(temp_df), by = 2),]  %>%
    rename(cnab_U = info)
  
  cnab <- cbind(cnab_T, cnab_U)
  
  cnab %>% 
    mutate(   
            #------------------ Segmento T ------------------# 
           `Código do Banco na Compensação (T)` = substring(cnab_T,1,3),
           `Lote de Serviço (T)` = substring(cnab_T, 4,7),
           `Tipo de Registro (T)` = substring(cnab_T, 8,8),
           `Número Sequencial Registro no Lote (T)` = substring(cnab_T, 9,13),
           `Código Segmento do Registro Detalhe (T)` = substring(cnab_T, 14, 14),
           `Uso Exclusivo FEBRABAN/CNAB (T)` = substring(cnab_T, 15,15),
           `Código de Movimento Retorno (T)` = substring(cnab_T, 16,17),
           `Agência Mantenedora da Conta (T)` = substring(cnab_T, 18,22),
           `Dígito Verificador da Agência (T)` = substring(cnab_T, 23, 23),
           `Número da Conta Corrente (T)` = substring(cnab_T, 24, 35),
           `Dígito Verificador da Conta (T)` = substring(cnab_T, 36, 36),
           `Dígito Verificador da Ag/Conta (T)` = substring(cnab_T, 37, 37),
           `Identificação do Título no Banco (T)` = substring(cnab_T, 48, 57),
           `Código da Carteira (T)` = substring(cnab_T, 58, 58),
           `Número do Documento de Cobrança (T)` = substring(cnab_T, 59, 68),
           `Data do Vencimento do Título (T)` = dmy(substring(cnab_T, 74, 81)),
           `Valor Nominal do Título (T)` = round(as.numeric(substring(cnab_T, 82, 96))/100,2),
           `Número do Banco (T)` = substring(cnab_T, 97, 99),
           `Agência Cobradora/Recebedora (T)` = substring(cnab_T, 100, 104),
           `Dígito Verificador da Agência Cobradora/Recebedora(T)` = substring(cnab_T, 105, 105),
           `Identificação do Título na Empresa (T)` = substring(cnab_T, 106, 130),
           `Código da Moeda (T)` = substring(cnab_T, 131, 132),
           `Sacado - Tipo de Inscrição (T)` = substring(cnab_T, 133, 133),
           `Sacado - Número de Inscrição (T)` = substring(cnab_T, 134, 148),
           `Sacado - Nome (T)` = substring(cnab_T, 149, 188),
           `Nº do Contr. da Operação de Crédito (T)` = substring(cnab_T, 199, 213),
           `Identificação para Rejeições, Tarifas, Custas, Liquidação e Baixas (T)` = substring(cnab_T, 214, 223),
           `Identifica se o Pagador é aderente ao DDA (T)` = substring(cnab_T, 224, 224),
           `Uso Exclusivo FEBRABAN/CNAB (T)` = substring(cnab_T, 225, 240),
           #------------------ Segmento U ------------------# 
           `Juros / Multa / Encargos (U)` = substring(cnab_U, 18,32),
           `Valor do Desconto Concedido (U)` = substring(cnab_U, 33, 47),
           `Valor do Abat. Concedido/Cancel (U)` = substring(cnab_U, 48, 62),
           `Valor do IOF Recolhido (U)` = substring(cnab_U, 63, 77),
           `Valor Pago pelo Sacado (U)` = substring(cnab_U, 78, 92),
           `Valor Líquido a ser Creditado (U)` = substring(cnab_U, 93, 107),
           `Valor de Outras Despesas (U)` = substring(cnab_U, 108, 122),
           `Valor de Outros Créditos (U)` = substring(cnab_U, 123, 137),
           `Data da Ocorrência (U)` = dmy(substring(cnab_U, 138, 145)),
           `Data da Efetivação do Crédito (U)` = substring(cnab_U, 146, 153),
           `Ocorr. do Sacado - Código da Ocorrência (U)` = substring(cnab_U, 154, 157),
           `Ocorr. do Sacado - Data da Ocorrência (U)` = substring(cnab_U, 158, 165),
           `Ocorr. do Sacado - Valor da Ocorrência (U)` = substring(cnab_U, 166, 180),
           `Ocorr. do Sacado - Complem. da Ocorrência (U)` = substring(cnab_U, 181, 210),
           `Cód. Bco. Corresp. na Compensação (U)` = substring(cnab_U, 211, 213),
           `Nosso Nº Banco Correspondente (U)` = substring(cnab_U, 214, 233),
           `Uso Exclusivo FEBRABAN/CNAB (U)` = substring(cnab_U, 234, 240)
           ) %>% 
    select(-c("cnab_T", "cnab_U")) -> cnab
  
  return(cnab)
  
  
}

cnab <- parse_cnab240_ret()
