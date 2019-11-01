# function to parse CNAB 400 remessa file into dataframe.
# made based on Itaú documentation: 
# https://download.itau.com.br/bankline/layout_cobranca_400bytes_cnab_itau_mensagem.pdf
#
# github: @juliogoldbach
# 
# janitor package can be used to clean column names, if desired.
# check: https://github.com/sfirke/janitor

rm(list = ls())
library(readtext)
library(tidyverse)
library(lubridate)
library(svDialogs)
options(digits = 10)



ajustar_cnab_file <- function() {
  # Expected return for this function:
  # ---------------------------------------------------------------------------------------
  # | Warning messages:                                                                   |
  # | 1: In get_source(x, text_field = text_field, encoding = e, source = source,  :      |
  # | Unsupported extension ‘rem’ of file ____.REM treating as plain text.                |
  # ---------------------------------------------------------------------------------------
  
  
  file <- readtext(dlg_open(title = "Select one file", filters = dlg_filters[c("All"), ])$res)[2]
  text_df <- as.data.frame(strsplit(file$text,"\n"))
  names(text_df)[1] <- paste("info")
  text_df <- as_tibble(text_df, skip = 1)[-1,]
  text_df <- text_df[-nrow(text_df),]
  
  
  text_df %>%
    mutate(`Tipo de registro` = substring(info, 1,1),
           `Código de inscrição` = substring(info,2,3),
           `Número de inscrição` = substring(info,4,17),
           `Agência` = substring(info,18,21),
           `Zeros` = substring(info,22,23),
           `Conta` = substring(info,24,28),
           `DAC` = substring(info,29,29),
           `Brancos 1` = substring(info,30,33),
           `Instrução/Alegaçãp` = substring(info,34,37),
           `Uso da empresa` = substring(info,38,62),
           `Nosso número` = substring(info,63,70),
           `Qtde de moeda` = substring(info,71,83),
           `Núm. da carteira` = substring(info,84,86), 
           `Uso do banco` = substring(info,87,107),
           `Carteira` = substring(info,108,108),
           `Cód. de ocorrência` = substring(info,109,110),
           `Núm. do documento` = substring(info,111,120),
           `Vencimento` = substring(info,121,126),
           `Valor do título` = round(as.numeric(substring(info,127,139))/100,2),
           `Código do banco` = substring(info,140,142),
           `Agência cobradora` = substring(info,143,147),
           `Espécie` = substring(info,148,149),
           `Aceite` = substring(info,150,150),
           `Data da emissão` = dmy(substring(info,151,156)),
           `Instrução 1` = substring(info,157,158),
           `Instrução 2` = substring(info,159,160),
           `Juros de 1 dia` = substring(info,161,173),
           `Desconto até` = substring(info,174,179),
           `Valor do desconto` = substring(info,180,192),
           `Valor do IOF` = substring(info, 193,205),
           `Abatimento` = substring(info,206,218),
           `Código de inscrição (sacado)` = substring(info,219,220),
           `Número de inscrição (sacado)` = substring(info,221,234),
           `Nome (sacado)` = substring(info,235,264),
           `Brancos 2` = substring(info,265,274),
           `Logradouro` = substring(info,275,314),
           `Bairro` = substring(info,315,326),
           `CEP` = substring(info,327,334),
           `Cidade` = substring(info,335,349),
           `Estado` = substring(info,350,351),
           `Sacador/Avalista` = substring(info,352,381),
           `Brancos 3` = substring(info,382,385),
           `Data de mora` = dmy(substring(info,386,391)),
           `Prazo` = substring(info,392,393),
           `Brancos 4` = substring(info,394,394),
           `Número sequencial` = substring(info,395,400)) %>%
    dplyr::select(-info) -> cnab
  
  
  
  return(cnab)
} 



cnab <- ajustar_cnab_file()