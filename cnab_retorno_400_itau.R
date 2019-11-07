# function to parse CNAB 400 retonro file into dataframe.
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
  # | Unsupported extension ‘ret’ of file ____.RET treating as plain text.                |
  # | *. _ failed to parse.  (possible)                                                   |
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
           `Brancos 1` = substring(info,30,37),
           `Uso da empresa` = substring(info,38,62),
           `Nosso número (Nota 2)` = substring(info,63,70),
           `Brancos 2` = substring(info,71,82),
           `Núm. da carteira` = substring(info,83,85),
           `Nosso número (Nota 3)` = substring(info,86,93),
           `DAC Nosso número` = substring(info,94,94),
           `Brancos 3` = substring(info,95,107),
           `Carteira` = substring(info,108,108),
           `Cód. de ocorrência` = substring(info,109,110),
           `Data de ocorrência` = dmy(substring(info,111,116)),
           `Núm. do documento` = substring(info,117,126),
           `Nosso número (Confirmação)` = substring(info,127,134),
           `Brancos 4` = substring(info,135,146),
           `Vencimento do título` = dmy(substring(info,147,152)),
           `Valor do título` = round(as.numeric(substring(info,153,165))/100,2),
           `Código do banco` = substring(info,166,168),
           `Agência cobradora` = substring(info,169,172),
           `DAC Agência cobradora` = substring(info,173,173),
           `Espécie` = substring(info,174,175),
           `Tarifa de cobrança` = substring(info,176,188),
           `Brancos 5` = substring(info,189,214),
           `Valor do IOF` = substring(info, 215,227),
           `Valor do abatimento` = substring(info,228,240),
           `Descontos` = substring(info,241,253),
           `Valor principal` = round(as.numeric(substring(info,254,266))/100,2),
           `Juros de mora/multa` = substring(info,267,279),
           `Outros créditos` = substring(info,280,292),
           `Boleto DDA` = substring(info,293,293),
           `Brancos 6` = substring(info,294,295),
           `Data crédito` = dmy(substring(info,296,301)),
           `Instrução cancelada` = substring(info,302,305),
           `Brancos 7` = substring(info,306,311),
           `Zeros` = substring(info,312,324),
           `Nome (sacado)` = substring(info,325,354),
           `Brancos 8` = substring(info,355,377),
           `Erros/Msg Informativa` = substring(info,378,385),
           `Brancos 9` = substring(info,386,392),
           `Código de liquidação` = substring(info,393,394),
           `Número Sequencial` = substring(info,395,400)) %>%
    dplyr::select(-info) -> cnab
  
  
  
  return(cnab)
} 



cnab <- ajustar_cnab_file()