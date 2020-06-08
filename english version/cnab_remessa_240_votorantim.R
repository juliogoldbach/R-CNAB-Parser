# function to parse CNAB 240 remessa file into dataframe.
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


parse_cnab240 <- function() {
  # Expected return for this function:
  # ---------------------------------------------------------------------------------------
  # | Warning messages:                                                                   |
  # | 1: In get_source(x, text_field = text_field, encoding = e, source = source,  :      |
  # | Unsupported extension ‘rem’ of file ____.REM treating as plain text.                |
  # ---------------------------------------------------------------------------------------
  
  
  first <- readtext(dlg_open(title = "Upload CNAB", filters = dlg_filters[c("All"), ])$res)[2]
  temp_df <- as.data.frame(strsplit(first$text,"\n"))
  names(temp_df)[1] <- paste("info")
  temp_df <- as_tibble(temp_df, skip = 1)[-1:-2,]
  new <- temp_df[-nrow(temp_df)+1:-nrow(temp_df),]
  
  
  cnab <- cbind(
    new[seq(1,nrow(new), 3),] %>% mutate(info1 = info),
                cbind(new[seq(2,nrow(new), 3),] %>% mutate(info2 = info),
                      new[seq(3,nrow(new), 3),] %>% mutate(info3 = info))
  )[,c(2,4,6)]
  

  cnab %>% 
    mutate(`Bank code` = substring(info1,1,3),
           `Service lot` = substring(info1, 4,7),
           `Registry type` = substring(info1, 8,8),
           # n.seq.registro.lote = substring(info1, 9,13),
           # cod.seg.registro = substring(info1, 14, 14),
           # uso.exclu.febran = substring(info1, 15,15),
           `Moviment code of the remessa` = substring(info1, 16,17),
           `bank branch` = substring(info1, 18,22),
           `verification digit of branch` = substring(info1, 23, 23),
           `account number` = substring(info1, 24, 35),
           #`Dígito Verificador da Conta` = substring(info1, 36, 36),
           #`Dígito Verificador da Ag/Conta` = substring(info1, 37, 37),
           # zeros = substring(info1, 38, 47),
           `identification of the paymentslip at the bank` = substring(info1, 48, 57),
           # codigo.carteira = substring(info1, 58, 58),
           # forma.cadastro.titulo = substring(info1, 59, 59),
           # tipo.documento = substring(info1, 60, 60),
           # ident.emissao.bloqueto = substring(info1, 61, 61),
           # ident.distribuicao = substring(info1, 62, 62),
           `collections document number` = substring(info1, 63, 72),
           # brancos = substring(info1, 73, 77),
           `due date of the receivable` = dmy(substring(info1, 78, 85)),
           `nominal value` = round(as.numeric(substring(info1,86,100))/100,2),
           # ag.cobradora = substring(info1, 101, 105),
           # dv = substring(info1, 106, 106),
           `receivable specie` = substring(info1, 107, 108),
           `identification of receivable accepted/not aceppted` = substring(info1, 109, 109),
           `emission date` = dmy(substring(info1, 110, 117)),
           `late interest code` = substring(info1, 118, 118),
           `late interest date` = dmy(substring(info1, 119, 126)),
           `late interest by day/rate` = as.numeric(substring(info1, 127, 141))/100,
           `discount code 1` = substring(info1, 142, 142),
           `discount date 1` = substring(info1, 143, 150),
           `value/percent to be given` = substring(info1, 151, 165),
           `IOF to be collected` = substring(info1, 166, 180),
           `deduction value` = substring(info1, 181, 195),
           `identification of the receivable at the companie` = substring(info1, 196, 220),
           `code to protest` = substring(info1, 221, 221),
           `number of days until protest` = substring(info1, 222, 223),
           `code for closing/devolution` = substring(info1, 224, 224),
           `number of days for closing/devolution` = substring(info1, 225, 227),
           `currency code` = substring(info1, 228, 229),
           `credit contract number` = substring(info1, 230, 239),
           `exlclusive use FEBRABAN/CNAB` = substring(info1, 240, 240),
           # banco = substring(info2, 1,3),
           # lote = substring(info2, 4,7),
           # registro = substring(info2, 8,8),
           # num.registro = substring(info2,9,13),
           # cod.seq.lote = substring(info2, 14, 14),
           # cnab.febraban2 = substring(info2, 15, 15),
           # cod.mov.remessa2 = substring(info2, 16, 17),
           `type of subscription` = substring(info2, 18,18),
           `number of subscription` = substring(info2, 19, 33),
           name = substring(info2, 34, 73),
           address = substring(info2, 74, 113),
           neighborhood = substring(info2, 114, 128),
           zipcode = substring(info2, 129, 133),
           `zipcode sufix` = substring(info2, 134, 136),
           city = substring(info2, 137, 151),
           district = substring(info2, 152, 153),
           `type of subscription (other)` = substring(info2, 154, 154),
           `number of subscription (other)` = substring(info2, 155, 169),
           `name of sacador/avalista` = substring(info2, 170, 209),
           #  nosso.numero.banco.corresp = substring(info2, 213, 232),
           #   cnab.febraban3 = substring(info2, 233, 240),
           `bank code at compensation` = substring(info2, 210, 212),
           `multa  code` = substring(info3, 66, 66),
           `multa  date` = dmy(substring(info3, 67, 74)),
           `multa  value` =  round(as.numeric(substring(info3,75,89))/100,2)
           ) %>% 
    select(-c("info1", "info2", "info3")) -> cnab
  
  return(cnab)
  
  
}

cnab <- parse_cnab240()