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
    mutate(`registry type` = substring(info, 1,1),
           `subscription type` = substring(info,2,3),
           `subscription number` = substring(info,4,17),
           `bank branch` = substring(info,18,21),
           `zeros` = substring(info,22,23),
           `account number` = substring(info,24,28),
           `DAC` = substring(info,29,29),
           `whites 1` = substring(info,30,33),
           `Instruction/Alegation` = substring(info,34,37),
           `Companie use` = substring(info,38,62),
           `Our number` = substring(info,63,70),
           `Qnt. of currency` = substring(info,71,83),
           `Wallet number` = substring(info,84,86), 
           `Bank use` = substring(info,87,107),
           `Wallet` = substring(info,108,108),
           `Ocorrency code` = substring(info,109,110),
           `Document number` = substring(info,111,120),
           `Due date` = dmy(substring(info,121,126)),
           `Nominal value` = round(as.numeric(substring(info,127,139))/100,2),
           `Bank code` = substring(info,140,142),
           `Collection branch` = substring(info,143,147),
           `Specie` = substring(info,148,149),
           `Agree` = substring(info,150,150),
           `Emission date` = dmy(substring(info,151,156)),
           `Instruction 1` = substring(info,157,158),
           `Instruction 2` = substring(info,159,160),
           `1 day interest` = substring(info,161,173),
           `Discount until` = substring(info,174,179),
           `Discount value` = substring(info,180,192),
           `IOF Value` = substring(info, 193,205),
           `Deduction` = substring(info,206,218),
           `Subscription code (buyer)` = substring(info,219,220),
           `Subscription number (buyer)` = substring(info,221,234),
           `Name (buyer)` = substring(info,235,264),
           `Whites 2` = substring(info,265,274),
           `Address` = substring(info,275,314),
           `Neighborhood` = substring(info,315,326),
           `ZIP code` = substring(info,327,334),
           `City` = substring(info,335,349),
           `State` = substring(info,350,351),
           `Sacador/Avalista` = substring(info,352,381),
           `Whites 3` = substring(info,382,385),
           `Late fees date` = dmy(substring(info,386,391)),
           `Term (time)` = substring(info,392,393),
           `Whites 4` = substring(info,394,394),
           `Sequential number` = substring(info,395,400)) %>%
    dplyr::select(-info) -> cnab
  
  
  
  return(cnab)
} 



cnab <- ajustar_cnab_file()