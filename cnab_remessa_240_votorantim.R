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
    mutate(`Código do Banco na Compensação` = substring(info1,1,3),
           `Lote de Serviço` = substring(info1, 4,7),
           `Tipo de Registro` = substring(info1, 8,8),
           # n.seq.registro.lote = substring(info1, 9,13),
           # cod.seg.registro = substring(info1, 14, 14),
           # uso.exclu.febran = substring(info1, 15,15),
           `Código de Movimento Remessa` = substring(info1, 16,17),
           `Agência Mantenedora da Conta` = substring(info1, 18,22),
           `Dígito Verificador da Agência` = substring(info1, 23, 23),
           `Número da Conta Corrente` = substring(info1, 24, 35),
           #`Dígito Verificador da Conta` = substring(info1, 36, 36),
           #`Dígito Verificador da Ag/Conta` = substring(info1, 37, 37),
           zeros0 = substring(info1, 38, 47),
           `Identificação do Título no Banco` = substring(info1, 48, 57),
           # codigo.carteira = substring(info1, 58, 58),
           # forma.cadastro.titulo = substring(info1, 59, 59),
           # tipo.documento = substring(info1, 60, 60),
           # ident.emissao.bloqueto = substring(info1, 61, 61),
           # ident.distribuicao = substring(info1, 62, 62),
           `Número do Documento de Cobrança` = substring(info1, 63, 72),
           # brancos = substring(info1, 73, 77),
           `Data de Vencimento do Título` = dmy(substring(info1, 78, 85)),
           `Valor Nominal do Título` = round(as.numeric(substring(info1,86,100))/100,2),
           # ag.cobradora = substring(info1, 101, 105),
           # dv = substring(info1, 106, 106),
           `Espécie do Título` = substring(info1, 107, 108),
           `Identific. de Título Aceito/Não Aceito` = substring(info1, 109, 109),
           `Data da Emissão do Título` = dmy(substring(info1, 110, 117)),
           `Código do Juros de Mora` = substring(info1, 118, 118),
           `Data do Juros de Mora` = dmy(substring(info1, 119, 126)),
           `Juros de Mora por Dia/Taxa` = as.numeric(substring(info1, 127, 141))/100,
           `Código do Desconto 1` = substring(info1, 142, 142),
           `Data do Desconto 1` = substring(info1, 143, 150),
           `Valor/Percentual a ser Concedido` = substring(info1, 151, 165),
           `Valor do IOF a ser Recolhido` = substring(info1, 166, 180),
           `Valor do Abatimento` = substring(info1, 181, 195),
           `Identificação do Título na Empresa` = substring(info1, 196, 220),
           `Código para Protesto` = substring(info1, 221, 221),
           `Número de Dias para Protesto` = substring(info1, 222, 223),
           `Código para Baixa/Devolução` = substring(info1, 224, 224),
           `Número de Dias para Baixa/Devolução` = substring(info1, 225, 227),
           `Código da Moeda` = substring(info1, 228, 229),
           `Nº do Contrato da Operação de Créd.` = substring(info1, 230, 239),
           `Uso Exclusivo FEBRABAN/CNAB` = substring(info1, 240, 240),
           # banco = substring(info2, 1,3),
           # lote = substring(info2, 4,7),
           # registro = substring(info2, 8,8),
           # num.registro = substring(info2,9,13),
           # cod.seq.lote = substring(info2, 14, 14),
           # cnab.febraban2 = substring(info2, 15, 15),
           # cod.mov.remessa2 = substring(info2, 16, 17),
           `Tipo de Inscrição` = substring(info2, 18,18),
           `Número de Inscrição` = substring(info2, 19, 33),
           Nome = substring(info2, 34, 73),
           Endereço = substring(info2, 74, 113),
           Bairro = substring(info2, 114, 128),
           CEP = substring(info2, 129, 133),
           `Sufixo do CEP` = substring(info2, 134, 136),
           Cidade = substring(info2, 137, 151),
           UF = substring(info2, 152, 153),
           `Tipo de Inscrição` = substring(info2, 154, 154),
           `Número de Inscrição` = substring(info2, 155, 169),
           `Nome do Sacador/Avalista` = substring(info2, 170, 209),
           #  nosso.numero.banco.corresp = substring(info2, 213, 232),
           #   cnab.febraban3 = substring(info2, 233, 240),
           `Cód. Bco. Corresp. na Compensação` = substring(info2, 210, 212),
           `Código de multa` = substring(info3, 66, 66),
           `Data de multa` = dmy(substring(info3, 67, 74)),
           `Valor da multa` =  round(as.numeric(substring(info3,75,89))/100,2)
           ) %>% 
    select(-c("info1", "info2", "info3")) -> cnab
  
  return(cnab)
  
  
}

cnab_remessa_ot <- parse_cnab240()
