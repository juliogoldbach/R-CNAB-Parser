# function to parse CNAB 400 remessa file into dataframe.
# made based on Bradesco documentation: 
# https://banco.bradesco/assets/pessoajuridica/pdf/4008-524-0121-layout-cobranca-versao-portugues.pdf
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
  
  #todo: finish translating
  col.names <- c("registry identification", "debt agency (optional)", "debt agency digit (optional)", "account reason (optional)", "account number (optional)",
                 "account digit (optional)", "identification of the beneficiary companie at the bank", "control number of the participant", "Código do Banco a ser debitado na Câmara de Compensação",
                 "Campo de Multa", "Percentual de multa", "Identificação do Título no Banco", "Digito de Auto Conferencia do Número Bancário", "join_no_bv", "Desconto Bonificação por dia", "Condição para Emissão da Papeleta de Cobrança",
                 "Ident. se emite Boleto para Débito Automático", "Identificação da Operação do Banco", "Indicador Rateio Crédito (opcional)", "Endereçamento para Aviso do Débito Automático em Conta Corrente (opcional)",
                 "Quantidade possíveis de pagamento", "Identificação da ocorrência", "Num. do Documento", "Data do Vencimento do Título", "Valor do Título", "Banco Encarregado da Cobrança", "Agência Depositária",
                 "Espécie de Título", "Identificação", "Data da emissão do Título", "1a instrução", "2ª instrução", "Valor a ser cobrado por Dia de Atraso", "Data Limite P/Concessão de Desconto", "Valor do Desconto",
                 "Valor do IOF", "Valor do Abatimento a ser concedido ou cancelado", "Identificação do Tipo de Inscrição do Pagador", "Num. Inscrição do Pagador", "Nome do Pagador", "Endereço Completo",
                 "1a Mensagem", "CEP", "Sufixo do CEP", "Sacador/Avalista ou 2a Mensagem", "No Sequencial do Registro ")
  
  
  text_df %>%
    mutate(id.registro = substring(info, 1,1),
           ag.debito = substring(info,2,6),
           dig.ag.debito = substring(info,7,7),
           razao.cc = substring(info,8,12),
           cc = substring(info,13,19),
           dig.cc = substring(info,20,20),
           id.emp.benef.banco = substring(info,21,37),
           num.controle.participante = substring(info,38,62),
           cod.banco.debitado = substring(info,63,65),
           multa = substring(info,66,66),
           percentagem.multa = substring(info,67,70),
           id.titulo.banco = substring(info,71,81),
           digito.autoconf = substring(info,82,82), 
           join.on.bv = as.numeric(substring(info, 72,81)),
           desconto.bonif.dia = substring(info,83,92),
           condi.emissao.papeleta = substring(info,93,93),
           ident.if.emite.boleto = substring(info,94,94),
           ident.op.banco = substring(info,95,104),
           indic.rateio = substring(info,105,105),
           end.aviso.debito = substring(info,106,106),
           quant.possiveis.pag = substring(info,107,108),
           id.ocorrencia = substring(info,109,110),
           num.doc = substring(info,111,120),
           vencimento = dmy(substring(info,121,126)),
           valor = round(as.numeric(substring(info,127,139))/100,2),
           banco.cobranca = substring(info,140,142),
           ag.depositaria = substring(info,143,147),
           especie.de.titulo = substring(info,148,149),
           identificacao = substring(info,150,150),
           data.emissao.titulo = dmy(substring(info, 151,156)),
           first.instru = substring(info,157,158),
           second.instru = substring(info,159,160),
           valor.por.dia.atraso = substring(info,161,173),
           data.limite.credito = substring(info,174,179),
           valor.desconto = as.numeric(substring(info,180,192))/100,
           valor.iof = as.numeric(substring(info,193,205))/100,
           valor.abatimento = as.numeric(substring(info,206,218))/100,
           ident.tipo.inscricao = substring(info,219,220),
           num.insc.pagador = substring(info,221,234),
           nome.pagador = substring(info,235,274),
           end.completo = substring(info,275,314),
           first.msg = substring(info,315,326),
           cep = substring(info,327,331),
           sufix.cep = substring(info,332,334),
           sacador.avalista.secondmsg = substring(info,335,394),
           num.seq.registro = substring(info,395,400)) %>%
    dplyr::select(-info) -> cnab
  
  
  
  colnames(cnab) <- col.names
  return(cnab)
} 



cnab <- ajustar_cnab_file()
