% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculaDisponibilidadeHidro.R
\name{calculaDisponibilidadeHidro}
\alias{calculaDisponibilidadeHidro}
\title{Calcula a disponibilidade hidraulica para o Balanco de Potencia}
\usage{
calculaDisponibilidadeHidro(
  baseSQLite,
  pastaCaso,
  pastaSaidas,
  tipoCaso,
  numeroCaso,
  codModelo,
  codTucurui,
  flagVert = FALSE,
  flagUHE = FALSE,
  execShiny = FALSE
)
}
\arguments{
\item{baseSQLite}{nome e localizacao da base SQLite do balanco de ponta}

\item{pastaCaso}{localizacao da pasta com os arquivos do NEWAVE do caso a ser analisado no balanco de ponta}

\item{pastaSaidas}{localizacao dos arquivos de saida do modulo NWLISTOP e arquivos do SUISHI}

\item{tipoCaso}{valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica}

\item{numeroCaso}{valor inteiro com o numero do caso}

\item{codModelo}{valor inteiro com o codigo do modelo. 1:NEWAVE; 2:SUISHI}

\item{codTucurui}{codigo atribuido para a usina de Tucurui}

\item{flagVert}{booleano que indica se considera ou nao o vertimento para todas as UHE}

\item{flagUHE}{booleano que indica se as saidas individuais por UHE deverao ou nao ser gravadas na base de dados}

\item{execShiny}{booleano que indica se a funcao esta sendo executada em um contexto reativo, para atualizacao da barra de progresso}
}
\value{
\code{mensagem} vetor de caracteres com a mensagem de sucesso de gravacao na base
}
\description{
Faz os calculos da disponibilidade hidraulica atualizando as tabelas BPO_A06_SAIDA_HIDRO_NEWAVE, BPO_A08_DADOS_CALCULADOS_UHE e 
BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA
}
