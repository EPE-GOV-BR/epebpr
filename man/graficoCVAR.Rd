% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graficoCVAR.R
\name{graficoCVAR}
\alias{graficoCVAR}
\title{Exibe graficos de CVaR}
\usage{
graficoCVAR(
  baseSQLite,
  tipoCaso,
  numeroCaso,
  codModelo,
  inicioHorizonteGrafico,
  fimHorizonteGrafico,
  tipoGrafico,
  tituloGraficoCVARMes = paste0("Profundidade de Déficit - CVAR Mensal 5\% - Caso ",
    numeroCaso),
  tituloGraficoCVARAno = paste0("Profundidade de Déficit - CVAR Anual - Caso ",
    numeroCaso)
)
}
\arguments{
\item{baseSQLite}{nome e localizacao da base SQLite do balanco de potencia}

\item{tipoCaso}{valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica}

\item{numeroCaso}{valor inteiro com o numero do caso}

\item{codModelo}{valor inteiro com o codigo do modelo. 1:NEWAVE; 2:SUISHI}

\item{inicioHorizonteGrafico}{valor numerico do ano de inicio do horizonte para o grafico. Formato: AAAA. Ex: 2020}

\item{fimHorizonteGrafico}{valor numerico do ano de fim do horizonte para o grafico. Formato: AAAA. Ex:2029}

\item{tipoGrafico}{valor numerico identificando o tipo de grafico. 1: Mensal por patamar; 2: Mensal em linha; 3:Anual}

\item{tituloGraficoCVARMes}{vetor de caracteres com o titulo do grafico de CVAR mensal - Nao obrigatorio - valor padrao com numero do caso}

\item{tituloGraficoCVARAno}{vetor de caracteres com o titulo do grafico de CVAR anual - Nao obrigatorio - valor padrao com numero do caso}
}
\value{
objeto do tipo plotly
}
\description{
Monta graficos de Profundidade de Deficit por CVAR de um caso especifico
}
