% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dadosGraficoRiscoDeficitSubsistema.R
\name{dadosGraficoRiscoDeficitSubs}
\alias{dadosGraficoRiscoDeficitSubs}
\title{Exporta os dados dos graficos de risco de defict por subsistema}
\usage{
dadosGraficoRiscoDeficitSubs(
  baseSQLite,
  tipoCaso,
  numeroCaso,
  codModelo,
  inicioHorizonteGrafico,
  fimHorizonteGrafico,
  tituloGrafico = paste0("Risco de Déficit por Subsistema - Caso ", numeroCaso)
)
}
\arguments{
\item{baseSQLite}{nome e localizacao da base SQLite do balanco de potencia}

\item{tipoCaso}{valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica}

\item{numeroCaso}{valor inteiro com o numero do caso}

\item{codModelo}{valor inteiro com o codigo do modelo. 1:NEWAVE; 2:SUISHI}

\item{inicioHorizonteGrafico}{valor numerico do ano de inicio do horizonte para o grafico. Formato: AAAA. Ex: 2020}

\item{fimHorizonteGrafico}{valor numerico do ano de fim do horizonte para o grafico. Formato: AAAA. Ex:2029}

\item{tituloGrafico}{vetor de caracteres com o titulo do grafico de risco - Nao obrigatorio - valor padrao com numero do caso}
}
\value{
tib.resultadosRisco tibble com os dados do grafico de risco
}
\description{
Exporta os dados dos graficos de risco de defict por subsistema de um caso especifico
}
