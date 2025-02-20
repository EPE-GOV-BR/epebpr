% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graficoRiscoDeficit.R
\name{graficoRiscoDeficit}
\alias{graficoRiscoDeficit}
\title{Exibe graficos de risco de defict}
\usage{
graficoRiscoDeficit(
  baseSQLite,
  tipoCaso,
  numeroCaso,
  codModelo,
  inicioHorizonteGrafico,
  fimHorizonteGrafico,
  tituloGrafico = paste0("Risco de Déficit - Caso ", numeroCaso)
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
objeto do tipo ggplot
}
\description{
Monta graficos de risco de defict de um caso especifico
}
