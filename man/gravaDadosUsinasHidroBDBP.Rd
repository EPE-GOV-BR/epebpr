% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gravaDadosUsinasHidroBDBP.R
\name{gravaDadosUsinasHidroBDBP}
\alias{gravaDadosUsinasHidroBDBP}
\title{Grava dados das usinas hidraulicas no banco de dados do balanco de potencia}
\usage{
gravaDadosUsinasHidroBDBP(pasta, conexao, tipoCaso, numeroCaso, codModelo)
}
\arguments{
\item{pasta}{localizacao dos arquivos do NEWAVE}

\item{conexao}{conexao com o banco de dados (classe SQLiteConnection)}

\item{tipoCaso}{valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica}

\item{numeroCaso}{valor inteiro com o numero do caso}

\item{codModelo}{valor inteiro com o codigo do modelo. 1:NEWAVE; 2:SUISHI}
}
\value{
\code{mensagem} vetor de caracteres com a mensagem de sucesso de gravacao na tabela BPO_A03_DADOS_UHE
}
\description{
Grava dados das usinas hidraulicas originalmente nos arquivos confhd.\* e hidr.\* na tabela BPO_A03_DADOS_UHE do banco de dados do balanco de potencia
Usa funcoes do pacote (\code{leitorrmpe}).
}
\examples{
\dontrun{
gravaDadosUsinasHidroBDBP("C:/PDE2027_Caso080", conexao, 1, 80, 1)
}

}
