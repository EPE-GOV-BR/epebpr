% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gravaDadosHidroVigenteBDBP.R
\name{gravacaoDadosHidroVigenteBDBP}
\alias{gravacaoDadosHidroVigenteBDBP}
\title{Gravacao dos dados das usinas hidreletricas ao longo do horizonte de simulacao}
\usage{
gravacaoDadosHidroVigenteBDBP(
  pastaCaso,
  conexao,
  tipoCaso,
  numeroCaso,
  codModelo
)
}
\arguments{
\item{pastaCaso}{caracter com a localizacao dos arquivos NEWAVE e auxliares do BP.}

\item{conexao}{caracter com a conexao com o Banco de Dados do Balanco de Potencia.}

\item{tipoCaso}{caracter com o tipo de caso simulado. [1]=PDE [2]=PMO [3]=GF.}

\item{numeroCaso}{caracter com o numero do caso, definido pelo usuario.}

\item{codModelo}{caracter com a definicao do modelo utilizado. [1]=Newave [2]=Suishi.}
}
\value{
\code{mensagem} vetor de caracteres com a mensagem de sucesso de gravacao na tabela BPO_A05_DADOS_VIGENTES_UHE
}
\description{
Faz a gravacao dos dados das usinas hidreletricas ao longo do horizonte de simulacao do NEWAVE no banco de dados do Balanco de Potencia (BDBP)
Os dados sao gravados na tabela BPO_A05_DADOS_VIGENTES_UHE do BDBP.
}
\examples{
\dontrun{
gravacaoDadosHidroVigenteBDBP("C:/PDE2027_Caso080", conexao, 1, 80, 1)
}

}
