test_that("epebpr works", {
  #### PDE, NEWAVE, Carga Liquida
  caminho <- withr::local_tempdir()
  criaBDBalanco(caminho, "testeCL")
  caminhoBD <- paste(caminho, "testeCL.sqlite3", sep = "/")
  
  expect_equal(carregaDadosSQLite(baseSQLite = caminhoBD,
                                  pastaCaso = "testDataCL",
                                  pastaSaidas = "testDataCL/nwlistop",
                                  tipoCaso = 1,
                                  numeroCaso = 1,
                                  codModelo = 1,
                                  descricaoCaso = "teste",
                                  horasPonta = 10,
                                  idDemandaLiquida = 2,
                                  sistemasNaoModulamPonta = c(6, 13),
                                  sistemasNaoModulamMedia = NA,
                                  sistemasModulamTabela = c(5, 8),
                                  codTucurui = 275,
                                  cotaLimiteTucurui = 62,
                                  geracaoLimiteTucurui = 4000,
                                  execShiny = FALSE),
               "Leitura e grava\u00E7\u00E3o dos dados de entrada efetuadas com sucesso!")
  
  # calcula disp hidro com flag vertimento TRUE e flag UHE FALSE
  expect_equal(calculaDisponibilidadeHidro(baseSQLite = caminhoBD,
                                           pastaCaso = "testDataCL",
                                           pastaSaidas = "testDataCL/nwlistop",
                                           tipoCaso = 1,
                                           numeroCaso = 1,
                                           codModelo = 1,
                                           codTucurui = 275,
                                           flagVert = TRUE,
                                           flagUHE = FALSE),
               "Disponibilidade hidro processada com sucesso!")
  
  # calcula disp hidro com flag vertimento FALSE e flag UHE TRUE
  expect_equal(calculaDisponibilidadeHidro(baseSQLite = caminhoBD,
                                           pastaCaso = "testDataCL",
                                           pastaSaidas = "testDataCL/nwlistop",
                                           tipoCaso = 1,
                                           numeroCaso = 1,
                                           codModelo = 1,
                                           codTucurui = 275,
                                           flagVert = FALSE,
                                           flagUHE = TRUE),
               "Disponibilidade hidro processada com sucesso!")
  
  # calcula balanco resumido
  expect_equal(calculaBalancoParalelo(baseSQLite = caminhoBD,
                                      tipoCaso = 1,
                                      numeroCaso = 1,
                                      codModelo = 1,
                                      cvuTransmissao = 2e-6,
                                      cvuHidro = 3e-5,
                                      cvuRenovaveis = 1e-5,
                                      cvuOutrasTermicas = 0.1,
                                      balancoResumido = TRUE,
                                      distribuicaoDeficit = 1,
                                      execShiny = FALSE),
               "Balan\u00E7o de pot\u00EAncia executado e gravado com sucesso!")
  
  # calcula balanco completo
  expect_equal(calculaBalancoParalelo(baseSQLite = caminhoBD,
                                      tipoCaso = 1,
                                      numeroCaso = 1,
                                      codModelo = 1,
                                      cvuTransmissao = 2e-6,
                                      cvuHidro = 3e-5,
                                      cvuRenovaveis = 1e-5,
                                      cvuOutrasTermicas = 0.1,
                                      balancoResumido = FALSE,
                                      distribuicaoDeficit = 1,
                                      execShiny = FALSE),
               "Balan\u00E7o de pot\u00EAncia executado e gravado com sucesso!")
  
  # grava saidas da analise
  expect_equal(gravacaoSaidasAnalises(baseSQLite = caminhoBD,
                                      tipoCaso = 1,
                                      numeroCaso = 1,
                                      codModelo = 1,
                                      df.dadosGerais = leitorrmpe::leituraDadosGerais("testDataCL")),
               "saidas de analise gravadas com sucesso!")
  
  #### PDE, NEWAVE, Carga Bruta
  caminho <- withr::local_tempdir()
  criaBDBalanco(caminho, "testeCB")
  caminhoBD <- paste(caminho, "testeCB.sqlite3", sep = "/")
  
  expect_equal(carregaDadosSQLite(baseSQLite = caminhoBD,
                                  pastaCaso = "testDataCB",
                                  pastaSaidas = "testDataCB/nwlistop",
                                  tipoCaso = 1,
                                  numeroCaso = 1,
                                  codModelo = 1,
                                  descricaoCaso = "teste",
                                  horasPonta = 10,
                                  idDemandaLiquida = 1,
                                  sistemasNaoModulamPonta = c(6, 13),
                                  sistemasNaoModulamMedia = NA,
                                  sistemasModulamTabela = c(5, 8),
                                  codTucurui = 275,
                                  cotaLimiteTucurui = 62,
                                  geracaoLimiteTucurui = 4000,
                                  execShiny = FALSE),
               "Leitura e grava\u00E7\u00E3o dos dados de entrada efetuadas com sucesso!")
  
  # calcula disp hidro com flag vertimento TRUE e flag UHE FALSE
  expect_equal(calculaDisponibilidadeHidro(baseSQLite = caminhoBD,
                                           pastaCaso = "testDataCB",
                                           pastaSaidas = "testDataCB/nwlistop",
                                           tipoCaso = 1,
                                           numeroCaso = 1,
                                           codModelo = 1,
                                           codTucurui = 275,
                                           flagVert = TRUE,
                                           flagUHE = FALSE),
               "Disponibilidade hidro processada com sucesso!")
  
  # calcula disp hidro com flag vertimento FALSE e flag UHE TRUE
  expect_equal(calculaDisponibilidadeHidro(baseSQLite = caminhoBD,
                                           pastaCaso = "testDataCB",
                                           pastaSaidas = "testDataCB/nwlistop",
                                           tipoCaso = 1,
                                           numeroCaso = 1,
                                           codModelo = 1,
                                           codTucurui = 275,
                                           flagVert = FALSE,
                                           flagUHE = TRUE),
               "Disponibilidade hidro processada com sucesso!")
  
  # calcula balanco resumido
  expect_equal(calculaBalancoParalelo(baseSQLite = caminhoBD,
                                      tipoCaso = 1,
                                      numeroCaso = 1,
                                      codModelo = 1,
                                      cvuTransmissao = 2e-6,
                                      cvuHidro = 3e-5,
                                      cvuRenovaveis = 1e-5,
                                      cvuOutrasTermicas = 0.1,
                                      balancoResumido = TRUE,
                                      distribuicaoDeficit = 1,
                                      execShiny = FALSE),
               "Balan\u00E7o de pot\u00EAncia executado e gravado com sucesso!")
  
  # calcula balanco completo
  expect_equal(calculaBalancoParalelo(baseSQLite = caminhoBD,
                                      tipoCaso = 1,
                                      numeroCaso = 1,
                                      codModelo = 1,
                                      cvuTransmissao = 2e-6,
                                      cvuHidro = 3e-5,
                                      cvuRenovaveis = 1e-5,
                                      cvuOutrasTermicas = 0.1,
                                      balancoResumido = FALSE,
                                      distribuicaoDeficit = 1,
                                      execShiny = FALSE),
               "Balan\u00E7o de pot\u00EAncia executado e gravado com sucesso!")
  
  # grava saidas da analise
  expect_equal(gravacaoSaidasAnalises(baseSQLite = caminhoBD,
                                      tipoCaso = 1,
                                      numeroCaso = 1,
                                      codModelo = 1,
                                      df.dadosGerais = leitorrmpe::leituraDadosGerais("testDataCB")),
               "saidas de analise gravadas com sucesso!")
  
  #### PMO, NEWAVE, Carga Bruta
  caminho <- withr::local_tempdir()
  criaBDBalanco(caminho, "testePMO")
  caminhoBD <- paste(caminho, "testePMO.sqlite3", sep = "/")
  
  expect_equal(carregaDadosSQLite(baseSQLite = caminhoBD,
                                  pastaCaso = "testDataPMO",
                                  pastaSaidas = "testDataPMO/nwlistop",
                                  tipoCaso = 2,
                                  numeroCaso = 1,
                                  codModelo = 1,
                                  descricaoCaso = "teste",
                                  horasPonta = 10,
                                  idDemandaLiquida = 1,
                                  sistemasNaoModulamPonta = c(6, 13),
                                  sistemasNaoModulamMedia = NA,
                                  sistemasModulamTabela = c(5, 8),
                                  codTucurui = 275,
                                  cotaLimiteTucurui = 62,
                                  geracaoLimiteTucurui = 4000,
                                  execShiny = FALSE),
               "Leitura e grava\u00E7\u00E3o dos dados de entrada efetuadas com sucesso!")
  
  # calcula disp hidro com flag vertimento TRUE e flag UHE FALSE
  expect_equal(calculaDisponibilidadeHidro(baseSQLite = caminhoBD,
                                           pastaCaso = "testDataPMO",
                                           pastaSaidas = "testDataPMO/nwlistop",
                                           tipoCaso = 2,
                                           numeroCaso = 1,
                                           codModelo = 1,
                                           codTucurui = 275,
                                           flagVert = TRUE,
                                           flagUHE = FALSE),
               "Disponibilidade hidro processada com sucesso!")
  
  # calcula disp hidro com flag vertimento FALSE e flag UHE TRUE
  expect_equal(calculaDisponibilidadeHidro(baseSQLite = caminhoBD,
                                           pastaCaso = "testDataPMO",
                                           pastaSaidas = "testDataPMO/nwlistop",
                                           tipoCaso = 2,
                                           numeroCaso = 1,
                                           codModelo = 1,
                                           codTucurui = 275,
                                           flagVert = FALSE,
                                           flagUHE = TRUE),
               "Disponibilidade hidro processada com sucesso!")
  
  # calcula balanco resumido
  expect_equal(calculaBalancoParalelo(baseSQLite = caminhoBD,
                                      tipoCaso = 2,
                                      numeroCaso = 1,
                                      codModelo = 1,
                                      cvuTransmissao = 2e-6,
                                      cvuHidro = 3e-5,
                                      cvuRenovaveis = 1e-5,
                                      cvuOutrasTermicas = 0.1,
                                      balancoResumido = TRUE,
                                      distribuicaoDeficit = 1,
                                      execShiny = FALSE),
               "Balan\u00E7o de pot\u00EAncia executado e gravado com sucesso!")
  
  # calcula balanco completo
  expect_equal(calculaBalancoParalelo(baseSQLite = caminhoBD,
                                      tipoCaso = 2,
                                      numeroCaso = 1,
                                      codModelo = 1,
                                      cvuTransmissao = 2e-6,
                                      cvuHidro = 3e-5,
                                      cvuRenovaveis = 1e-5,
                                      cvuOutrasTermicas = 0.1,
                                      balancoResumido = FALSE,
                                      distribuicaoDeficit = 1,
                                      execShiny = FALSE),
               "Balan\u00E7o de pot\u00EAncia executado e gravado com sucesso!")
  
  # grava saidas da analise
  expect_equal(gravacaoSaidasAnalises(baseSQLite = caminhoBD,
                                      tipoCaso = 2,
                                      numeroCaso = 1,
                                      codModelo = 1,
                                      df.dadosGerais = leitorrmpe::leituraDadosGerais("testDataPMO")),
               "saidas de analise gravadas com sucesso!")
})
