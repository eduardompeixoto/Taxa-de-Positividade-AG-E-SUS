pos<-function(){
  # Install the necessary packages if not already installed
  required_packages <- c("httr", "jsonlite", "dplyr")
  new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  
  # Load libraries
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(stringr)
  
  # Define the base URL for the Elasticsearch API
  url <- "https://elasticsearch-saps.saude.gov.br/desc-esus-notifica-estado-rj/_search"
  
  # Define credentials
  user <- "user-public-notificacoes"
  senha <- "Za4qNXdyQNSa9YaA"
  
  # Encode credentials in base64
  credenciais_base64 <- enc2utf8("dXNlci1wdWJsaWMtbm90aWZpY2Fjb2VzOlphNHFOWGR5UU5TYTlZYUE=")
  
  # Define headers
  headers <- add_headers(
    Authorization = paste("Basic", credenciais_base64),
    "Content-Type" = "application/json",
    Cookie = "ELASTIC-PROD=1635856782.811.5320.80284"
  )
  
  # Function to make API requests and extract data
  fetch_data <- function(query, url, headers) {
    response <- httr::POST(url, body = query, config = headers)
    stop_for_status(response)  # Stop execution if response status code is not 200
    json_data <- content(response)
    data_frame <- json_data$hits$hits %>%
      purrr::map_df(function(hit) as.data.frame(t(hit$`_source`)))
    data_frame <- data_frame %>% mutate(testes_list = list(testes))
    return(data_frame)
  }
  
  # Loop to fetch data for different date ranges
  data_frames <- lapply(1:7, function(days_back) {
    query <- sprintf('{
    "size": 10000,
    "query": {
      "match": {
        "dataNotificacao": "now-%dd/d"
      }
    }
  }', days_back)
    
    fetch_data(query, url, headers)
  })
  
  a<-bind_rows(data_frames)
  dat<-a
  
  tipoTeste<-str_extract_all(dat$testes, "tipoTeste*(.*?)\\s*,", simplify=T)
  tipoTeste<-data.frame(((tipoTeste)))
  
  dataTeste<-str_extract_all(dat$testes,"dataColetaTeste*(.*?)\\s*,", simplify=T)
  dataTeste<-data.frame(((dataTeste)))
  
  resultadoTeste<-str_extract_all(dat$testes,"resultadoTeste*(.*?)\\s*,", simplify=T)
  resultadoTeste<-data.frame(((resultadoTeste)))
  
  estadoTeste<-str_extract_all(dat$testes,"estadoTeste*(.*?)\\s*,", simplify=T)
  estadoTeste<-data.frame(((estadoTeste)))
  
  total<-NULL
  total<-dplyr::bind_cols(tipoTeste,dataTeste,resultadoTeste,estadoTeste,dat)
  
  total$n_testes<-str_count(total$testes,"tipo")
  total<-as.data.frame(total)
  table(total$n_testes)
  
  
  colnames(total)<-c('tipoTeste1',
                     'tipoTeste2',
                     'tipoTeste3',
                     'tipoTeste4',
                     'dataTeste1',
                     'dataTeste2',
                     'dataTeste3',
                     'dataTeste4',
                     'resultado1',
                     'resultado2',
                     'resultado3',
                     'resultado4',
                     'estado1',
                     'estado2',
                     'estado3',
                     'estado4',
                     'sexo','outrosSintomas','codigoEstrategiaCovid','@timestamp','dataTeste','dataSegundaReforcoDose','tipoTeste','resultadoTesteSorologicoIgA','condicoes','resultadoTeste','loteSegundaReforcoDose','@version','dataPrimeiraDose','codigoContemComunidadeTradicional','dataSegundaDose','cbo','outroLocalRealizacaoTestagem','dataEncerramento','idCollection','qualAntiviral','outrasCondicoes','estadoNotificacao','evolucaoCaso','estadoTeste','dataReforcoDose','codigoBuscaAtivaAssintomatico','outroBuscaAtivaAssintomatico','municipio','resultadoTesteSorologicoIgG','codigoDosesVacina','classificacaoFinal','estado','municipioIBGE','estadoIBGE','sintomas','id','codigoQualAntiviral','laboratorioSegundaReforcoDose','dataInicioSintomas','outroAntiviral','resultadoTesteSorologicoIgM','idade','codigoTriagemPopulacaoEspecifica','testes','estrangeiro','profissionalSaude','dataTesteSorologico','municipioNotificacaoIBGE','resultadoTesteSorologicoTotais','estadoNotificacaoIBGE','outroTriagemPopulacaoEspecifica','codigoRecebeuVacina','racaCor','tipoTesteSorologico','dataNotificacao','codigoRecebeuAntiviral','registroAtual','dataInicioTratamento','profissionalSeguranca','municipioNotificacao','recebeuAntiviral','codigoLocalRealizacaoTestagem','testes_list','n_testes')
  
  total$n_testes<-as.numeric(total$n_testes)
  
  a1<-subset(total,select = c('tipoTeste1', 'dataTeste1','resultado1','estado1','testes','n_testes','dataNotificacao',"estado","municipio"))
  a1<-subset(a1,a1$n_testes==1)
  
  a2<-subset(total,select = c(tipoTeste2, dataTeste2,resultado2,estado2,testes,n_testes,dataNotificacao,estado,municipio))
  a2<-subset(a2,a2$n_testes>1)
  
  a3<-subset(total,select = c(tipoTeste3, dataTeste3,resultado3,estado3,testes,n_testes,dataNotificacao,estado,municipio))
  a3<-subset(a3,a3$n_testes>2)
  
  colnames(a1)<-c('TipoTeste', 'dataTeste','resultado','estado','testes','n_testes','dataNotificacao','estado','municipio')
  colnames(a2)<-c('TipoTeste', 'dataTeste','resultado','estado','testes','n_testes','dataNotificacao','estado','municipio')
  colnames(a3)<-c('TipoTeste', 'dataTeste','resultado','estado','testes','n_testes','dataNotificacao','estado','municipio')
  a0<-subset(total,total$n_testes==0)
  total_mesmo<-NULL
  dat<-dplyr::bind_rows(a1,a2,a3)
  
  
  dat$tipo_exames<-toupper(dat$TipoTeste)
  dat$tipo_exames<-str_replace(dat$tipo_exames,"TIPOTESTE':","")
  dat$tipo_exames<-str_replace(dat$tipo_exames,"'","")
  dat$tipo_exames<-str_replace(dat$tipo_exames,"',","")
  dat$resultado_exames<-str_replace(dat$resultado,"resultadoTeste:","")
  dat$resultado_exames<-str_replace(dat$resultado_exames,"'","")
  dat$resultado_exames<-str_replace(dat$resultado_exames,"',","")
  dat$resultado_exames<-str_replace(dat$resultado_exames,"resultadoTeste:","")
  dat$resultado_exames<-str_replace(dat$resultado_exames,"'","")
  dat$resultado_exames<-str_replace(dat$resultado_exames,"',","")
  #sem resultados 818165 (None)
  dat$resultado_exames[str_detect(dat$resultado_exames,"Detectável")|str_detect(dat$resultado_exames,"Positivo")]<-"Reagente"
  dat$resultado_exames[dat$resultado_exames=="Não detectável"|dat$resultado_exames=="Negativo"|dat$resultado_exames=="Não Reagente"]<-"Não Reagente"
  dat$resultado_exames[dat$resultado_exames=="resultadoTeste: None,"]<-"None"
  
  
  dat$resultado_exames[str_detect(dat$resultado_exames,"ão")]<-"não detectavel"
  dat$resultado_exames[str_detect(dat$resultado_exames,"Reagente")]<-"detectavel"
  dat$resultado_exames[str_detect(dat$resultado_exames,"None")]<-"sem resultado"
  
  
  
  
  colnames(dat)<-c("TipoTeste"   ,      "dataTeste",         "resultado"   ,      "estado_teste"   ,"n_testes","dataNotificacao","estado","testes","municipio","tipo_teste_limpo","resultado_teste_limpo" )
  #require(table1)
  #table1(~periodo |resultado_exames,data=dat)
  require(qdapRegex)
  dat$dataTeste_limpa<-ex_date(dat$dataTeste)
  dat$dataTeste_limpa<-str_replace(dat$dataTeste_limpa,"20-","2020-")
  dat$dataTeste_limpa<-str_replace(dat$dataTeste_limpa,"21-","2021-")
  dat$dataTeste_limpa<-str_replace(dat$dataTeste_limpa,"22-","2022-")
  dat$dataTeste_limpa<-str_replace(dat$dataTeste_limpa,"23-","2023-")
  
  dat$dataTeste_limpa<-as.Date(dat$dataTeste_limpa)
  dat$dataTeste_limpa[lubridate::year(dat$dataTeste_limpa)<2020]<-NA
  dat$concluido<-"Não"
  dat$concluido[stringr::str_detect(dat$estado_teste,"onclu")]<-"Sim"
  dat$concluido[stringr::str_detect(dat$resultado,"None")==F]<-"Sim"
  testes<-subset(dat,select = c(n_testes,dataNotificacao,estado,municipio,testes,tipo_teste_limpo,resultado_teste_limpo,dataTeste_limpa,concluido))
  a<-str_detect(testes$tipo_teste_limpo,"ANTÍGENO")
  a<-subset(testes,a)
  colnames(a)<-c("testes","n_testes","dataNotificacao","municipio","estado","tipo_teste_limpo","resultado_teste_limpo","dataTeste_limpa","concluido")
  a<-subset(a,lubridate::year(a$dataTeste_limpa)>2021)
  #a<-subset(a,stringr::str_detect(a$estado,"Rio de Janeiro"))
  a$mes<- a$dataTeste_limpa
  a<-subset(a,a$concluido=="Sim")
  a$municipio[a$municipio=="Armação de Búzios"|a$municipio=="Armação dos Búzios"]<-"Armação de Búzios"
  b<-subset(a,a$resultado_teste_limpo=="detectavel")
  AG_TOTAL<-data.frame(table(as.character(a$dataTeste_limpa), as.character(a$municipio)))
  AG_POS<-data.frame(table(as.character(b$dataTeste_limpa), as.character(b$municipio)))
  require(dplyr)
  junto<-left_join(AG_TOTAL,AG_POS,by=c("Var1","Var2"))
  ###################################
  junto<-subset(junto,junto$Var2!="Parintins")
  junto<-subset(junto,junto$Var2!="")
  
  junto$cod_muni<-NA
  
  colnames(junto)<-c('dt_coleta', 'municipio', 'Total_Exames',	'Positivos','cod_muni')
  
  junto<-subset(junto,select=c(municipio,	cod_muni,	dt_coleta,	Positivos,	Total_Exames))                   
  
  junto$cod_muni[junto$municipio=="Angra dos Reis"]<-330010
  junto$cod_muni[junto$municipio=="Aperibé"]<-330015
  junto$cod_muni[junto$municipio=="Araruama"]<-330020
  junto$cod_muni[junto$municipio=="Areal"]<-330022
  junto$cod_muni[junto$municipio=="Armação de Búzios"]<-330023
  junto$cod_muni[junto$municipio=="Arraial do Cabo"]<-330025
  junto$cod_muni[junto$municipio=="Barra do Piraí"]<-330030
  junto$cod_muni[junto$municipio=="Barra Mansa"]<-330040
  junto$cod_muni[junto$municipio=="Belford Roxo"]<-330045
  junto$cod_muni[junto$municipio=="Bom Jardim"]<-330050
  junto$cod_muni[junto$municipio=="Bom Jesus do Itabapoana"]<-330060
  junto$cod_muni[junto$municipio=="Cabo Frio"]<-330070
  junto$cod_muni[junto$municipio=="Cachoeiras de Macacu"]<-330080
  junto$cod_muni[junto$municipio=="Cambuci"]<-330090
  junto$cod_muni[junto$municipio=="Campos dos Goytacazes"]<-330100
  junto$cod_muni[junto$municipio=="Cantagalo"]<-330110
  junto$cod_muni[junto$municipio=="Carapebus"]<-330093
  junto$cod_muni[junto$municipio=="Cardoso Moreira"]<-330115
  junto$cod_muni[junto$municipio=="Carmo"]<-330120
  junto$cod_muni[junto$municipio=="Casimiro de Abreu"]<-330130
  junto$cod_muni[junto$municipio=="Comendador Levy Gasparian"]<-330095
  junto$cod_muni[junto$municipio=="Conceição de Macabu"]<-330140
  junto$cod_muni[junto$municipio=="Cordeiro"]<-330150
  junto$cod_muni[junto$municipio=="Duas Barras"]<-330160
  junto$cod_muni[junto$municipio=="Duque de Caxias"]<-330170
  junto$cod_muni[junto$municipio=="Engenheiro Paulo de Frontin"]<-330180
  junto$cod_muni[junto$municipio=="Guapimirim"]<-330185
  junto$cod_muni[junto$municipio=="Iguaba Grande"]<-330187
  junto$cod_muni[junto$municipio=="Itaboraí"]<-330190
  junto$cod_muni[junto$municipio=="Itaguaí"]<-330200
  junto$cod_muni[junto$municipio=="Italva"]<-330205
  junto$cod_muni[junto$municipio=="Itaocara"]<-330210
  junto$cod_muni[junto$municipio=="Itaperuna"]<-330220
  junto$cod_muni[junto$municipio=="Itatiaia"]<-330225
  junto$cod_muni[junto$municipio=="Japeri"]<-330227
  junto$cod_muni[junto$municipio=="Laje do Muriaé"]<-330230
  junto$cod_muni[junto$municipio=="Macaé"]<-330240
  junto$cod_muni[junto$municipio=="Macuco"]<-330245
  junto$cod_muni[junto$municipio=="Magé"]<-330250
  junto$cod_muni[junto$municipio=="Mangaratiba"]<-330260
  junto$cod_muni[junto$municipio=="Maricá"]<-330270
  junto$cod_muni[junto$municipio=="Mendes"]<-330280
  junto$cod_muni[junto$municipio=="Mesquita"]<-330285
  junto$cod_muni[junto$municipio=="Miguel Pereira"]<-330290
  junto$cod_muni[junto$municipio=="Miracema"]<-330300
  junto$cod_muni[junto$municipio=="Natividade"]<-330310
  junto$cod_muni[junto$municipio=="Nilópolis"]<-330320
  junto$cod_muni[junto$municipio=="Niterói"]<-330330
  junto$cod_muni[junto$municipio=="Nova Friburgo"]<-330340
  junto$cod_muni[junto$municipio=="Nova Iguaçu"]<-330350
  junto$cod_muni[junto$municipio=="Paracambi"]<-330360
  junto$cod_muni[junto$municipio=="Paraíba do Sul"]<-330370
  junto$cod_muni[junto$municipio=="Paraty"]<-330380
  junto$cod_muni[junto$municipio=="Paty do Alferes"]<-330385
  junto$cod_muni[junto$municipio=="Petrópolis"]<-330390
  junto$cod_muni[junto$municipio=="Pinheiral"]<-330395
  junto$cod_muni[junto$municipio=="Piraí"]<-330400
  junto$cod_muni[junto$municipio=="Porciúncula"]<-330410
  junto$cod_muni[junto$municipio=="Porto Real"]<-330411
  junto$cod_muni[junto$municipio=="Quatis"]<-330412
  junto$cod_muni[junto$municipio=="Queimados"]<-330414
  junto$cod_muni[junto$municipio=="Quissamã"]<-330415
  junto$cod_muni[junto$municipio=="Resende"]<-330420
  junto$cod_muni[junto$municipio=="Rio Bonito"]<-330430
  junto$cod_muni[junto$municipio=="Rio Claro"]<-330440
  junto$cod_muni[junto$municipio=="Rio das Flores"]<-330450
  junto$cod_muni[junto$municipio=="Rio das Ostras"]<-330452
  junto$cod_muni[junto$municipio=="Rio de Janeiro"]<-330455
  junto$cod_muni[junto$municipio=="Santa Maria Madalena"]<-330460
  junto$cod_muni[junto$municipio=="Santo Antônio de Pádua"]<-330470
  junto$cod_muni[junto$municipio=="São Fidélis"]<-330480
  junto$cod_muni[junto$municipio=="São Francisco de Itabapoana"]<-330475
  junto$cod_muni[junto$municipio=="São Gonçalo"]<-330490
  junto$cod_muni[junto$municipio=="São João da Barra"]<-330500
  junto$cod_muni[junto$municipio=="São João de Meriti"]<-330510
  junto$cod_muni[junto$municipio=="São José de Ubá"]<-330513
  junto$cod_muni[junto$municipio=="São José do Vale do Rio Preto"]<-330515
  junto$cod_muni[junto$municipio=="São Pedro da Aldeia"]<-330520
  junto$cod_muni[junto$municipio=="São Sebastião do Alto"]<-330530
  junto$cod_muni[junto$municipio=="Sapucaia"]<-330540
  junto$cod_muni[junto$municipio=="Saquarema"]<-330550
  junto$cod_muni[junto$municipio=="Seropédica"]<-330555
  junto$cod_muni[junto$municipio=="Silva Jardim"]<-330560
  junto$cod_muni[junto$municipio=="Sumidouro"]<-330570
  junto$cod_muni[junto$municipio=="Tanguá"]<-330575
  junto$cod_muni[junto$municipio=="Teresópolis"]<-330580
  junto$cod_muni[junto$municipio=="Trajano de Moraes"]<-330590
  junto$cod_muni[junto$municipio=="Três Rios"]<-330600
  junto$cod_muni[junto$municipio=="Valença"]<-330610
  junto$cod_muni[junto$municipio=="Varre-Sai"]<-330615
  junto$cod_muni[junto$municipio=="Vassouras"]<-330620
  junto$cod_muni[junto$municipio=="Volta Redonda"]<-330630
  junto$dt_coleta<-as.Date(junto$dt_coleta)
  
  k<-subset(junto,junto$dt_coleta>= (Sys.Date()-7))
  
  junto<-k
  junto$dt_coleta<-format(junto$dt_coleta,"%d/%m/%Y")
  junto[is.na(junto)]<-0
  
  junto

                  }
