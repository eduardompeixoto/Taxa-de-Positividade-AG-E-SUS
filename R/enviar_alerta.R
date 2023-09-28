enviar_alerta <- function() {


  library(telegram.bot)
  library(googlesheets4)
  library(dplyr)
  library(purrr)

  a<-"1qwZrBScJzYe8PywgLANq7OGDnrf5uh-ebwMY8rl-7Vw"

  googlesheets4::gs4_deauth()

  variaveis <- c("Unidade","Data","Acolhimento (07:00 às 07:00)","Registro   (07:00 às 07:00)","Pacientes Classificados (AZUL)   (07:00 às 07:00)","Pacientes Classificados (VERDE)  (07:00 às 07:00)","Pacientes Classificados (AMARELO)   (07:00 às 07:00)","Pacientes Classificados (LARANJA)    (07:00 às 07:00)","Pacientes Classificados (VERMELHO)   (07:00 às 07:00)","Total de Pacientes Classificados (CM e PED)","Atendimento Clínica Médica    (07:00 às 07:00)","Atendimento Pediatria    (07:00 às 07:00)","Total de Pacientes Atendidos (CM e PED)","Atendimento Serviço Social    (07:00 às 07:00)","Atendimento Odontologia    (07:00 às 07:00)","Atendimento - Síndrome Gripal (Adulto)  (07:00 às 07:00)  CID : B34.0 ; B34.2 ; J00.0 ; J06.9 ;  J10.0 ; J10.1 J11.0 ; J11.1 ;","Atendimento - Síndrome Gripal (Pediatria)   (07:00 às 07:00)  CID : B34.0 ; B34.2 ; J00.0 ; J06.9 ;  J10.0 ; J10.1 J11.0 ; J11.1 ;  J21.9","Total de Pacientes em Observação","Observação Adulto - Demais Causas  (TOTAL DE PACIENTES DEITADOS NO LEITO/POLTRONAS ÀS 00:00H)","Observação Adulto - Síndrome Gripal  (TOTAL DE PACIENTES DEITADOS NO LEITO/POLTRONAS ÀS 00:00H)","Desfecho - Alta Hospitalar...21","Desfecho - Transfência (SER)...22","Desfecho - Transfência (VAGA ZERO) - ATENÇAO !! NÃO COMPUTAR PACIENTE QUE RETORNA PARA UNIDADE","Desfecho - Revelia...24","Desfecho - Óbito...25","Observação Pediátria - Demais Causas    (TOTAL DE PACIENTES DEITADOS NO LEITO/POLTRONAS ÀS 00:00H)","Observação Pediátrica - Síndrome Gripal      (TOTAL DE PACIENTES DEITADOS NO LEITO/POLTRONAS ÀS 00:00H)","Desfecho - Alta Hospitalar...28","Desfecho - Transfência (SER)...29","Desfecho - Transfência (VAGA ZERO)","Desfecho - Revelia...31","Desfecho - Óbito...32","Observação Sala Vermelha - Demais Causas     (TOTAL DE PACIENTES DEITADOS NO LEITO/POLTRONAS ÀS 00:00H)","Observação Sala Vermelha - Síndrome Gripal    (TOTAL DE PACIENTES DEITADOS NO LEITO/POLTRONAS ÀS 00:00H)","Desfecho - Alta Hospitalar...35","Desfecho - Transfência (SER)...36","Desfecho - Transfência (VAGA ZERO)    ATENÇAO !! NÃO COMPUTAR PACIENTE QUE RETORNA PARA UNIDADE","Desfecho - Revelia...38","Desfecho - Óbito...39","Observação Sala de Isolamento - Demais Causas    (TOTAL DE PACIENTES DEITADOS NO LEITO/POLTRONAS ÀS 00:00H)","Observação Sala de Isolamento - Síndrome Gripal    (TOTAL DE PACIENTES DEITADOS NO LEITO/POLTRONAS ÀS 00:00H)","Desfecho - Alta Hospitalar...42","Desfecho - Transfência (SER)...43","Desfecho - Transfência (VAGA ZERO)      ATENÇAO !! NÃO COMPUTAR PACIENTE QUE RETORNA PARA UNIDADE","Desfecho - Revelia...45","Desfecho - Óbito...46","RX","Exames Laboratorial","Sutura","Curativo","ECG","TOTAL DE ÓBITOS","Óbito Não Institucional","Óbito Institucional","Óbito Clínico S/ Suspeita de Síndrome Gripal (Adulto)","Óbito Clínico S/ Suspeita de Síndrome Gripal (Pediátrico)","Óbito Clínico C/ Suspeita de Síndrome Gripal (Adulto)","Óbito Clínico C/ Suspeita de Síndrome Gripal (Pediátrico)","Óbito Trauma / Violento","Total de Transferência (SER)","Transferência p/Hosp. Federal (SER)","Transferência p/ Hosp. Estadual (SER)","Transferência p/ Hosp. Municipal (SER)","Transferência P/ Hosp. Particular (SER)","Transferência P/ UPA (SER)","Total de Vaga Zero","Recebimento de Ambulância (CBMERJ)","Recusa de Ambulância (CBMERJ)","Recebimento de Ambulância (SAMU)","Recusa de Ambulância (SAMU)","Recebimento de Ambulância (ATENÇÃO BÁSICA)","Recusa de Ambulância (ATENÇÃO BÁSICA)","Arbovirose (DENGUE - CID - \"A90\")","Arbovirose ( CHIKUNGUNYA - CID - \"A92.0\")","Arbovirose (ZIKA - CID - \"A92.8\")")

  atrasados <- purrr::map(1:27, function(planilha) {
    googlesheets4::read_sheet(a, sheet = planilha) |>
      dplyr::select(tidyselect::all_of(variaveis)) |>
      dplyr::arrange(dplyr::desc(Data)) |>
      head(1) |>
      dplyr::mutate(dplyr::across(.cols = 3:length(variaveis), ~ as.character(.x) |> as.numeric()))

  }) |>
    dplyr::bind_rows() |>
    dplyr::filter(as.Date(Data) < Sys.Date() - 1)


  msg<-paste0("Os seguintes registros estão com atualização atrasada: ",paste(atrasados$Unidade,collapse = ","))

  start <- function(bot, update) {
    bot$sendMessage(
      chat_id = update$message$chat$id,
      text = sprintf("Hello %s!", update$message$from$first_name)
    )
  }

  updater <- telegram.bot::Updater("1956893433:AAE4WVZRtK9lU0iQZWSkmyGIDbHj0CUnJHk") + telegram.bot::CommandHandler("start", start)

  #updater$start_polling() # Send "/start" to the bot

  bot <- telegram.bot::Bot(token = "1956893433:AAE4WVZRtK9lU0iQZWSkmyGIDbHj0CUnJHk")

  # Get bot info
  print(bot$getMe())

  # Get updates
  updates <- bot$getUpdates()

  # Retrieve your chat id
  # Note: you should text the bot before calling `getUpdates`
  # chat_id <- updates[[1L]]$from_chat_id()


  # Send message

  bot$sendMessage(1895076581,
                  text =msg,
                  parse_mode = "Markdown"
  )

}
