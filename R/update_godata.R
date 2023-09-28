update_godata <- function(){
# Importar ----------------------------------------------------------------
password= 'CIEVSSESRJ2023'
username= 'eduardomesquitapeixoto@gmail.com'
url='http://godata-rj.saude.rj.gov.br/'
# outbreak_id='de896ad2-b352-458e-8377-16fe1b1224af'

# Get ID for active outbreak:
outbreak_id <- godataR::get_active_outbreak(url = url,
                                            username = username,
                                            password = password)


# Get case data:
cases <- godataR::get_cases(url = url,
                            username = username,
                            password = password,
                            outbreak_id = outbreak_id)

# Arrumar -----------------------------------------------------------------
# Teste 1

# planilha <-
# cases |>
#   tidyr::unnest(
#     cols = c("questionnaireAnswers.numero_de_aves_no_foco",
#              "questionnaireAnswers.unidade_de_notificacao"),
#     names_repair = "universal"
#
#   ) |>
#   dplyr::select("id", "endereco" = `value...51`)

limpar<-function(x){
  stringr::str_replace_all(x, "\\slist\\s","") |>
    stringr::str_replace_all('\\sc\\(',"") |>
    stringr::str_replace_all('value \\= \\"\\,\\"', "") |>
    stringr::str_replace_all('\\"\\,\'', "") |>
    stringr::str_replace_all('\\(\\)', "") |>
    stringr::str_replace_all('\\\"\\)', "") |>
    stringr::str_replace_all('list\\(value = \\\"', "") |>
    stringr::str_replace_all('list\\(value = ', "") |>
    stringr::str_replace_all('\\blist',"") |>
    stringr::str_replace_all('\\\\t',"") |>
    stringr::str_replace_all('(?<=\\d)\\)',"") |>
    stringr::str_replace_all('NA\\)',"") |>
    stringr::str_replace_all('\\\\\\\"',"") |>
    stringr::str_trim()
}

planilha <- purrr::map(cases, limpar) |>
  list2DF() |>
  janitor::clean_names() |>
  dplyr::select(id, date_of_reporting, first_name, middle_name,
                last_name, date_become_case, date_of_onset,
                risk_reason, date_of_outcome,
                questionnaire_answers_unidade_de_notificacao,
                questionnaire_answers_data_de_notificacao,
                questionnaire_answers_notificador,
                questionnaire_answers_cargo_funcao_do_notificador,
                questionnaire_answers_endereco_onde_o_animal_foi_identificado,
                questionnaire_answers_animal_envolvido_na_notificacao,
                questionnaire_answers_especie_do_animal,
                questionnaire_answers_animal_encontrado_vivo,
                questionnaire_answers_observados_sinais_de_adoecimento_no_animal,
                questionnaire_answers_endereco_do_local_para_onde_o_animal_foi_levado)
planilha

}


