pkgload::load_all()


df_esus <- pos()

writexl::write_xlsx(df_esus, "inst/planilha_esus.xlsx")

commit_message <- paste0("", Sys.time())

writeLines(commit_message, "mensagem-comit.txt")
