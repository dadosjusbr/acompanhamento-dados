#' @title Função para carregar tabela de órgãos monitorados
#' @description essa função recebe o endpoint de consulta de órgãos da API do DadosJusBr ("https://api.dadosjusbr.org/v1/orgaos") e retorna uma tabela com metadados de órgãos monitorados. Informações sobre órgãos não monitorados pelo DadosJusBr também serão exibidas no console após executar a função.
#' @param endp_orgaos_dadosjusbr `string` com url para consultar órgãos na API (defaulf: "https://api.dadosjusbr.org/v1/orgaos")
#' @param salva_copia `boolean` vc quer salvar uma cópia em alguma planilha do google spreadsheets? (default: `FALSE`)
#' @param gsheet `string` url da planilha em que vc deseja salvar a consulta
orgaos_monitorados <- function(endp_orgaos_dadosjusbr = "https://api.dadosjusbr.org/v1/orgaos",
                               salva_copia = FALSE,
                               gsheet = "https://docs.google.com/spreadsheets/d/1nj4YEt8SycovFMGIRhEqy4fVAEftVS0CRg25iyDrvZc") {
  # CONSULTA TABELA DE ÒRGÂOS NA API
  orgaos <- endp_orgaos_dadosjusbr %>%
    fromJSON() %>%
    as_tibble() %>%
    unnest(collecting, keep_empty = TRUE) %>%
    unnest(description, keep_empty = TRUE) %>%
    group_by(aid) %>%
    mutate(
      idx = str_glue("description{1:n()}"),
      timestamp = as_datetime(timestamp)
    ) %>%
    ungroup() %>%
    pivot_wider(names_from = idx, values_from = description)

  if ("agrupamento" %notin% colnames(orgaos)) {
    # LISTA DE ÓRGÂOS COMPLETA COM AGRUPAMENTO - CASO ELA NÃO EXISTA
    orgaos <- orgaos %>%
      mutate(
        agrupamento = case_when(
          aid %in% c("cjf", "cnj") ~ "Conselhos de Justiça",
          type == "Trabalho" & aid != "tst" ~ "Justiça do Trabalho",
          type == "Eleitoral" & aid != "tse" ~ "Justiça Eleitoral",
          entity == "Tribunal" & type == "Estadual" ~ "justiça Estadual",
          aid %in% str_glue("trf{1:5}") ~ "Justiça Federal",
          type == "Militar" & aid != "stm" ~ "Justiça Militar",
          entity == "Ministério" ~ "Ministério Público",
          aid %in% c("stf", "stj", "stm", "tse", "tst") ~ "Tribunais Superiores",
          TRUE ~ NA_character_
        )
      )
  }

  if (salva_copia) {
    # Atualiza a documentação de órgãos monitorados
    googlesheets4::write_sheet(
      data = orgaos,
      ss = my_gsheet,
      sheet = "órgãos na API"
    )
  }

  # DISPLAY INFO SOBRE ÓRGÂOS MONITORADOS
  message("tabela de órgãos carregada:\n")
  orgaos %>%
    count(agrupamento, name = "Quantidade de órgãos") %>%
    column_to_rownames(var = "agrupamento") %>%
    print()

  message("\nLISTA DE ÓRGÃOS NÃO MONITORADOS PELO DADOSJUSBR:\n")
  orgaos %>%
    filter(!is.na(timestamp)) %>%
    str_glue_data("\n## {aid} - {name}:\n - {description1}\n - {description2}\n\n", .na = "") %>%
    print()

  message("Consulta finalizada !")
  return(orgaos)

}

message("\nA função `orgaos_monitorados()` foi carregada!\n")
