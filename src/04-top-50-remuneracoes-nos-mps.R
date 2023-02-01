# use libs
library(tidyverse)
library(jsonlite)
library(lubridate)
library(here)
library(glue)
`%notin%` <- function(x, y) !(x %in% y)

# CARREGA TABELA DE ÓRGAOS -----------------------------------------------------

# Lista de órgãos, o script abaixo pega dados da API faz um agrupamento de órgãos
# e tratamento do campo `aid`
source(here("src/01-orgaos-monitorados.R"), encoding = "utf-8")

mps_monitorados <- orgaos_monitorados() %>%
  filter(agrupamento == "Ministério Público") %>%
  transmute(aid, orgao_monitorado = is.na(timestamp))

mps_nao_monitorados <- orgaos_monitorados() %>%
  filter(agrupamento == "Ministério Público") %>%
  filter(!is.na(timestamp)) %>%
  transmute(
    aid_pkg = aid,
    agrupamento_pkg = agrupamento,
    status = "Órgão não monitorado pelo DadosJusBr",
    pkg_coletado = FALSE
  ) %>%
  crossing(ano_pkg = 2018:2022L) %>%
  crossing(mes_pkg = 1:12L) %>%
  mutate(date_pkg = my(glue("{mes_pkg}-{ano_pkg}")))

# USO DA API -------------------------------------------------------------------

#' @title Função para pegar dados da API para cara órgão/ano/mes
#' @description recebe sigla do órgão (`aid`), `ano` e `mes` de referencia
#' @param aid `string` com sigla do órgão, lower case e sem abreviação (ex: "mpal" ou "tjpb")
#' @param ano `string` com ano de referencia a partid de 2018 (ex: "2022")
#' @param mes `string` com mês de referencia, entre 1 e 12 (ex: "6")
#' @return data-frame com resultados da coleta ou erro (quando dado não existe).`get_data_safe` retorna uma lista com itens `result` e `error` e é adequado para não quebrar loops.
#'
get_data <- function(id_orgao, ano, mes) {
  message(str_glue("get {id_orgao} {ano} {mes}"))
  Sys.sleep(.15)
  fromJSON(
    # url-base
    str_glue("https://api.dadosjusbr.org/v1/dados/{id_orgao}/{ano}/{mes}")
  )
}

# safely mode
get_data_safe <- safely(get_data)

# DOWNLOAD PACOTE DE DADOS -----------------------------------------------------

# Aqui os dados são coletados para todos os órgãos/ano/mês disponíveis no site
# cada órgão/ano/mes é uma linha da tabela e quando um órgão/ano/mês não é
# coletado nós mantemos a linha na tabela juntamente com o erro de coleta
# ATENÇÃO: tempo de processamento/coleta desses dados da API pode demorar até 1h
# {
#   ini <- now()
#   message(str_glue("Início: {ini}"))

#   pacote_de_dados <- orgaos %>%
#     filter(agrupamento == "Ministério Público") %>%
#     filter(is.na(timestamp)) %>%
#     select(id_orgao = aid, name, type, entity, uf, agrupamento) %>%
#     crossing(ano = 2018:2022, .) %>%
#     crossing(mes = 1:12L, .) %>%
#     arrange(id_orgao, ano, mes) %>%
#     mutate(a1 = pmap(list(id_orgao, ano, mes), get_data_safe)) %>%
#     unnest(a1) %>%
#     group_by(id_orgao, ano, mes) %>%
#     mutate(tipo = c("df", "download_error")) %>%
#     ungroup()

#   end <- now()
#   message(str_glue("Início: {ini}"))
#   message(str_glue("Fim: {end}"))
#   message(ini - end)

#   # guardo uma cópia para não precisar ficar consumindo a API toda hora
#   saveRDS(pacote_de_dados, here::here(str_glue("data/load/pacote-de-dados-mps-{today()}.rds")))
# }

#
pkgs <- readRDS(here("data/load/pacote-de-dados-mps-2023-01-30.rds")) %>%
  pivot_wider(names_from = tipo, values_from = a1) %>%
  rename(aid = id_orgao) %>%
  set_names(glue("{names(.)}_pkg")) %>%
  transmute(
    mes_pkg,
    ano_pkg,
    aid_pkg,
    df_pkg
    # download_error_pkg,
    # pkg_coletado = if_else(!map_lgl(df_pkg, is.null), TRUE, FALSE),
    # download_error_pkg = map(download_error_pkg, as.character)
  ) %>%
  unnest(df_pkg) %>%
  select(contains("pkg"), pacote_de_dados) %>%
  unnest(pacote_de_dados) %>%
  mutate(
    url = replace_na(url, "https://dadosjusbr-public.s3.amazonaws.com/mpt/datapackage/mpt-2019-12.zip")
  )

pkgs %>%
  transmute(
    url,
    destfile = url %>%
      str_remove_all(glue("https://dadosjusbr-public.s3.amazonaws.com/{aid_pkg}/datapackage/")) %>%
      paste0("data/load/temp/", .) %>%
      here()
  ) %>%
  pwalk(download.file, mode = "wb", method = "libcurl")

pkgs %>%
  transmute(
    zipfile = url %>%
      str_remove_all(glue("https://dadosjusbr-public.s3.amazonaws.com/{aid_pkg}/datapackage/")) %>%
      paste0("data/load/temp/", .) %>%
      here(),
    file1 = "contra_cheque.csv",
    file2 = "remuneracao.csv",
    files = map2(file1, file2, ~ c(.x, .y)),
    exdir = here(glue("data/load/temp/{aid_pkg}-{ano_pkg}-{mes_pkg}"))
  ) %>%
  select(-file1, -file2) %>%
  pwalk(unzip)

create_income_rank <- function(df1, df2) {
  df1 %>%
    left_join(df2) %>%
    count(
      id_contra_cheque, chave_coleta, nome, matricula, funcao,
      local_trabalho, tipo, ativo, categoria,
      wt = valor, name = "valor"
    ) %>%
    pivot_wider(names_from = categoria, values_from = valor)
}

create_income_rank_safe <- safely(create_income_rank)

dados <- pkgs %>%
  transmute(
    remuneracao_path = here(glue("data/load/temp/{aid_pkg}-{ano_pkg}-{mes_pkg}/remuneracao.csv")),
    contra_cheque_path = here(glue("data/load/temp/{aid_pkg}-{ano_pkg}-{mes_pkg}/contra_cheque.csv")),
  ) %>%
  mutate(
    remuneracao_df = map(remuneracao_path, read_csv, col_types = cols(valor = col_double(), .default = col_character())),
    contra_cheque_df = map(contra_cheque_path, read_csv, col_types = cols(.default = col_character())),
    income_rank = map2(remuneracao_df, contra_cheque_df, create_income_rank_safe)
  )

remuneracoes <- dados %>%
  transmute(
    id = remuneracao_path %>%
      str_remove("C:/Users/rdurl/OneDrive/Documentos/acompanhamento-dados/data/load/temp/") %>%
      str_remove("/remuneracao.csv"),
    income_rank
  ) %>%
  unnest(income_rank) %>%
  group_by(id) %>%
  mutate(df = c("resultado", "erro")) %>%
  ungroup() %>%
  pivot_wider(names_from = df, values_from = income_rank) %>%
  mutate(erro = map(erro, as.character)) %>%
  unnest(erro, keep_empty = TRUE) %>%
  unnest(resultado, keep_empty = TRUE) %>%
  transmute(
    aid = str_remove(id, "-\\d{4}-\\d{1,2}$"),
    month = str_extract(id, "\\d{1,2}$"),
    year = str_extract(id, "\\d{4}"),
    id_contra_cheque, chave_coleta, nome, matricula,
    funcao, local_trabalho, tipo, ativo,
    across(where(is.double), replace_na, 0)
  ) %>%
  mutate(remuneracao_total = rowSums(across(where(is.numeric))))

# Envia os documentos

remuneracoes %>%
  select(aid:ativo, remuneracao_total) %>%
  mutate(ranking = rank(-remuneracao_total)) %>%
  arrange(ranking) %>%
  select(-id_contra_cheque, -chave_coleta) %>%
  select(ranking, everything()) %>%
  mutate(ativo = if_else(ativo == "true", "Sim", "Não")) %>%
  # slice_min(order_by = ranking, n = 50) %>%
  # mutate(
  #   link_para_pesquisa = glue('=HIPERLINK("https://dadosjusbr.org/pesquisar?anos={year}&meses={month}&orgaos={aid}&categorias=tudo"; "LINK - PESQUISA AVANÇADA")')
  # ) %>%
  # googlesheets4::write_sheet(
  #   ss = "https://docs.google.com/spreadsheets/d/17xSfvRDoCbiqniphfIExJr4MBlL_JvZPco8rS-teDGo",
  #   sheet = "Top 50 remunerações nos mínistérios públicos"
  # )
  write_csv(here("data/load/remuneracoes-mps-2018-2022.csv"))
