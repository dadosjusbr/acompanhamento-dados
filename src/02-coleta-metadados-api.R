# use libs
library(tidyverse)
library(jsonlite)
library(lubridate)
library(here)
`%notin%` <- function(x, y) !(x %in% y)

# CARREGA TABELA DE ÓRGAOS -----------------------------------------------------

# A função carregada irá organizar os órgãos para download
source(here("src/01-orgaos-monitorados.R"), encoding = "utf-8")

# Tabela com metadados dos órgãos monitorados pelo DadosJusBr
orgaos <- orgaos_monitorados(endp_orgaos_dadosjusbr = "https://api.dadosjusbr.org/v1/orgaos")

# USO DA API -------------------------------------------------------------------

#' @title Função para pegar dados da API para cada órgão/ano/mes
#' @description recebe sigla do órgão (`aid`), `ano` e `mes` de referência
#' @param aid `string` com sigla do órgão em letras minúsculas e sem abreviação (ex: "mpal" ou "tjpb")
#' @param ano `string` com ano de referência a partir de 2018 (ex: "2022")
#' @param mes `string` com mês de referência, entre 1 e 12 (ex: "6" para o mês de junho)
#' @return data-frame com resultados da coleta ou erro (quando dado não existe).`get_data_safe` retorna uma lista com itens `result` e `error` e é adequado para não quebrar loops.
#'
get_data <- function(aid, ano, mes) {
  message(str_glue("get {aid} {ano} {mes}"))
  Sys.sleep(.15)
  fromJSON(
    # url-base
    str_glue("https://api.dadosjusbr.org/v1/dados/{aid}/{ano}/{mes}")
  )
}

# safely mode of `get_data`
get_data_safe <- safely(get_data)

# PACOTE DE DADOS --------------------------------------------------------------

# Aqui os dados são coletados para todos os órgãos/ano/mês disponíveis no site
# cada órgão/ano/mes é uma linha da tabela e quando um órgão/ano/mês não é
# coletado nós mantemos a linha na tabela juntamente com o erro de coleta
# ATENÇÃO: tempo de processamento/coleta desses dados da API pode demorar até 1h
{
  ini <- now()
  message(str_glue("Início: {ini}"))

  pacote_de_dados <- orgaos %>%
    select(aid, agrupamento) %>%
    crossing(ano = 2018:2022, .) %>%
    crossing(mes = 1:12L, .) %>%
    arrange(aid, ano, mes) %>%
    mutate(a1 = pmap(list(aid, ano, mes), get_data_safe)) %>%
    unnest(a1) %>%
    group_by(aid, ano, mes) %>%
    mutate(tipo = c("df", "download_error")) %>%
    ungroup()

  end <- now()
  message(str_glue("Início: {ini}"))
  message(str_glue("Fim: {end}"))
  message(ini - end)

  # guardo uma cópia para não precisar ficar consumindo a API toda hora
  saveRDS(pacote_de_dados, here::here(str_glue("data/load/pacote-de-dados-{today()}.rds")))
}

pacote_de_dados %>%
  pivot_wider(names_from = tipo, values_from = a1) %>%
  mutate(download_error = map(download_error, as.character)) %>%
  unnest(c(df, download_error), keep_empty = TRUE, names_repair = "universal") %>%
  count(download_error, sort = TRUE) %>%
  glimpse()
