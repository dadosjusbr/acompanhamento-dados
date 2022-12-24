library(tidyverse)
library(jsonlite)

# URL COM STATUS DE COLETA
orgaos_url_status <- "https://dadosjusbr.org/status"

# ENDPOINT PARA CONSULTA DE ÓRGÂOS NA API DADOSJUSBR
orgaos_endp <- "https://api.dadosjusbr.org/v1/orgaos"

# LISTA DE UFS
ufs <- c(
  "ac", "al", "ap", "am", "ba", "ce", "es", "go", "ma",
  "mt", "ms", "mg", "pa", "pb", "pr", "pe", "pi", "rj",
  "rn", "rs", "ro", "rr", "sc", "sp", "se", "to", "dft"
)

# CONSULTA TABELA DE ÒRGÂOS NA API
orgaos_raw <- fromJSON(orgaos_endp) %>%
  as_tibble() %>%
  rename(id_orgao = aid) %>%
  unnest(collecting, keep_empty = TRUE) %>%
  unnest(description, keep_empty = TRUE) %>%
  group_by(id_orgao) %>%
  mutate(idx = str_glue("description{1:n()}")) %>%
  ungroup() %>%
  pivot_wider(names_from = idx, values_from = description) %>%
  googlesheets4::write_sheet("https://docs.google.com/spreadsheets/d/1nWRydP5Um1JrCQ7pTe9feubLdd2sTK8KRpoyyvv0DAY", sheet = "órgãos na API")

# LISTA DE ÓRGÂOS COMPLETA COM AGRUPAMENTO
lista_de_orgaos <- bind_rows(
  # Esse agrupamento reflete a navegação no site e a órdem dos gráficos do
  # índice de transparência
  tibble(grupo = "Ministérios Públicos", id_orgao = str_glue("mp{ufs}")),
  tibble(grupo = "Ministérios Públicos", id_orgao = c("mpf", "mpt", "mpm")),
  tibble(grupo = "Tribunais de Justiça", id_orgao = str_glue("tj{ufs}")),
  tibble(grupo = "Justiça Militar", id_orgao = str_glue("tjm{ufs}")),
  tibble(grupo = "Justiça Eleitoral", id_orgao = str_glue("tre{ufs}")),
  tibble(grupo = "Justiça do Trabalho", id_orgao = str_glue("trt{1:24}")),
  tibble(grupo = "Justiça Federal", id_orgao = str_glue("trf{1:5}")),
  tibble(grupo = "Justiça Federal", id_orgao = c("mpf", "mpt", "stf", "stj"))
) %>%
  mutate(
    aid = case_when(
      # aid label com upper case
      grupo == "Justiça Eleitoral" ~ gsub("^(tre)(.+)$", "\\1-\\2", id_orgao),
      grupo == "Ministérios Públicos" ~ gsub("^(mp)(.+)$", "\\1-\\2", id_orgao),
      grupo == "Tribunais de Justiça" ~ gsub("^(tj)(.+)$", "\\1-\\2", id_orgao),
      grupo == "Justiça do Trabalho" ~ gsub("^(trt)(.+)$", "\\1-\\2", id_orgao),
      grupo == "Justiça Militar" ~ gsub("^(tjm)(.+)$", "\\1-\\2", id_orgao),
      grupo == "Justiça Federal" ~ gsub("^(trf)(.+)$", "\\1-\\2", id_orgao),
      grupo == "Justiça Federal" ~ id_orgao
    ) %>% toupper()
  )

# TABELA DE ÓRGAOS NÃO MONITORADOS
orgaos_nao_monitorados <- orgaos_raw %>%
  filter(!map_lgl(collecting, is.null)) %>%
  select(id_orgao, collecting) %>%
  unnest(collecting) %>%
  unnest(description) %>%
  group_by(id_orgao) %>%
  mutate(idx = 1:n()) %>%
  ungroup() %>%
  pivot_wider(
    names_from = idx,
    values_from = description,
    names_glue = "description_{idx}"
  ) %>%
  mutate(timestamp = lubridate::as_datetime(timestamp))

orgaos <- orgaos_raw %>%
  # inclui grupo e aid label com upper case
  tidylog::left_join(lista_de_orgaos) %>%
  # inclui status de órgãos não monitorados
  tidylog::left_join(orgaos_nao_monitorados) %>%
  mutate(
    collecting = !map_lgl(collecting, is.null)
  )

rm(list = ls()[ls() != "orgaos"])

message("tabela de órgãos carregada !")
glimpse(orgaos)
