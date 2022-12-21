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
orgaos <- fromJSON(orgaos_endp) %>%
  as_tibble() %>%
  rename(id_orgao = aid)

# LISTA DE ÓRGÂOS COMPLETA COM AGRUPAMENTO
lista_de_orgaos <- bind_rows(
  crossing(grupo = "Justiça Eleitoral",    id_orgao = str_glue("tre{ufs}")),
  crossing(grupo = "Ministérios Públicos", id_orgao = str_glue("mp{ufs}")),
  crossing(grupo = "Tribunais de Justiça", id_orgao = str_glue("tj{ufs}")),
  crossing(grupo = "Justiça do Trabalho",  id_orgao = str_glue("trt{1:24}")),
  crossing(grupo = "Justiça Militar", id_orgao = str_glue("tjm{ufs}")),
  crossing(grupo = "Justiça Federal", id_orgao = str_glue("trf{1:5}")),
  crossing(grupo = "Justiça Federal", id_orgao = c("mpf", "mpt", "stf", "stj"))
  ) %>%
  mutate(
    aid = case_when(
      # Esse agrupamento reflete a navegação no site e a órdem dos gráficos do
      # índice de transparência
      grupo == "Justiça Eleitoral" ~ gsub("^(tre)(.+)$", "\\1-\\2", id_orgao),
      grupo == "Ministérios Públicos" ~ gsub("^(mp)(.+)$", "\\1-\\2", id_orgao),
      grupo == "Tribunais de Justiça" ~ gsub("^(tj)(.+)$", "\\1-\\2", id_orgao),
      grupo == "Justiça do Trabalho" ~ gsub("^(trt)(.+)$", "\\1-\\2", id_orgao),
      grupo == "Justiça Militar" ~ gsub("^(tjm)(.+)$", "\\1-\\2", id_orgao),
      grupo == "Justiça Federal" ~ gsub("^(trf)(.+)$", "\\1-\\2", id_orgao),
      grupo == "Justiça Federal" ~ id_orgao
  ) %>% toupper())

# TABELA DE ÓRGAOS NÃO MONITORADOS
orgaos_nao_monitorados <- orgaos %>%
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

orgaos %>%
  left_join(lista_de_orgaos) %>%
  left_join(orgaos_nao_monitorados) %>%
  mutate(
    collecting = !map_lgl(collecting, is.null)
  )
