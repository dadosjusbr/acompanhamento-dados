library(tidyverse)
library(jsonlite))

# CONSULTA TABELA DE ÒRGÂOS NA API
orgaos_raw <- fromJSON(orgaos_endp) %>%
  as_tibble() %>%
  rename(id_orgao = aid) %>%
  unnest(collecting, keep_empty = TRUE) %>%
  unnest(description, keep_empty = TRUE) %>%
  group_by(id_orgao) %>%
  mutate(
    idx = str_glue("description{1:n()}"),
    timestamp = as_datetime(timestamp)
  ) %>%
  ungroup() %>%
  pivot_wider(names_from = idx, values_from = description)

# Atualiza a documentação de órgãos monitorados
googlesheets4::write_sheet(
  data = orgaos_raw,
  ss = "https://docs.google.com/spreadsheets/d/1nj4YEt8SycovFMGIRhEqy4fVAEftVS0CRg25iyDrvZc",
  sheet = "órgãos na API"
)

# LISTA DE ÓRGÂOS COMPLETA COM AGRUPAMENTO
orgaos <- orgaos_raw %>%
  mutate(
    agrupamento = case_when(
      # Justiça Eleitoral
      id_orgao == "tse" ~ "JUSTIÇA ELEITORAL",
      entity == "Tribunal" & type == "Eleitoral" ~ "JUSTIÇA ELEITORAL",
      # Ministérios Públicos
      entity == "Ministério" ~ "MINISTÉRIO PÚBLICO",
      # Tribunais de Justiça
      id_orgao == "stj" ~ "TRIBUNAL DE JUSTIÇA",
      type == "Estadual" & entity == "Tribunal" ~ "TRIBUNAL DE JUSTIÇA",
      # Justiça do Trabalho
      type == "Trabalho" ~ "JUSTIÇA DO TRABALHO",
      # Justiça Federal
      id_orgao %in% paste0("trf", 1:5) ~ "JUSTIÇA FEDERAL",
      # Justiça Militar
      type == "Militar" ~ "JUSTIÇA MILITAR",
      # Órgãos Superiores
      id_orgao %in% c("cjf", "cnj", "stf") ~ "ÓRGÃOS SUPERIORES",
      # não é esperado nenum campo missing
      TRUE ~ NA_character_
    )
  )

googlesheets4::write_sheet(
  data = orgaos,
  ss = "https://docs.google.com/spreadsheets/d/1nj4YEt8SycovFMGIRhEqy4fVAEftVS0CRg25iyDrvZc",
  sheet = "órgãos na API"
)
