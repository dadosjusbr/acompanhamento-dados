library(tidyverse)
library(glue)
library(here)

# baixa zip file
mpm_metadados <- tibble(id_orgao = "mpm") %>%
  crossing(ano = 2018:2022, .) %>%
  crossing(mes = 1:12L, .) %>%
  mutate(
    url = glue("https://dadosjusbr-public.s3.amazonaws.com/mpm/datapackage/{id_orgao}-{ano}-{mes}.zip"),
    destfile = here(glue("data/load/temp/{id_orgao}-{ano}-{mes}.zip"))
  )

download.file_safe <- download.file


mpm_metadados <- mpm_metadados %>%
  mutate(
    dwld_status = map2(url, destfile, download.file_safe, mode = "wb")
  )

# unzip files
mpm_metadados %>%
  transmute(
    zipfile = url %>%
      str_remove_all(glue("https://dadosjusbr-public.s3.amazonaws.com/{id_orgao}/datapackage/")) %>%
      paste0("data/load/temp/", .) %>%
      here(),
    file1 = "contra_cheque.csv",
    file2 = "remuneracao.csv",
    files = map2(file1, file2, ~ c(.x, .y)),
    exdir = here(glue("data/load/temp/{id_orgao}-{ano}-{mes}"))
  ) %>%
  select(-file1, -file2) %>%
  pwalk(unzip)


# open files

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

mpm <- tibble(id_orgao = "mpm") %>%
  crossing(ano = 2018:2022, .) %>%
  crossing(mes = 1:12L, .) %>%
  mutate(
    remuneracao_path = here(glue("data/load/temp/{id_orgao}-{ano}-{mes}/remuneracao.csv")),
    contra_cheque_path = here(glue("data/load/temp/{id_orgao}-{ano}-{mes}/contra_cheque.csv")),
    remuneracao_df = map(remuneracao_path, read_csv, col_types = cols(valor = col_double(), .default = col_character())),
    contra_cheque_df = map(contra_cheque_path, read_csv, col_types = cols(.default = col_character())),
    income_rank = map2(remuneracao_df, contra_cheque_df, create_income_rank_safe)
  )

# apronta e exporta a base
mpm_final <- mpm %>%
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
    across(where(is.double), \(x) replace_na(x, 0))
  ) %>%
  mutate(remuneracao_total = rowSums(across(where(is.numeric)))) %>%
  janitor::clean_names() %>%
  mutate(ranking_remuneracao_total = rank(-remuneracao_total)) %>%
  mutate(ativo = if_else(ativo == "true", "Sim", "Não")) %>%
  sample_n(1) %>%
  glimpse()

write_csv(mpm_final, here("data/load/remuneracoes-mpm-2018-2022.csv"))
