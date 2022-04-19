options(radian.complete_while_typing = FALSE)
library(tidyverse)
library(lubridate)
library(here)

# Planilhas com órgãos coletados
url_sheet <- "https://docs.google.com/spreadsheets/d/1sIebyMnsFMwGnUCZiQ6d_dIvPmvLtcFhszZnMU_QSTY"

# quero somente órgãos em que a coleta está ativa
limite_esforco <- url_sheet %>%
  googlesheets4::read_sheet(range = "B:C") %>%
  janitor::clean_names() %>%
  filter(str_detect(orgao, "MP")) %>%
  mutate(
    status = if_else(orgao %in% c("MPSP", "MPRN", "MPSE") | is.na(status),
                     "Limite de esforço", status),
    orgao = str_replace(orgao, "^MP", "MP-")
  )

# colunas
col_defs <- cols(
    orgao = col_character(),
    ano = col_integer(),
    mes = col_integer(),
    tem_lotacao = col_logical(),
    tem_cargo = col_logical(),
    tem_matricula = col_logical(),
    receita_base = col_character(),
    despesas = col_character(),
    outras_receitas = col_character(),
    nao_requer_login = col_logical(),
    nao_requer_captcha = col_logical(),
    acesso = col_character(),
    formato_consistente = col_logical(),
    estritamente_tabular = col_logical(),
    acesso_i = col_double(),
    indice_completude = col_double(),
    indice_facilidade = col_double(),
    indice_transparencia = col_double()
  )

# isso é provisório
insere_linhas_faltando <- function(df) {
  df %>% add_row(
    orgao = c("mpam", "mppb"),
    ano = c(2021L, 2021L),
    mes = c(12L, 12L),
    tem_lotacao	= c(T, T),
    tem_cargo	= c(T, T),
    tem_matricula	= c(T, T),
    receita_base = c("DETALHADO",	"DETALHADO"),
    despesas = c("DETALHADO",	"DETALHADO"),
    outras_receitas = c("DETALHADO",	"DETALHADO"),
    nao_requer_login	= c(T, T),
    nao_requer_captcha = c(T, T),
    acesso = c("NECESSITA_SIMULACAO_USUARIO", "AMIGAVEL_PARA_RASPAGEM"),
    formato_consistente = c(T, T),
    estritamente_tabular = c(F, F),
    acesso_i = c(0, 0.5),
  )
}

# vamos recalcular os índices
indice_raw <- "data/load/indice_transparencia_2022_03_08.csv" %>%
  here() %>%
  read_csv(col_types = col_defs) %>%
  filter(str_detect(orgao, "^mp"), indice_transparencia != 0) %>%
  # vamos recalcular os índices
  select(
    -indice_completude,
    -indice_facilidade,
    -indice_transparencia
  ) %>%
  insere_linhas_faltando() %>%
  mutate(orgao = str_replace(toupper(orgao), "^MP", "MP-"))

# limpeza
indice <- indice_raw %>%
  # ordena órgãos
  mutate(orgao = factor(orgao, levels = limite_esforco$orgao)) %>%
  # Inclui meses em que órgão não prestou contas
  complete(orgao, ano, mes) %>%
  # Inclui labels
  left_join(limite_esforco) %>%
  mutate(
    necessidade_login = case_when(
      status == "Limite de esforço" ~ status,
      nao_requer_login ~ "Não requer login",
      !nao_requer_login ~ "Requer login",
      TRUE ~ "Órgão não prestou contas"
    ),
    ord = case_when(
      necessidade_login == "Limite de esforço" ~ 0,
      necessidade_login == "Órgão não prestou contas" ~ 0.1,
      necessidade_login == "Requer login" ~ 0.5,
      necessidade_login == "Não requer login" ~ 1
    ),
    necessidade_captcha = case_when(
      status == "Limite de esforço" ~ status,
      nao_requer_captcha ~ "Não requer captcha",
      !nao_requer_captcha ~ "Requer captcha",
      TRUE ~ "Órgão não prestou contas"
    ),
    # Atribui pontuações para classificações de completude
    across(c(receita_base, despesas, outras_receitas),
      ~ case_when(
        . == "DETALHADO" ~ 1,
        . == "SUMARIZADO" ~ 0.5,
        TRUE ~ NA_real_
    )),
  )

criterios <- c("Tem cargo", "Tem lotação", "Tem matrícula", "Receita base", "Outras receitas", "Despesas")

# formato long
indice_long <- indice %>%
  filter(status == "Ok") %>%
  select(
    orgao:nao_requer_captcha,
    acesso = acesso_i,
    formato_consistente,
    estritamente_tabular
  ) %>%
  pivot_longer(-c(orgao, ano, mes), names_to = "criterio", values_to = "pontuacao") %>%
  mutate(
    criterio = criterio %>%
      str_replace_all("_", " ") %>%
      str_to_sentence() %>%
      str_replace("matricula", "matrícula") %>%
      str_replace("lotacao", "lotação"),
    dimensao = if_else(criterio %in% criterios, "Completude", "Facilidade"),
    status = if_else(is.na(pontuacao), "Órgão não prestou contas", "Ok"),
    pontuacao = replace_na(pontuacao, 0)
  )

glimpse(indice_long)
saveRDS(indice_long, here("data/load/indice-long.rds"))

# Salva uma cópia no GoogleSheets
library(googlesheets4)

url_sheet <- "https://docs.google.com/spreadsheets/d/1zgmCoyDUd99Uf0uYGFzQb_tybOvteulIPcmLpTqQOIU"

indice_long %>%
  group_by(orgao, criterio, dimensao, status) %>%
  summarise(pontuacao = mean(pontuacao), .groups = "drop") %>%
  arrange(dimensao, orgao, criterio, dimensao, status) #%>%
  #write_sheet(url_sheet, sheet = "Analítico")