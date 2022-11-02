# use libs
library(tidyverse)
library(jsonlite)
library(lubridate)

# USO DA API -------------------------------------------------------------------

# Função para pegar dados da API
get_data <- function(aid, ano) {
  message(str_glue("get {aid} {ano}"))
  Sys.sleep(.3)
  fromJSON(
    # url-base
    str_glue("https://api.dadosjusbr.org/v1/dados/{aid}/{ano}")
  )
}

# safely mode
get_data_safe <- safely(get_data)

# Lista de órgãos categorizados por grupo e subgrupo
orgaos <- "https://api.dadosjusbr.org/v1/orgaos" %>%
  fromJSON() %>%
  as_tibble()

# PACOTE DE DADOS --------------------------------------------------------------

# coleta o pacote de dados para cada orgao
pacote_de_dados <- orgaos %>%
  select(aid, name, type, entity, uf) %>%
  crossing(ano = 2018:2022, .) %>%
  arrange(aid, ano) %>%
  mutate(a1 = map2(aid, ano, get_data_safe)) %>%
  unnest(a1) %>%
  group_by(aid, ano) %>%
  mutate(tipo = c("df", "error")) %>%
  ungroup()

# graças ao safely mode é possível acessar os erros de coleta, caso existam
erros_de_coleta <- pacote_de_dados %>%
  filter(tipo == "error") %>%
  mutate(erro_de_coleta = !map_lgl(a1, is.null))

# AGRUPA ÓRGÃOS ----------------------------------------------------------------

# extrai dados de índice do pacote de dados
indices <- pacote_de_dados %>%
  filter(tipo != "error") %>%
  select(-ano, -tipo) %>%
  unnest(a1) %>%
  mutate(
    
    # cria um grupo mais amplo para separar órgãos
    grupo = case_when(
      str_detect(aid, "^tj") ~ "Tribunal de Justiça",
      str_detect(aid, "^mp[^f$]") ~ "Ministério Público",
      str_detect(aid, "^tr") ~ "Tribunal Regional",
      TRUE ~ "Órgãos superiores"
    ),
    
    # cria subgrupos mais granulares
    subgrupo = case_when(
      grupo == "Tribunal de Justiça" & type == "Estadual" ~ "Tribunais de Justiça estaduais",
      grupo == "Tribunal de Justiça" & type != "Estadual" & str_detect(name, "Militar") ~ "Tribunais de Justiça Militar",
      grupo == "Ministério Público" ~ "Ministérios Públicos estaduais",
      grupo == "Tribunal Regional" & uf == "Federal" ~  "Tribunais Regionais Federais",
      grupo == "Tribunal Regional" & uf == "Trabalho" ~  "Tribunais Regionais do Trabalho",
      TRUE ~ "Órgãos superiores"
    ),
    
    # recodifica siglas de órgão (maiúscula e com hífen)
    aid = case_when(
      grupo == "Ministério Público" ~ gsub("^(mp)(.+)$", "\\1-\\2", aid),
      subgrupo == "Tribunais de Justiça estaduais" ~ gsub("^(tj)(.+)$", "\\1-\\2", aid),
      subgrupo == "Tribunais de Justiça Militar" ~ gsub("^(tjm)(.+)$", "\\1-\\2", aid),
      grupo == "Tribunal Regional" ~ gsub("^([a-z]+)(\\d{1,2})$", "\\1-\\2", aid),
      TRUE ~ aid
    ) %>% toupper()
    
  ) %>%
  select(aid, mes, ano, Meta, subgrupo) %>%
  unnest(Meta) %>%
  mutate(across(where(is.logical), replace_na, FALSE)) %>%
  complete(mes, ano, nesting(aid, subgrupo)) %>%
  mutate(
    data = my(str_glue("{mes}-{ano}")),
    periodo_invalido = if_else(
      ano == year(today()) & mes > month(today()) & day(today()) < 16,
      TRUE, FALSE
    )
  )

# PESOS ------------------------------------------------------------------------

# Atribui pesos para as categorias dos órgãos
indices <- indices %>% 
  transmute(
    
    # colunas de data e identificação de órgão
    mes, ano, data, periodo_invalido, aid, subgrupo,
    
    ##
    ## FACILIDADE
    ##
    
    # define o tipo de acesso aos dados
    acesso = case_when(
      acesso == "ACESSO_DIRETO" ~ "Acesso direto",
      acesso == "AMIGAVEL_PARA_RASPAGEM" ~ "Acesso direto",
      acesso == "RASPAGEM_DIFICULTADA" ~ "Raspagem dificultada",
      acesso == "NECESSITA_SIMULACAO_USUARIO" ~ "Necessita simulação de usuário",
      TRUE ~ "Órgão não prestou contas"
    ),
    
    # define um peso para ordenar as categorias
    acesso_wgt = case_when(
      acesso == "Acesso direto" ~ 1,
      acesso == "Raspagem dificultada" ~ 0.5,
      acesso == "Necessita simulação de usuário" ~ 0,
      acesso == "Órgão não prestou contas" ~ -1
    ),
    
    # define consistência de formato de dado
    manteve_consistencia_no_formato = case_when(
      manteve_consistencia_no_formato ~ "Manteve consistência no formato",
      !manteve_consistencia_no_formato ~ "Não manteve consistência no formato",
      TRUE ~ "Órgão não prestou contas"
    ),
    
    # define um peso para ordenar as categorias
    manteve_consistencia_no_formato_wgt = case_when(
      manteve_consistencia_no_formato == "Manteve consistência no formato" ~ 1L,
      manteve_consistencia_no_formato == "Não manteve consistência no formato" ~ 0L,
      manteve_consistencia_no_formato == "Órgão não prestou contas" ~ -1L
    ),
    
    # define tabularidade dos dados
    dados_estritamente_tabulares = case_when(
      dados_estritamente_tabulares ~ "Dados estritamente tabulares",
      !dados_estritamente_tabulares ~ "Dados não tabulares",
      TRUE ~ "Órgão não prestou contas"
    ),
    
    # define um peso para ordenar as categorias
    dados_estritamente_tabulares_wgt = case_when(
      dados_estritamente_tabulares == "Dados estritamente tabulares" ~ 1L,
      dados_estritamente_tabulares == "Dados não tabulares" ~ 0L,
      dados_estritamente_tabulares == "Órgão não prestou contas" ~ -1L
    ),
    
    # define uso de formato aberto
    extensao = if_else(is.na(extensao), "Órgão não prestou contas", extensao),
    
    # define um peso para ordenar as categorias
    extensao_wgt = case_when(
      extensao == "HTML" ~ 1L,
      extensao == "ODS" ~ 1L,
      extensao == "CSV" ~ 1L,
      extensao == "XLS" ~ 0L,
      extensao == "Órgão não prestou contas" ~ -1L
    ),
    
    # define uso de formato aberto (binário)
    formato_aberto = case_when(
      extensao %in% c("HTML", "ODS") ~ "Formato Aberto",
      extensao == "Órgão não prestou contas" ~ extensao,
      TRUE ~ "Formato Proprietário"
    ),
    
    # define um peso para ordenar as categorias
    formato_aberto_wgt = case_when(
      formato_aberto == "Formato Aberto" ~ 1L,
      formato_aberto == "Formato Proprietário" ~ 0L,
      TRUE ~ -1L
    ),
    
    ##
    ## COMPLETUDE
    ##
    
    # define presença e ausência de de matricula e nome
    tem_matricula = case_when(
      tem_matricula ~ "Possui nome e matrícula",
      !tem_matricula ~ "Não possui nome e matrícula",
      TRUE ~ "Órgão não prestou contas"
    ),
    
    # define um peso para ordenar as categorias
    tem_matricula_wgt = case_when(
      tem_matricula == "Possui nome e matrícula" ~ 1L,
      tem_matricula == "Não possui nome e matrícula" ~ 0L,
      tem_matricula == "Órgão não prestou contas" ~ -1L
    ),
    
    # define presença e ausência de cargo
    tem_cargo = case_when(
      tem_cargo ~ "Possui cargo",
      !tem_cargo ~ "Não possui cargo",
      TRUE ~ "Órgão não prestou contas"
    ),
    
    # define um peso para ordenar as categorias
    tem_cargo_wgt = case_when(
      tem_cargo == "Possui cargo" ~ 1L,
      tem_cargo == "Não possui cargo" ~ 0L,
      tem_cargo == "Órgão não prestou contas" ~ -1L
    ),
    
    # define presença e ausência de cargo
    tem_lotacao = case_when(
      tem_lotacao ~ "Possui lotação",
      !tem_lotacao ~ "Não possui lotação",
      TRUE ~ "Órgão não prestou contas"
    ),
    
    # define um peso para ordenar as categorias
    tem_lotacao_wgt = case_when(
      tem_lotacao == "Possui lotação" ~ 1L,
      tem_lotacao == "Não possui lotação" ~ 0L,
      tem_lotacao == "Órgão não prestou contas" ~ -1L
    ),
    
    # define dados de remuneração básica, despesas e outras receitas
    across(
      .cols = c(remuneracao_basica, despesas, outras_receitas), 
      .fns = ~ case_when(
        . == "DETALHADO" ~ "Dados detalhados",
        . == "SUMARIZADO" ~ "Dados sumarizados",
        . == "AUSENCIA" ~ "Dados ausentes",
        TRUE ~ "Órgão não prestou contas"
      )),
    
    # define pesos para ordenar categorias
    across(
      .cols = c(remuneracao_basica, despesas, outras_receitas),
      .names = "{.col}_wgt",
      .fns = ~ case_when(
        . == "Dados detalhados" ~ 1,
        . == "Dados sumarizados" ~ 0.5,
        . == "Dados ausentes" ~ 0,
        . == "Órgão não prestou contas" ~ -1
      )),
    
    # reordena as categorias de acordo com o peso
    acesso = fct_reorder(acesso, acesso_wgt),
    manteve_consistencia_no_formato = fct_reorder(manteve_consistencia_no_formato,
                                                  manteve_consistencia_no_formato_wgt),
    dados_estritamente_tabulares = fct_reorder(dados_estritamente_tabulares,
                                               dados_estritamente_tabulares_wgt),
    extensao = fct_reorder(extensao, extensao_wgt),
    formato_aberto = fct_reorder(formato_aberto, formato_aberto_wgt),
    tem_matricula = fct_reorder(tem_matricula, tem_matricula_wgt),
    tem_cargo = fct_reorder(tem_cargo, tem_cargo_wgt),
    tem_lotacao = fct_reorder(tem_lotacao, tem_lotacao_wgt),
    remuneracao_basica = fct_reorder(remuneracao_basica, remuneracao_basica_wgt),
    despesas = fct_reorder(despesas, despesas_wgt),
    outras_receitas = fct_reorder(outras_receitas, outras_receitas_wgt)
    
  )

saveRDS(indices, here::here(str_glue("data/load/indices-{Sys.Date()}.rds")))
