# use libs
library(tidyverse)
library(jsonlite)
library(lubridate)
library(here)

# CARREGA TABELA DE ÓRGAOS -----------------------------------------------------

# Lista de órgãos, o script abaixo pega dados da API faz um agrupamento de órgãos
# e tratamento do campo `aid`
source(here("src/01-tabela-de-orgaos.R"), encoding = "utf-8")

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

# PACOTE DE DADOS --------------------------------------------------------------

# Aqui os dados são coletados para todos os órgãos/ano/mês disponíveis no site
# cada órgão/ano/mes é uma linha da tabela e quando um órgão/ano/mês não é
# coletado nós mantemos a linha na tabela juntamente com o erro de coleta
# ATENÇÃO: tempo de processamento/coleta desses dados da API pode demorar até 1h
{
  ini <- now()
  message(str_glue("Início: {ini}"))

  pacote_de_dados <- orgaos %>%
    select(id_orgao, name, type, entity, uf, agropamento) %>%
    crossing(ano = 2018:2022, .) %>%
    crossing(mes = 1:12L, .) %>%
    arrange(id_orgao, ano, mes) %>%
    mutate(a1 = pmap(list(id_orgao, ano, mes), get_data_safe)) %>%
    unnest(a1) %>%
    group_by(id_orgao, ano, mes) %>%
    mutate(tipo = c("df", "download_error")) %>%
    ungroup()

  end <- now()
  message(str_glue("Início: {ini}"))
  message(str_glue("Fim: {end}"))
  message(ini - end)

  # guardo uma cópia para não precisar ficar consumindo a API toda hora
  saveRDS(pacote_de_dados, here::here(str_glue("data/load/pacote-de-dados-{today()}.rds")))
}

# recupero a versão mais recente de `pacote_de_dados` que salvei localmente
pacote_de_dados <- "data/load" %>%
  here() %>%
  list.files(pattern = "pacote-de-dados-202[2-3]-\\d{2}-\\d{2}", full.names = TRUE) %>%
  file.info() %>%
  rownames_to_column(var = "file") %>%
  filter(ctime == max(ctime)) %>%
  pull(file) %>%
  readRDS()

# AGRUPA ÓRGÃOS ----------------------------------------------------------------

# Apronta os dados para análisar os índices e agrupa órgãos
indices <- pacote_de_dados %>%
  pivot_wider(names_from = tipo, values_from = a1) %>%
  rename(id_ano = ano, id_mes = mes) %>%
  mutate(
    coletado = if_else(!map_lgl(df, is.null), TRUE, FALSE),
    download_error = map(download_error, as.character)
  ) %>%
  unnest(c(df, download_error), keep_empty = TRUE) %>%
  select(
    id_mes, id_ano, id_orgao = aid,
    name, type, entity, error, coletado,
    uf, mes, ano, Meta
  ) %>%
  left_join(select(orgaos, id_orgao, aid, grupo)) %>%
  unnest(Meta)

# CONTA MESES COLETADOS --------------------------------------------------------

# Faço a contagem de números de órgão/ano/mês coletados.
# Se o valor de `total_de_meses_coletado` == 0, o órgão não está incluso (ainda)
# no pipeline de coletas do DadosJusBr (não há um coletor em produção)
indices <- indices %>%
  group_by(aid) %>%
  mutate(total_de_meses_coletados = sum(coletado)) %>%
  ungroup()

# PESOS ------------------------------------------------------------------------

# Atribui pesos para as categorias dos órgãos - tratamento final da base para
# executar o cálculo do índice de transparência
indices <- indices %>%
  transmute(

    # colunas de data e identificação de órgão
    aid = aid,
    grupo = grupo,
    mes = id_mes,
    ano = id_ano,
    data = my(str_glue("{id_mes}-{id_ano}")),

    # periodo inválido para controlar balanceamento da base (sempre em intervalo de 12 meses)
    periodo_invalido = case_when(
      id_ano == year(today()) & id_mes == 12 ~ TRUE,
      id_ano == year(today()) & day(today()) < 16 ~ TRUE,
      TRUE ~ FALSE
    ),
    # periodo_invalido = if_else(
    #   id_ano == year(today()) & id_mes > month(today()) & day(today()) < 16,
    #   TRUE, FALSE
    # ),

    # se foi realida uma coleta para órgão/ano/mês
    coletado = coletado,
    total_de_meses_coletados = total_de_meses_coletados,

    ##
    ## FACILIDADE
    ##

    # define o tipo de acesso aos dados
    acesso = case_when(
      periodo_invalido ~ "Coleta fora do período",
      total_de_meses_coletados == 0 ~ "Órgão não coletado pelo DadosJusBr",
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
      acesso == "Órgão não prestou contas" ~ -1,
      acesso == "Órgão não coletado pelo DadosJusBr" ~ -2,
      acesso == "Coleta fora do período" ~ -3
    ),

    # define consistência de formato de dado
    manteve_consistencia_no_formato = case_when(
      periodo_invalido ~ "Coleta fora do período",
      total_de_meses_coletados == 0 ~ "Órgão não coletado pelo DadosJusBr",
      manteve_consistencia_no_formato ~ "Manteve consistência no formato",
      !manteve_consistencia_no_formato ~ "Não manteve consistência no formato",
      TRUE ~ "Órgão não prestou contas"
    ),

    # define um peso para ordenar as categorias
    manteve_consistencia_no_formato_wgt = case_when(
      manteve_consistencia_no_formato == "Manteve consistência no formato" ~ 1L,
      manteve_consistencia_no_formato == "Não manteve consistência no formato" ~ 0L,
      manteve_consistencia_no_formato == "Órgão não prestou contas" ~ -1L,
      manteve_consistencia_no_formato == "Órgão não coletado pelo DadosJusBr" ~ -2L,
      manteve_consistencia_no_formato == "Coleta fora do período" ~ -3L
    ),

    # define tabularidade dos dados
    dados_estritamente_tabulares = case_when(
      total_de_meses_coletados == 0 ~ "Órgão não coletado pelo DadosJusBr",
      dados_estritamente_tabulares ~ "Dados estritamente tabulares",
      !dados_estritamente_tabulares ~ "Dados não tabulares",
      TRUE ~ "Órgão não prestou contas"
    ),

    # define um peso para ordenar as categorias
    dados_estritamente_tabulares_wgt = case_when(
      dados_estritamente_tabulares == "Dados estritamente tabulares" ~ 1L,
      dados_estritamente_tabulares == "Dados não tabulares" ~ 0L,
      dados_estritamente_tabulares == "Órgão não prestou contas" ~ -1L,
      dados_estritamente_tabulares == "Órgão não coletado pelo DadosJusBr" ~ -2L,
      dados_estritamente_tabulares == "Coleta fora do período" ~ -3L
    ),

    # define uso de formato aberto
    extensao = case_when(
      periodo_invalido ~ "Coleta fora do período",
      total_de_meses_coletados == 0 ~ "Órgão não coletado pelo DadosJusBr",
      is.na(extensao) ~ "Órgão não prestou contas",
      TRUE ~ extensao
    ),

    # define um peso para ordenar as categorias
    extensao_wgt = case_when(
      extensao == "HTML" ~ 1L,
      extensao == "ODS" ~ 1L,
      extensao == "CSV" ~ 1L,
      extensao == "XLS" ~ 0L,
      extensao == "Órgão não prestou contas" ~ -1L,
      extensao == "Órgão não coletado pelo DadosJusBr" ~ -2L,
      extensao == "Coleta fora do período" ~ -3L
    ),

    # define uso de formato aberto (binário)
    formato_aberto = case_when(
      periodo_invalido ~ "Coleta fora do período",
      total_de_meses_coletados == 0 ~ "Órgão não coletado pelo DadosJusBr",
      extensao %in% c("HTML", "ODS") ~ "Formato Aberto",
      extensao == "Órgão não prestou contas" ~ extensao,
      TRUE ~ "Formato Proprietário"
    ),

    # define um peso para ordenar as categorias
    formato_aberto_wgt = case_when(
      formato_aberto == "Coleta fora do período" ~ -3L,
      formato_aberto == "Órgão não coletado pelo DadosJusBr" ~ -2L,
      formato_aberto == "Formato Aberto" ~ 1L,
      formato_aberto == "Formato Proprietário" ~ 0L,
      TRUE ~ -1L
    ),

    ##
    ## COMPLETUDE
    ##

    # define presença e ausência de de matricula e nome
    tem_matricula = case_when(
      periodo_invalido ~ "Coleta fora do período",
      total_de_meses_coletados == 0 ~ "Órgão não coletado pelo DadosJusBr",
      tem_matricula ~ "Possui nome e matrícula",
      !tem_matricula ~ "Não possui nome e matrícula",
      TRUE ~ "Órgão não prestou contas"
    ),

    # define um peso para ordenar as categorias
    tem_matricula_wgt = case_when(
      tem_matricula == "Possui nome e matrícula" ~ 1L,
      tem_matricula == "Não possui nome e matrícula" ~ 0L,
      tem_matricula == "Órgão não prestou contas" ~ -1L,
      tem_matricula == "Órgão não coletado pelo DadosJusBr" ~ -2L,
      tem_matricula == "Coleta fora do período" ~ -3L
    ),

    # define presença e ausência de cargo
    tem_cargo = case_when(
      periodo_invalido ~ "Coleta fora do período",
      total_de_meses_coletados == 0 ~ "Órgão não coletado pelo DadosJusBr",
      tem_cargo ~ "Possui cargo",
      !tem_cargo ~ "Não possui cargo",
      TRUE ~ "Órgão não prestou contas"
    ),

    # define um peso para ordenar as categorias
    tem_cargo_wgt = case_when(
      tem_cargo == "Possui cargo" ~ 1L,
      tem_cargo == "Não possui cargo" ~ 0L,
      tem_cargo == "Órgão não prestou contas" ~ -1L,
      tem_cargo == "Órgão não coletado pelo DadosJusBr" ~ -2L,
      tem_cargo == "Coleta fora do período" ~ -3L
    ),

    # define presença e ausência de cargo
    tem_lotacao = case_when(
      periodo_invalido ~ "Coleta fora do período",
      total_de_meses_coletados == 0 ~ "Órgão não coletado pelo DadosJusBr",
      tem_lotacao ~ "Possui lotação",
      !tem_lotacao ~ "Não possui lotação",
      TRUE ~ "Órgão não prestou contas"
    ),

    # define um peso para ordenar as categorias
    tem_lotacao_wgt = case_when(
      tem_lotacao == "Possui lotação" ~ 1L,
      tem_lotacao == "Não possui lotação" ~ 0L,
      tem_lotacao == "Órgão não prestou contas" ~ -1L,
      tem_lotacao == "Órgão não coletado pelo DadosJusBr" ~ -2L,
      tem_lotacao == "Coleta fora do período" ~ -3L
    ),

    # define dados de remuneração básica, despesas e outras receitas
    across(
      .cols = c(remuneracao_basica, despesas, outras_receitas),
      .fns = ~ case_when(
        periodo_invalido ~ "Coleta fora do período",
        total_de_meses_coletados == 0 ~ "Órgão não coletado pelo DadosJusBr",
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
        . == "Órgão não prestou contas" ~ -1,
        . == "Órgão não coletado pelo DadosJusBr" ~ -2,
        . == "Coleta fora do período" ~ -3
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

saveRDS(indices, here::here(str_glue("data/load/indices-{today()}.rds")))