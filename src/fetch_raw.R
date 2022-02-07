library(tidyverse)

# exemplo de url de mês: 
# https://cloud5.lsd.ufcg.edu.br:8080/swift/v1/dadosjusbr/tjrj/tjrj-2018.zip

url_base = "https://cloud5.lsd.ufcg.edu.br:8080/swift/v1/dadosjusbr"
anos = 2018:2021

# estou pegando a lista de órgãos disponíveis daqui:
# https://docs.google.com/spreadsheets/d/1sIebyMnsFMwGnUCZiQ6d_dIvPmvLtcFhszZnMU_QSTY/edit#gid=0
orgaos = "1sIebyMnsFMwGnUCZiQ6d_dIvPmvLtcFhszZnMU_QSTY" %>% 
  googlesheets4::read_sheet(sheet = "Estaduais e do DFT", range = "A:E") %>%
  filter(str_detect(Órgão, "^TJ"), status == "Ok") %>% 
  pull(Órgão) %>% 
  tolower()

# orgaos = c("tjsp", "tjrj", "tjro", "tjrs", "tjpa", "tjsc", "tjdft", "tjmt",
#            "tjes", "tjms", "tjam", "tjma", "tjto", "tjap", "tjac", "tjrr", 
#            "tjce", "tjrn", "tjpb", "tjal", "tjpi", "tjba", "tjpe", "tjgo",
#            "tjse", "tjpr", "tjmg")

diretorio = here::here("data/raw")
dir.create(diretorio, showWarnings = F, recursive = T)

alvo = expand.grid(orgao = orgaos,
                   ano = anos,
                   stringsAsFactors = F) %>% 
  mutate(url = str_glue("{url_base}/{orgao}/{orgao}-{ano}.zip"), 
         dest = str_glue("{diretorio}/{orgao}-{ano}.zip"), 
         data_dir = str_glue("{diretorio}/{orgao}-{ano}"))

# Download

safe_download <- safely(~ download.file(.x , .y, mode = "wb"))
walk2(alvo$url, alvo$dest, safe_download)

# Unzip

safe_unzip <- safely(~ unzip(.x, exdir = .y))
walk2(alvo$dest, alvo$data_dir, safe_unzip)

# Cruza e apronta

cols_remuneracao <- cols(
  .default = col_character()
)

cols_contra_cheque <- cols(
  .default = col_character()
)

cols_metadados <- cols(
  chave_coleta = col_character(),
  acesso = col_character(),
  extensao = col_character(),
  detalhamento_receita_base = col_character(),
  detalhamento_outras_receitas = col_character(),
  detalhamento_descontos = col_character()
)

contra_cheque <- list.files(path = alvo$data_dir,
                            pattern = "contra_cheque",
                            full.names = T) %>%
  map_df(~ read_csv(.x, col_types = cols_contra_cheque)) %>% 
  mutate(ativo = if_else(ativo == "true", TRUE, FALSE))

remuneracoes <- list.files(path = alvo$data_dir,
                           pattern = "remuneracao",
                           full.names = T) %>%
  map_df(
    ~ .x %>%
      read_csv(col_types = cols_remuneracao) %>% 
      filter(valor != "valor") %>% 
      mutate(valor = as.numeric(valor))
  )

coletas <- list.files(path = alvo$data_dir,
                      pattern = "coleta",
                      full.names = T) %>%
  map_df(~ read_csv(.x, col_types = cols(.default = col_character()))) 

coletas = coletas %>% 
  select(chave_coleta, orgao, mes, ano) %>% 
  mutate(data = as.Date(str_glue("{ano}-{mes}-01")))

metadados <- list.files(path = alvo$data_dir,
                        pattern = "metadados",
                        full.names = T) %>%
  map_df(~ read_csv(.x, col_types = cols_metadados))

incomes = remuneracoes %>%
  left_join(contra_cheque, by = c("id_contra_cheque", "chave_coleta")) %>%
  left_join(coletas, by = "chave_coleta") %>% 
  mutate(
    natureza = if_else(natureza == "D", "Desconto", "Recebimento")
  )

diretorio = dir.create(here::here("data/ready"), recursive = T)
incomes %>% 
  saveRDS(here::here("data/ready/remuneracoes-contracheques.csv"))

metadados %>% 
  left_join(coletas, by = "chave_coleta") %>% 
  write_csv(here::here("data/ready/metadados.csv"))

  