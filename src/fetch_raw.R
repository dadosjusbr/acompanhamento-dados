library(tidyverse)


# exemplo de url de mês: 
# https://cloud5.lsd.ufcg.edu.br:8080/swift/v1/dadosjusbr/tjrj/tjrj-2018.zip

url_base = "https://cloud5.lsd.ufcg.edu.br:8080/swift/v1/dadosjusbr"
anos = 2018:2021
orgaos = c("tjpb", "tjrj")

diretorio = here::here("data/raw")
dir.create(diretorio, showWarnings = F)

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

walk2(alvo$dest, alvo$data_dir, ~ unzip(.x, exdir = .y))

# Cruza e apronta

cols_remuneracao <- cols(
  Valor = col_double(),
  .default = col_character()
)

cols_contra_cheque <- cols(
  Ativo = col_logical(),
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
  map_df(~ read_csv(.x, col_types = cols_contra_cheque)) 

remuneracoes <- list.files(path = alvo$data_dir,
                           pattern = "remuneracao",
                           full.names = T) %>%
  read_csv(col_types = cols_remuneracao)

coletas <- list.files(path = alvo$data_dir,
                      pattern = "coleta",
                      full.names = T) %>%
  read_csv(col_types = cols(.default = col_character())) 

coletas = coletas %>% 
  select(chave_coleta, orgao, mes, ano) %>% 
  mutate(data = as.Date(str_glue("{ano}-{mes}-01")))

metadados <- list.files(path = alvo$data_dir,
                         pattern = "metadados",
                         full.names = T) %>%
  read_csv(col_types = cols_metadados)

incomes = remuneracoes %>%
  left_join(contra_cheque, by = c("IdContraCheque", "ChaveColeta")) %>%
  janitor::clean_names() %>%
  left_join(coletas, by = "chave_coleta") %>% 
  mutate(
    natureza = if_else(natureza == "D", "Desconto", "Recebimento")
  ) 

incomes %>% 
  write_csv(here::here("data/ready/remuneracoes-contracheques.csv"))

metadados %>% 
  left_join(coletas, by = "chave_coleta") %>% 
  write_csv(here::here("data/ready/metadados.csv"))
  