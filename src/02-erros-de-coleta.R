# use libs
library(tidyverse)
library(jsonlite)
library(lubridate)
library(here)
library(glue)
`%notin%` <- function(x, y) !(x %in% y)


source(here("src/01-orgaos-monitorados.R"), encoding = "utf-8")

orgaos <- orgaos_monitorados() %>%
  transmute(aid, orgao_monitorado = is.na(timestamp))


# recupero a versão mais recente de `pacote_de_dados` que salvei localmente
pacote_de_dados <- "data/load" %>%
  here() %>%
  list.files(pattern = "pacote-de-dados-202[2-3]-\\d{2}-\\d{2}", full.names = TRUE) %>%
  file.info() %>%
  rownames_to_column(var = "file") %>%
  filter(ctime == max(ctime)) %>%
  pull(file) %>%
  readRDS()

# Erros de download de pacote
pacote_de_dados %>%
  pivot_wider(names_from = tipo, values_from = a1) %>%
  set_names(glue("{names(.)}_pkg")) %>%
  mutate(
    pkg_coletado = if_else(!map_lgl(df_pkg, is.null), TRUE, FALSE),
    download_error_pkg = map(download_error_pkg, as.character)
  ) %>%
  left_join(orgaos, by = c("aid_pkg" = "aid")) %>%
  transmute(
    date_pkg = my(glue("{mes_pkg}-{ano_pkg}")),
    ano_pkg,
    aid_pkg = reorder(aid_pkg, pkg_coletado, sum),
    agrupamento_pkg = reorder(agrupamento_pkg, pkg_coletado, sum),
    status = case_when(
      pkg_coletado & orgao_monitorado ~ "Órgão prestou contas",
      !pkg_coletado & orgao_monitorado ~ "Órgão não prestou contas",
      !orgao_monitorado ~ "Órgão não monitorado"
    )
  ) %>%
  ggplot(aes(x = date_pkg, y = aid_pkg, fill = status)) +
  facet_grid(agrupamento_pkg ~ ano_pkg, scales = "free") +
  geom_tile()

# Apronta os dados para análisar os índices e agrupa órgãos
pacote_de_dados %>%
  pivot_wider(names_from = tipo, values_from = a1) %>%
  set_names(str_glue("{names(.)}_pkg")) %>%
  mutate(
    pkg_coletado = if_else(!map_lgl(df_pkg, is.null), TRUE, FALSE),
    download_error_pkg = map(download_error_pkg, as.character)
  ) %>%
  select(mes_pkg:agrupamento_pkg, download_error_pkg, pkg_coletado) %>%
  unnest(download_error_pkg, keep_empty = TRUE) %>%
  filter(!pkg_coletado) %>%
  filter(!is.na(download_error_pkg)) %>%
  count(download_error_pkg) %>%
  print(n = Inf)

unnest(c(df_pkg, download_error_pkg), keep_empty = TRUE) %>%
  select(contains("pkg"), error) %>%
  # filter(aid_pkg == "mprn") %>%
  unnest(error) %>%
  filter(!is.na(err_msg))




plot_categoria <- function(df, orgao, categoria, pallete) {
  df <- df %>% filter(aid %in% orgao)
  plot_title <- str_glue("{itens_categoria[[categoria]]} | {unique(df$grupo)}")
  categoria <- sym(categoria)
  df %>%
    ggplot(aes(x = data, y = aid, fill = !!categoria)) +
    facet_wrap(ano ~ ., nrow = 1, scales = "free_x") +
    geom_tile(color = "gray30") +
    scale_fill_manual(values = pallete) +
    scale_x_date(
      date_breaks = "3 months",
      date_labels = "%b",
      expand = c(0, 0)
    ) +
    theme_adjust_waffle +
    labs(
      x = "Cada célula representa 1 mês",
      y = NULL,
      title = plot_title,
      fill = NULL
    ) +
    guides(fill = guide_legend(nrow = 1, reverse = TRUE)) +
    scale_y_discrete(expand = c(0, 0)) +
    theme(
      axis.title.x = element_text(face = "italic"),
      axis.text.y = element_text(hjust = 0)
    )
}
