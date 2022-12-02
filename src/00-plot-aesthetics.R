# peguei as cores do site e criei um vetor com nomes fáceis de lembrar
cores_dadosjusbr <- c(
  lilas = "#B361C6",
  cyan = "#2FBB96",
  cinza_azulado = "#3e5363",
  verde = "#96ba2f",
  laranja = "#F2C94C",
  cinza_claro = "#F5F5F5"
)

itens_categoria <- c(
  # - Facilidade:
  "login_nao_necessario" = "Requer login",
  "captcha_nao_necessario" = "Necessário captcha",
  "acesso" = "Acesso",
  "manteve_consistencia_no_formato" = "Manteve consistência no formato",
  "dados_estritamente_tabulares" = "Formato estritamente tabular",
  "extensao" = "Extensão",
  "formato_aberto" = "Formato Aberto",
  # - Completude:
  "tem_matricula" = "Possui nome e matrícula",
  "tem_lotacao" = "Possui lotação",
  "tem_cargo" = "Possui cargo",
  "remuneracao_basica" = "Dados de remuneração básica",
  "despesas" = "Dados de despesas",
  "outras_receitas" = "Dados de outras remunerações"
)

# cores para índices
cores_indice <- c(
  "Transparência" = cores_dadosjusbr[["cyan"]],
  "Completude" = cores_dadosjusbr[["laranja"]],
  "Facilidade" = cores_dadosjusbr[["lilas"]]
)

# cores para classificar se requer login
cores_login_nao_necessario <- c(
  "Não requer login" = cores_dadosjusbr[["cyan"]],
  "Requer login" = cores_dadosjusbr[["lilas"]],
  "Órgão não prestou contas" = cores_dadosjusbr[["cinza_azulado"]]
)

# cores para classificar necessidade de captcha
cores_captcha_nao_necessario <- c(
  "Não requer captcha" = cores_dadosjusbr[["cyan"]],
  "Requer captcha" = cores_dadosjusbr[["lilas"]],
  "Órgão não prestou contas" = cores_dadosjusbr[["cinza_azulado"]]
)

# cores para classificar acesso
cores_acesso <- c(
  "Acesso direto" = cores_dadosjusbr[["verde"]],
  # "Amigável para raspagem" = cores_dadosjusbr[["cyan"]],
  "Raspagem dificultada" = cores_dadosjusbr[["cyan"]],
  "Necessita simulação de usuário" = cores_dadosjusbr[["lilas"]],
  "Órgão não prestou contas" = cores_dadosjusbr[["cinza_azulado"]]
)

# cores se manteve consistência no formato
cores_manteve_consistencia_no_formato <- c(
  "Manteve consistência no formato" = cores_dadosjusbr[["cyan"]],
  "Não manteve consistência no formato" = cores_dadosjusbr[["lilas"]],
  "Órgão não prestou contas" = cores_dadosjusbr[["cinza_azulado"]]
)

# cores para tabularidade dos dados
cores_dados_estritamente_tabulares <- c(
  "Dados estritamente tabulares" = cores_dadosjusbr[["cyan"]],
  "Dados não tabulares" = cores_dadosjusbr[["lilas"]],
  "Órgão não prestou contas" = cores_dadosjusbr[["cinza_azulado"]]
)

# cores para presença de nome e matricula
cores_tem_matricula <- c(
  "Possui nome e matrícula" = cores_dadosjusbr[["cyan"]],
  "Não possui nome e matrícula" = cores_dadosjusbr[["lilas"]],
  "Órgão não prestou contas" = cores_dadosjusbr[["cinza_azulado"]]
)

# cores para presença de lotação de membro
cores_tem_lotacao <- c(
  "Possui lotação" = cores_dadosjusbr[["cyan"]],
  "Não possui lotação" = cores_dadosjusbr[["lilas"]],
  "Órgão não prestou contas" = cores_dadosjusbr[["cinza_azulado"]]
)

# cores para presença de cargo de membro
cores_tem_cargo <- c(
  "Possui cargo" = cores_dadosjusbr[["cyan"]],
  "Não possui cargo" = cores_dadosjusbr[["lilas"]],
  "Órgão não prestou contas" = cores_dadosjusbr[["cinza_azulado"]]
)

# cores para dados de remuneracao básica, outras receitas ou despesas
cores_remuneracoes <- c(
  "Dados detalhados" = cores_dadosjusbr[["cyan"]],
  "Dados sumarizados" = cores_dadosjusbr[["laranja"]],
  "Dados ausentes" = cores_dadosjusbr[["lilas"]],
  "Órgão não prestou contas" = cores_dadosjusbr[["cinza_azulado"]]
)

# cores para formato/extensão dos dados
cores_extensao <- c(
  "CSV" = cores_dadosjusbr[["cyan"]],
  "HTML" = cores_dadosjusbr[["laranja"]],
  "ODS" = cores_dadosjusbr[["verde"]],
  "XLS" = cores_dadosjusbr[["lilas"]],
  "Órgão não prestou contas" = cores_dadosjusbr[["cinza_azulado"]]
)

# cores ára formato aberto ou proprietário
cores_formato_aberto <- c(
  "Formato Aberto" = cores_dadosjusbr[["cyan"]],
  "Formato Proprietário" = cores_dadosjusbr[["lilas"]],
  "Órgão não prestou contas" = cores_dadosjusbr[["cinza_azulado"]]
)

# cores se órgão presou contas ou não
cores_prestou_contas <- c(
  "Órgão prestou contas" = cores_dadosjusbr[["laranja"]],
  "Órgão não prestou contas" = cores_dadosjusbr[["cinza_azulado"]],
  "Coleta não realizada" = cores_dadosjusbr[["cinza_claro"]]
)

# uso de fontes e plot-aesthetics
hrbrthemes::import_roboto_condensed()
extrafont::loadfonts()
ggplot2::theme_set(hrbrthemes::theme_ipsum_rc())

ggplot2::theme_update(
  axis.ticks.x = ggplot2::element_line(),
  plot.title = ggplot2::element_text(color = "black", size = 14),
  plot.subtitle = ggplot2::element_text(color = cores_dadosjusbr[["cinza_azulado"]]),
  plot.background = ggplot2::element_rect(fill = cores_dadosjusbr[["cinza_claro"]], color = "transparent"),
  axis.text.x = ggplot2::element_text(color = cores_dadosjusbr[["cinza_azulado"]], face = "bold"),
  axis.text.y = ggplot2::element_text(color = cores_dadosjusbr[["cinza_azulado"]], face = "bold"),
  axis.title.x = ggplot2::element_text(color = "black", face = "bold"),
  axis.title.y = ggplot2::element_text(color = "black", face = "bold"),
  legend.text = ggplot2::element_text(color = "black", face = "bold"),
  legend.title = ggplot2::element_text(color = "black", face = "bold"),
  strip.text = ggplot2::element_text(color = cores_dadosjusbr[["cinza_azulado"]], face = "bold"),
  panel.background = ggplot2::element_blank(),
  text = ggplot2::element_text(family = "Roboto Condensed")
)

hrbrthemes::update_geom_font_defaults(color = cores_dadosjusbr[["cinza_azulado"]],
                                      family = "Roboto Condensed")

theme_adjust_waffle = ggplot2::theme(
  legend.position = "top",
  legend.direction = "vertical",
  legend.justification = "left",
  panel.grid.major = ggplot2::element_blank(),
  panel.grid.minor = ggplot2::element_blank(),
  panel.spacing = ggplot2::unit(2, "mm")
)
