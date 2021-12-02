# Análises com dados do DadosJusBR no leiaute v2

Validação e EDA dos dados libertados no projeto no leiaute v2. 

## Para gerar csvs com todos os dados que temos libertados

```
./src/fetch_raw.R
```

## Para desenvolver

Dados brutos vão em `dados/raw`, e prontos em `dados/ready`. Outros dados que nós criamos manualmente e usamos para criar o ready vão em `dados/input`. 

Código para obter dados (e colocá-los em `dados/raw`) e transformar dados (colocando-os e `dados/ready`), assim como funções reusáveis vão em `src/`. 

Relatórios que usam dados prontos (`dados/ready`) ficam em `reports/`. Coloque o html de versões para publicação em `docs/` e eles estarão disponíveis em https://dadosjusbr.github.io/acompanhamento-dados/. Não coloque o html dos relatórios em `reports/`. 
