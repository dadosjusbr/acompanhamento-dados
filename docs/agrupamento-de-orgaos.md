# Agrupamento de órgãos

[PLANILHA COM AGRUPAMENTO](https://docs.google.com/spreadsheets/d/1nj4YEt8SycovFMGIRhEqy4fVAEftVS0CRg25iyDrvZc/edit#gid=1619472317)

## Conselhos de Justiça

* 2 órgãos:  `cjf`,  `cnj`

```r
id_orgao %in% c("cjf", "cnj")
```

## Justiça do Trabalho

* **24 órgãos**: todos os tribunais regionais do trabalho. Exemplo: `trt1`, `trt2`,  `trt3` ...
* ⚠️ `tst` é um Tribunal Superior

```r
type == "Trabalho" & aid != "tst"
```

## Justiça Eleitoral

* **27 órgãos:** todos os tribunais eleitorais estaduais mais `tredft`. Ex: `tresp`, `treto`, `treal`, ...
* ⚠️ `tse` é um Tribunal Superior

```R
type == "Eleitoral" & aid != "tse"
```

## Justiça Estadual

* **27 órgãos**: todos os tribunais de justiça estaduais mais `tjdft`. Ex: `tjms`,  `tjpr`, `tjam`, ...
* ⚠️ `stj` é um Tribunal Superior

```r
type == "Estadual" & type == "Tribunal"
```

## Justiça Federal

* **5 órgãos**: todos os Tibunais Regionais Federais (`trf1`, `trf2`, `trf3`, `trf4`, `trf5`).

```r
aid %in% c("trf1", "trf2", "trf3", "trf4", "trf5")
```

## Justiça Militar

* **3 órgãos**: todos os Tribunais de Justiça Militar estaduais mais `tjmdft`. Ex: `tjmmg`, `tjmrs`, `tjmsp`, ...
* ⚠️ `stm` é um Tribunal Superior

```r
type == "Militar" & aid != "stm"
```

## Ministério Público

* **29 órgãos:** todos os ministérios públicos estaduais mais `mpdft`,  `mpf` e `mpt`. Ex: `mppe`, `mprs`, `mppb`, ..., `mpf`, `mpt`.

```r
entity == "Ministério"
```

## Tribunais Superiores

* **5 órgãos**: `stf`, `stj`**,** `stm`, `tse`, `tst`.

```r
aid %in% c("stf", "stj", "stm", "tse", "tst")
```
