# Taxas referenciais

# http://www.b3.com.br/pt_br/market-data-e-indices/servicos-de-dados/market-data/consultas/mercado-de-derivativos/precos-referenciais/taxas-referenciais-bm-fbovespa/

### Carregando pacotes

library(glue)
library(xml2)
library(stringr)
library(bizdays)

dipre <- function() {
  name <- "dipre"
  ticker <- "PRE"
  url <- "http://www2.bmf.com.br/pages/portal/bmfbovespa/lumis/lum-taxas-referenciais-bmf-ptBR.asp"

  obj <- list(name = name, ticker = ticker, url = url)
  structure(obj, class = c(name, "rates"))
}

get_from_b3 <- function(obj, ...) {
  UseMethod("get_from_b3", obj)
}

get_from_b3.dipre <- function(obj, refdate, ...) {
  res <- tryCatch(
    {
      url <- glue("{obj$url}?Data={format(refdate, '%d/%m/%Y')}&Data1={format(refdate, '%Y%m%d')}&slcTaxa={obj$ticker}")
      doc <- read_html(url)
      tbl <- xml_find_all(doc, "//table[contains(@id, 'tb_principal1')]")

      num <- xml_find_all(tbl[[1]], "td") %>%
      xml_text() %>%
      str_trim() %>%
      str_replace(",", ".") %>%
      as.numeric()

      dc <- num[c(TRUE, FALSE, FALSE)]
      taxas <- num[c(FALSE, TRUE, FALSE)]

      200
    },
    error = function(cond) {
      message(paste("Couldn't get data from URL:", url))
      message("Here's the original error message:")
      message(cond)
      return(NA)
    }
  )
  return(list(dc = dc, taxas = taxas))
}