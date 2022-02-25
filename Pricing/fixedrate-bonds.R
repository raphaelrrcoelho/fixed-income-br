library(tidyverse)
library(bizdays)
library(GetTDData)
source("Utils/rendafixa-compounding.R")
source("Utils/rendafixa-daycount.R")
source("Utils/rendafixa-spotrate.R")

ltn <- function() {
  name <- "ltn"

  obj <- list(
    name = name,
    ticker = "LTN",
    url = "https://www.tesourodireto.com.br/titulos/precos-e-taxas.htm",
    data_folder = "Data/TD Files/LTN/",
    calendar = "Brazil/ANBIMA",
    par_value = 1000
  )

  structure(obj, class = c(name, "fixedratebond"))
}

ntnf <- function() {
  name <- "ntnf"

  obj <- list(
    name = name,
    ticker = "NTN-F",
    url = "https://www.tesourodireto.com.br/titulos/precos-e-taxas.htm",
    data_folder = "Data/TD Files/NTNF/",
    calendar = "Brazil/ANBIMA",
    par_value = 1000
  )

  structure(obj, class = c(name, "fixedratebond"))
}

read_td_files <- function(obj, ...) {
  UseMethod("read_td_files", obj)
}

price_from_rate <- function(obj, ...) {
  UseMethod("price_from_rate", obj)
}

filter_by_refdates <- function(obj, ...) {
  UseMethod("filter_by_refdates", obj)
}

get_cashflow <- function(obj, ...) {
  UseMethod("get_cashflow", obj)
}

get_td_files <- function(obj, ...) {
  UseMethod("get_td_files", obj)
}

get_td_files.fixedratebond <- function(obj, refdate, ...) {
  res <- tryCatch(
    {
      download.TD.data(asset.codes = obj$ticker, dl.folder = obj$data_folder)
    },
    error = function(cond) {
      message("Couldn't get data from Tesouro Direto")
      message("Here's the original error message:")
      message(cond)
      return(NA)
    }
  )
}

read_td_files.fixedratebond <- function(obj, maturities, ...) {
  read.TD.files(
    dl.folder = obj$data_folder,
    asset.codes = obj$ticker,
    maturity = maturities
  ) %>%
  mutate(
    VencimentoAdj = following(matur.date, obj$calendar),
    ParValue = obj$par_value
  )
}

filter_by_refdates.fixedratebond <- function(obj, df, refdates, ...) {
  refdates <- c(0, refdates %>% following(obj$calendar))

  df %>%
    filter(ref.date %in% refdates) %>%
    mutate(DU = bizdays(ref.date, matur.date, obj$calendar))
}

price_from_rate.ltn <- function(obj, bonds, ...) {
  rate <- spotrate(bonds$yield.bid, discrete(), business252(), ltn$calendar)
  bonds %>%
    mutate(
      DU = bizdays(ref.date, matur.date, rate$calendar),
      ParValue = obj$par_value,
      PresentValue = ParValue / compound(rate, DU, "days")
    )
}

price_from_rate.ntnf <- function(obj, bonds, ...) {
  cashflows <- apply(bonds, 1, get_cashflow, obj=obj, simplify = FALSE)

  bonds <- bonds %>%
    mutate(
      PresentValue = lapply(cashflows, function(x) sum(x$PresentValue))
    )
}

get_cashflow.ntnf <- function(obj, bond, ...) {
  coupon_rate <- 0.1 # 10% p.a.
  matur_date = as.Date(bond[["matur.date"]])
  ref_date = as.Date(bond[["ref.date"]])
  yield_bid = as.numeric(bond[["yield.bid"]])

  dates <- following(seq(matur_date, ref_date, by = "-6 months"), obj$calendar) %>% sort()
  n <- length(dates)
  r <- spotrate(rep(coupon_rate, n), discrete(), business252(), obj$calendar)

  cf <- tibble(
    DataRef = ref_date,
    Vencimento = dates,
  )

  cf <- cf %>%
    mutate(
      DU = bizdays(DataRef, Vencimento, obj$calendar),
      InterestRate = compound(r, 6, "months") - 1,
      Coupons = obj$par_value * InterestRate,
      Principal = c(rep(0, n-1), obj$par_value),
      Payments = Coupons + Principal
    )

  y <- spotrate(yield_bid, discrete(), business252(), obj$calendar)

  cf %>%
    mutate(
      PresentValue = Payments / compound(y, DU, "days")
    )
}