
business252 <- function() {
  name <- "business/252"
  obj <- list(name = name, dib = 252)
  structure(obj, class = c(name, "daycount"))
}

actual360 <- function() {
  name <- "actual/360"
  obj <- list(name = name, dib = 360)
  structure(obj, class = c(name, "daycount"))
}

print.daycount <- function(x, ...) {
  msg <- sprintf("<daycount %s>", x$name)
  cat(msg, "\n")
}

timefactor <- function(obj, ...) {
  UseMethod("timefactor", obj)
}

timefactor.daycount <- function(obj, term, units = c("years", "months", "days"), ...) {
  units <- match.arg(units)
  if (units == "days") {
    term / obj$dib
  } else if (units == "months") {
    term / 12
  } else if (units == "years") {
    term
  } else {
    stop("Incorrect units")
  }
}

timefactor.default <- function(obj, term, units = c("years", "months"), ...) {
  units <- match.arg(units)
  if (units == "months") {
    term / 12
  } else if (units == "years") {
    term
  } else {
    stop("Incorrect units")
  }
}
