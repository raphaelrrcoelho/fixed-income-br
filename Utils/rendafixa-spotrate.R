
spotrate <- function(value, compounding, daycount = NULL, calendar = NULL, ...) {
  obj <- list(
    value = value,
    compounding = compounding,
    daycount = daycount,
    calendar = calendar
  )
  structure(obj, class = "spotrate")
}

print.spotrate <- function(x, ...) {
  m <- if (length(x$value) == 1) {
    sub(" +$", "", paste(x$value, x$compounding$name, x$daycount$name, x$calendar))
  } else {
    hdr <- sub(" +$", "", paste(x$compounding$name, x$daycount$name, x$calendar))
    paste(hdr, paste(x$value, collapse = " "), sep = "\n")
  }
  cat(m, "\n")
}


compound.spotrate <- function(obj, term, units=c("years", "months", "days"), ...) {
  units <- match.arg(units)
  n <- timefactor(obj$daycount, term, units)
  compound(obj$compounding, obj$value, n)
}
