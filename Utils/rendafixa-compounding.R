
simple <- function() {
  name <- "simple"
  obj <- list(name = name)
  structure(obj, class = c(name, "compounding"))
}

compound <- function(obj, ...) {
  UseMethod("compound", obj)
}

compound.simple <- function(obj, rate, term, ...) {
  (1 + rate*term)
}

discrete <- function() {
  name <- "discrete"
  obj <- list(name = name)
  structure(obj, class = c(name, "compounding"))
}

compound.discrete <- function(obj, rate, term, ...) {
  (1 + rate) ** term
}

continuous <- function() {
  name <- "continuous"
  obj <- list(name = name)
  structure(obj, class = c(name, "compounding"))
}

compound.continuous <- function(obj, rate, term, ...) {
  exp(rate * term)
}

print.compounding <- function(x, ...) {
  msg <- sprintf("<compounding %s>", x$name)
  cat(msg, "\n")
  invisible(x)
}
