registry <- new.env(parent = emptyenv())

compact <- function(x) {
  if (!is_bare_list(x)) return(x)
  x[lengths(x) != 0]
}
