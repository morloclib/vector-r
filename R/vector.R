## Foundational 1D vector operations in base R.
## Vector maps to a 1-D R array (atomic vector); the Nat dimension is phantom
## at runtime.

morloc_zeros1 <- function(d1) array(0, dim = c(d1))
morloc_ones1  <- function(d1) array(1, dim = c(d1))
morloc_fill1  <- function(v, d1) array(v, dim = c(d1))

## Vector <-> List bridge. pack: Vector (atomic R vector) -> List (R list).
## unpack: List -> Vector. Only collapse to an atomic vector when every
## element is a length-1 atomic scalar; otherwise keep the boxed list so
## that nested element types (tuples, records, nested vectors) survive
## the round-trip without unlist() flattening their structure.
morloc_packVector <- function(x) as.list(x)
morloc_unpackVector <- function(x) {
  if (length(x) == 0) return(unlist(x))
  if (all(vapply(x, function(y) is.atomic(y) && length(y) == 1L, logical(1)))) {
    return(unlist(x, use.names = FALSE))
  }
  x
}

## Typeclass-method backends for Vector. Element-wise apply preserves the
## R atomic-vector form for primitive cases; otherwise the result falls
## back to a list and downstream consumers handle the boxing.
morloc_vec_concat <- function(xs, ys) c(xs, ys)

morloc_vec_map <- function(f, xs) {
  if (length(xs) == 0) return(xs)
  out <- lapply(xs, f)
  if (all(vapply(out, function(y) is.atomic(y) && length(y) == 1L, logical(1)))) {
    return(unlist(out, use.names = FALSE))
  }
  out
}

morloc_vec_fold <- function(f, init, xs) {
  Reduce(f = f, x = xs, init = init, accumulate = FALSE)
}

morloc_vec_fold1 <- function(f, xs) {
  acc <- xs[[1]]
  if (length(xs) > 1) {
    for (i in 2:length(xs)) acc <- f(acc, xs[[i]])
  }
  acc
}

morloc_vec_safeFold1 <- function(f, xs) {
  if (length(xs) == 0) return(NULL)
  morloc_vec_fold1(f, xs)
}

## Structural equality for Vector. Returns a single logical (TRUE/FALSE);
## R's `==` is element-wise so we collapse with `all`. Lists (boxed
## non-atomic vectors of records / nested types) get a recursive
## identical() comparison.
morloc_vec_eq <- function(xs, ys) {
  if (length(xs) != length(ys)) return(FALSE)
  if (length(xs) == 0) return(TRUE)
  if (is.list(xs) || is.list(ys)) {
    return(identical(xs, ys))
  }
  isTRUE(all(xs == ys))
}

## Lexicographic <= on Vector. Atomic vectors get element-wise
## comparison; for ties on a prefix the shorter one is treated as less.
## Lists fall back to identical() for the equality case and a
## per-element walk otherwise.
morloc_vec_le <- function(xs, ys) {
  n <- min(length(xs), length(ys))
  if (n > 0) {
    for (i in seq_len(n)) {
      a <- if (is.list(xs)) xs[[i]] else xs[i]
      b <- if (is.list(ys)) ys[[i]] else ys[i]
      if (a < b) return(TRUE)
      if (a > b) return(FALSE)
    }
  }
  length(xs) <= length(ys)
}
