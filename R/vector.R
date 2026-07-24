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

morloc_vec_size <- function(xs) length(xs)

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

## Element access via Indexable. Morloc passes 0-based indices.
## Index arrives as ?Int64 to match __to_index__'s return shape; a NULL
## index has no semantic meaning at runtime. Negative indices wrap from
## the end (Python semantics), matching root-r's morloc_at.
morloc_vec_at <- function(i, xs) {
  if (is.null(i)) stop("morloc_vec_at: index is NULL")
  if (i < 0) i <- i + length(xs)
  idx <- i + 1L
  if (is.list(xs)) xs[[idx]] else xs[idx]
}

## Python-style slice with optional bounds. start/stop/step may each be
## NULL (passed as morloc Null). step 0 is a runtime error per the
## Sliceable contract. Mirrors root-r's morloc_slice in arithmetic
## style (numeric not integer, seq not seq.int, xs[0] for empty) since
## that path is exercised heavily by the List tests and has shaken out
## R's empty-vector subscript quirks.
morloc_vec_slice <- function(start, stop, step, xs) {
  n <- as.numeric(length(xs))
  k <- if (is.null(step)) 1 else as.numeric(step)
  if (k == 0) stop("slice step cannot be zero")
  if (k > 0) {
    i <- if (is.null(start)) 0 else as.numeric(start)
    j <- if (is.null(stop))  n else as.numeric(stop)
  } else {
    i <- if (is.null(start)) n - 1 else as.numeric(start)
    j <- if (is.null(stop))  -1    else as.numeric(stop)
  }
  if (i < 0 && !is.null(start)) i <- i + n
  if (j < 0 && !is.null(stop))  j <- j + n
  if (k > 0) {
    i <- max(0, min(i, n))
    j <- max(0, min(j, n))
  } else {
    i <- max(-1, min(i, n - 1))
    j <- max(-1, min(j, n - 1))
  }
  idx <- if (k > 0) {
    if (i >= j) numeric(0) else seq(i, j - 1, by = k)
  } else {
    if (i <= j) numeric(0) else seq(i, j + 1, by = k)
  }
  if (length(idx) == 0) {
    xs[0]
  } else {
    xs[idx + 1]
  }
}
