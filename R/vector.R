## Foundational 1D vector operations in base R.
## Vector maps to a 1-D R array (atomic vector); the Nat dimension is phantom
## at runtime.

morloc_zeros1 <- function(d1) array(0, dim = c(d1))
morloc_ones1  <- function(d1) array(1, dim = c(d1))
morloc_fill1  <- function(v, d1) array(v, dim = c(d1))
