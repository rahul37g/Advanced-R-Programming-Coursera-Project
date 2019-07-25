## load Package
library(purrr)
library(microbenchmark)

## Define factorial loop function
Factorial_loop <- function(x) {
  if (x == 0 || x == 1)
    return(1)
  for (i in (x - 1):1) {
    x <- x * i
  }
  x
}

## Define factorial reduce function
Factorial_reduce <- function(x) {
  if (x == 0)
    return(1)
  reduce(as.numeric(1:x), `*`)
}

## Define factorial func
Factorial_func <- function(x) {
  if (x == 0)
    return(1)
  x * Factorial_func(x - 1)
}


## Define factorial Memoization
Factorial_mem <- function(x) {
  if (x == 0)
    return(1)
  if (!is.na(fact_tbl)[x])
    return(fact_tbl[x])
  fact_tbl[x] <<- x * Factorial_mem(x - 1)
  fact_tbl[x]
}

fact_tbl <- c(rep(NA, 100))

## Test Function

tmp <- 100
factorial(100)

tmp %>% 
  lapply(function(x) {
    cat('Factorial_loop   is', x %>% Factorial_loop, '\n')
    cat('Factorial_reduce is', x %>% Factorial_reduce, '\n')
    cat('Factorial_func   is', x %>% Factorial_func, '\n')
    cat('Factorial_mem    is', x %>% Factorial_mem, '\n')
  })

all.equal( 
  tmp %>% Factorial_loop, 
  tmp %>% Factorial_reduce, 
  tmp %>% Factorial_func, 
  tmp %>% Factorial_mem)

sink("factorial_output.txt")

cat("PART 1: Performance and comparison of specific input values across factorial functions \n\n")

microbenchmark( 
  tmp %>% Factorial_loop, 
  tmp %>% Factorial_reduce, 
  tmp %>% Factorial_func, 
  tmp %>% Factorial_mem)

cat("\n\n PART 2: Performance and comparison of larger input values across factorial functions \n\n")

## fatorial 1000
fact_tbl <- c(rep(NA, 1000))
microbenchmark( 
  1000 %>% Factorial_loop, 
  1000 %>% Factorial_reduce, 
  1000 %>% Factorial_func, 
  1000 %>% Factorial_mem)

sink()
