primo <- function(n) {
  if (n < 4) {
    return(TRUE)
  }
  if (n %% 2 == 0) { # par
    return(FALSE)
  }
  for (i in seq(3, max(3, n - 1), 2)) {
    if (n %% i == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}

primos <- numeric()
for (n in 1:30000) {
  if (primo(n)) {
    primos <-  c(primos, n)
  }
}
primos [1:200]
tail(primos,200)