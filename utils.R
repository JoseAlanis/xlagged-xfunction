# define helper functions

# scaling function
z_score <- function(x) {
  z <- (x - mean(x, na.rm = T))/sd(x, na.rm = T)
  z
}

