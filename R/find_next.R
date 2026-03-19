
find_next <- function(x, y) {
  # Find the index of the next value in y following x
  approx(y, 
         seq_along(y),
         method = "constant",
         f = 1, 
         rule = 2,
         xout = x)$y
}