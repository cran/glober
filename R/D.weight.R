### Weighted difference matrix D ####
D.weight <- function(x, ord){
  D = diag(length(x))
  for (i in 1:ord){
    xobs= x[i:length(x)]
    N = length(xobs)
    D0= bandSparse(N-1, N, 0:1, list(rep(-1, N-1), rep(1, N-1)))
    W = Diagonal(N-1,1/diff(xobs))
    D = W %*% D0 %*% D
  }
  return(D)
}
