e <- function(k, x, N) {
  h <- 3/N
  if (x > (k-1)*h && x < k*h) {
    return(x/h - k + 1)
  } else if (x > k*h && x < (k+1)*h) {
    return(k + 1 - x/h)
  } else {
    return(0)
  }
}

de <- function(k, x, N) {
  h <- 3/N
  if (x > (k-1)*h && x < k*h) {
    return(1/h)
  } else if (x > k*h && x < (k+1)*h) {
    return(-1/h)
  } else {
    return(0)
  }
}

ee <- function(i, j, x, N) {
  return(de(i, x, N) * de(j, x, N))
}

B <- function(i, j, N) {
  h <- 3/N
  if (abs(j - i) > 1) {
    return(0)
  }
  else if (j == i) {
    a <- (i-1)*h
    b <- (i+1)*h
  } else {
    a <- min(i, j)*h
    b <- max(i, j)*h
  }
  return(((a-b)/2) * (ee(i, j, (b-a)/(2*sqrt(3)) + (a+b)/2, N)
                    + ee(i, j, (a-b)/(2*sqrt(3)) + (a+b)/2, N)
        ))
}

L <- function(j, N) {
  h <- 3/N
  if (j*h < 1 || j*h > 2) {
    return(0)
  } else {
    a <- (j-1)*h
    b <- (j+1)*h
    integral <- ((a-b)/2) * (e(j, (b-a)/(2*sqrt(3)) + (a+b)/2, N)
                           + e(j, (a-b)/(2*sqrt(3)) + (a+b)/2, N))
    return(4*pi*6.6743e-11 * integral)
  }
}

B_tilde <- function(j, N) {
  h <- 3/N
  a <- (j-1)*h
  b <- (j+1)*h
  integral <- ((a-b)/2) * (e(j, (b-a)/(2*sqrt(3)) + (a+b)/2, N)
                         + e(j, (a-b)/(2*sqrt(3)) + (a+b)/2, N))
  return(integral/3)
}

L_tilde <- function(j, N) {
  return(L(j, N) - B_tilde(j, N))
}

create_M <- function(N) { # Tworzy lewą macierz
  M <- matrix(numeric((N-1)^2), nrow=N-1, ncol=N-1)
  for (j in 1:(N-1)) {
    for (i in 1:(N-1)) {
      M[j, i] <- B(i, j, N)
    }
  }
  return(M)
}

create_Y <- function(N) { # Tworzy prawą macierz
  Y <- matrix(numeric(N-1), nrow=N-1, ncol=1)
  for (j in 1:N-1) {
    Y[j] <- L_tilde(j, N)
  }
  return(Y)
}

Phi <- function(x, W, N) {
  suma <- 0.0
  for (i in 1:(N-1)) {
    suma <- suma + (W[i] * e(i, x, N))
  }
  return(5 - x/3 + suma)
}

main <- function(N) {
  M <- create_M(N)
  Y <- create_Y(N)
  print(M)
  print(Y)
  W <- solve(M, Y)
  print(W)
  vPhi <- Vectorize(Phi, "x")
  curve(vPhi(x, c(W), N), from=0, to=3, n=1000,
        type="l", lwd=3, main="Wykres przybliżonej funkcji Φ", ylab="Φ(x)")
}

main(20)