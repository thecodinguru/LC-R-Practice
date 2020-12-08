###Poisson generator based upon the inversion by sequential search
Poisson_generator <- function(n, lambda){
  out <- rep(0, n)
  for (i in 1: n){
  x <- 0
  pdf <- exp(-lambda)
  cdf <- exp(-lambda)
  u <- runif(1)
  while (u > cdf) {
    x <- x + 1
    pdf <- pdf * lambda / x
    cdf <- cdf + pdf}
    out[i] <- x
  }
  out
}

###Geometric generator based on uniform random number.
Geometric_generator <- function(n, p){
  out <- rep(0, n)
  for (i in 1: n){
    k <- 1
    v <- runif(1)
    while (v < (1- p)^ k){
      k <- k + 1
    }
    out[i] <- k
  }
  out
}

###Binomial generator based on several uniform random numbers.
Binomial_generator <- function(m, n, p){
  out <- rep(0, m)
  for (i in 1: m){
    out[i] <- sum(runif(n) < p)
  }
  out
}

###NBA playoffs.
NBA_generator <- function(m, n, p){
  out <- rep(0, m)
  for (i in 1: m){
    game <- 0
    win <- 0
    lose <- 0
    while (win < ceiling(n / 2) && lose < ceiling(n / 2)) {
      game <- game + 1
      if (runif(1) < p){
        win <- win + 1
      } else{
        lose <- lose + 1
      }
    }
    out[i] <- game
  }
  out
}

sum(NBA_generator(10000, 7, 0.5) == 7)/10000

###Binomial generator based on one uniform random number.
Binomial_generator1 <- function(m, n, p){
  out <- rep(0, m)
  for (i in 1: m){
    u <- runif(1)
    k <- 0
    ind <- 0
    while (k < n){
      if (u < p){
        ind <- ind + 1
        u <- u / p
      }
      else {
        u <- (u - p) / (1 - p)
      }
      k <- k + 1
    }
    out[i] <- ind
  }
  out
}
mean(Binomial_generator1(10000, 10, 0.5))

###	Expectation of the length of a sequence.
random_drawing <- function(m, N) {
  out <- rep(0, m)
  for (i in 1: m){
    x <- 1: N
    x0 <- sample(x, 1)
    x <- x[x != x0]
    x1 <- sample(x, 1)
    len <- 2
    while (x1 > x0){
      x0 <- x1
      x <- x[x != x0]
      x1 <- sample(x, 1)
      len <- len + 1
    }
    out[i] <- len
  }
  out}
mean(random_drawing(10000, 1000))

###	generate unif(0,1) until a decrease, what is the expectation of the length.
random_uniform <- function(m) {
  out <- rep(0, m)
  for (i in 1: m){
    x0 <- runif(1)
    x1 <- runif(1)
    len <- 2
    while (x1 > x0){
      x0 <- x1
      x1 <- runif(1)
      len <- len + 1
    }
    out[i] <- len
  }
  out}
mean(random_uniform(10000))

### Exponential generator based on uniform random number.
### http://www.columbia.edu/~ks20/4703-Sigman/4703-07-Notes-ARM.pdf
Exponential_generator <- function(m, lambda){
  out <- rep(0, m)
  for (i in 1: m){
    out[i] <- - log(1 - runif(1))/ lambda
  }
  out
}
mean(Exponential_generator(10000, 0.5))

### Normal generator based on uniform random numbers.
Normal_generator <- function(m, mu){
  out <- rep(0, m)
  for (i in 1: m){
    u <- runif(1)
    v <- runif(1)
    y <- -log(1 - v)
    while (u > exp(-1/2 * (y - mu-1)^2)){
      u <- runif(1)
      v <- runif(1)
      y <- -log(1 - v)
    }
    out[i] <- y * (-1) ^ (v > 0.5)
  }
  out
}
mean(Normal_generator(10000, 0))

### Normal generator based on uniform random numbers.
### https://www.math.arizona.edu/~tgk/mc/book_chap4.pdf
Normal_generator <- function(m, mu, sigma){
  out <- rep(0, m)
  for (i in 1: m){
    u <- runif(1)
    v <- runif(1)
    y <- tan((v - 0.5)*pi)
    while (u > sqrt(pi/2)/1.52 *exp(- y^2/2)*(1 + y^2)){
      u <- runif(1)
      v <- runif(1)
      y <- tan((v - 0.5)*pi)
    }
    out[i] <- y * sigma + mu
  }
  out
}
mean(Normal_generator(10000, 0, 1))
