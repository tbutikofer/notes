# Composite states
n = 5000
y1 = rnorm(n, mean = 0, sd = 0.5)
y2 = rnorm(n, mean = 1.5, sd = 0.2)
hist(c(y1,0.8*y2), breaks = 40, xlim = c(-2,2), freq = FALSE, main = "N=10000", xlab = "Observation", ylab = "Probability")


# Custom probability distriubtion
#Probability distribution
probability <- function(x, D, k, d) {
  phi1 <- complex(real = 0, imaginary = k*sqrt(D^2+(x-d/2)^2))
  Psi1 <- exp(phi1)/sqrt(sqrt(D^2+(x-d/2)^2))
  
  phi2 <- complex(real = 0, imaginary = k*sqrt(D^2+(x+d/2)^2))
  Psi2 <- exp(phi2)/sqrt(sqrt(D^2+(x+d/2)^2))
  
  abs(Psi1 + Psi2)^2
}

curve(probability(x, D=2, k=1000, d=0.02), from=-5, to=5, n=1000)

measurement <- function(N, max, D, k, d)
{
  samples <- numeric(N)
  for (i in 1:N) 
  {
    repeat
    {
      # 1. sample from the propsal distribution:
      rx <- runif(1, -max, max)
      # 2. sample a point along a vertical line 
      #    at the rx point from Uniform(0, f(x)*m):
      rv <- runif(1, 0, 2)
      # 3. evaluate the density of p(x) at rx
      dx <- probability(rx,D=D, k=k, d=d)
      # 4. compare and accept if appropriate
      if (rv <= dx) 
      {
        samples[i] <- rx
        break        
      }
    }
  }
  return(samples)
}

par(mfrow=c(3,1))
hist(measurement(N=100, max=5, D=2, k=1000, d=0.02), breaks = 100, xlim = c(-5,5), freq = FALSE, main = "N=100", xlab = "Observation", ylab = "Probability")
hist(measurement(N=1000, max=5, D=2, k=1000, d=0.02), breaks = 100, xlim = c(-5,5), freq = FALSE, main = "N=1000", xlab = "Observation", ylab = "Probability")
hist(measurement(N=100000, max=6, D=2, k=1000, d=0.02), breaks = 100, xlim = c(-6,6), freq = FALSE, main = "N=1000000", xlab = "Observation", ylab = "Probability")

hist(c(2,2), breaks=1000, xlim=c(-6,6))

