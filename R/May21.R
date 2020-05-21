# When you scale a random walk and the scaling is with a large enough tao, your random walk becomes a brownian motion diffusion process.

rnd.walk <- function(T, p=0.5, x0=0){
  out <- rep(0, T)

  out[1] <- x0

  for(i in 2:T){

    out[i] <- out[(i-1)] + sample(x = c(1,-1), size = 1, replace = TRUE, prob=c(0.5, 0.5))

  }
  return(out)
}

# Generating a sample path
sample.path <- rnd.walk(T=25000, p=0.5, x0=0)

# Scaling according to time and sqrt of time

path25th <- sample.path[1:25000]/sqrt(25000)
path10th <- sample.path[1:10000]/sqrt(10000)
path5th <- sample.path[1:5000]/sqrt(5000)
path1th <- sample.path[1:1000]/sqrt(1000)
path200 <- sample.path[1:200]/sqrt(200)
path50 <- sample.path[1:50]/sqrt(50)
path50Normal <- sample.path[1:50]


timesteps <- seq(1,25000)/ 25000


par(mfrow = c(2,4), font = 1, mai = c(0.45, 0.45, 0.28, 0.70), oma = c(1.45, 1.25, 0.5, 0.25))

plot(1:50, path50Normal, type = "s")
plot(seq(1,50)/50, path50, type = "s")
plot(seq(1,200)/200, path200, type = "s")
plot(seq(1,1000)/1000, path1th, type = "s")
plot(seq(1,5000)/5000, path5th, type = "s")
plot(seq(1,10000)/10000, path10th, type = "s")
plot(seq(1,25000)/25000, path25th, type = "s")

# This is how you get a diffusion process












