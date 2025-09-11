library(tidyverse)
# Q1

z<- exp(rnorm(n = 1000, mean = 0, sd = 0.1))
mu_z<- sum(z) /length(z)
# arithmetic mean 
mean(z)

# geometric
# taking the ean in log scale
mu_z <- runif(1000, min=10, max=15)
prod(mu_z)^ (1 / length(mu_z))
exp(mean(log(mu_z)))

# use product
prod(z)^(1 /length(z))

# median
mu_z <- runif(1000, min=10, max=15)
mu_z <- sort(mu_z)
index <- (length(mu_z) + 1) / 2
mu_z[index]
median(mu_z)
# median
median(z)

# Q2

df_z <- tibble(z = z)
df_z%>%
ggplot(aes(x=z)) +
  geom_histogram()


