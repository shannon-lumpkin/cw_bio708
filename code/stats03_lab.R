# probability lab

library(tidyverse)
library(patchwork)

## normal distribution

v <- rnorm(n=50, mean= 100, sd= 5)

floor(min(v))
min(v)
floor(min(v))
ceiling(max(v))
max(v)

bin<-seq(floor(min(v)),
         ceiling(max(v)),
         by=1)
bin

pnorm(bin[2],mean = mean(v), sd=sd(v))

p <- NULL # empty object for probability
for (i in 1:(length(bin) - 1)) {
  p[i] <- pnorm(bin[i+1], mean = mean(v), sd = sd(v)) - 
    pnorm(bin[i], mean = mean(v), sd = sd(v))
}

df_prob <- tibble(bin=bin[-length(bin)] +0.5,
       prob=p) %>%
  mutate(freq = length(v) * prob)

df_v <- tibble (v=v)
df_v %>%
  ggplot(aes(x=v))+
  geom_histogram()+
  geom_point(data = df_prob,
             aes(x=bin,
                 y=freq),
             color="darkgreen")+

  geom_line(data = df_prob,
            aes(y = freq,
                x = bin),
            color = "darkgreen")


# poisson distribution
## get data for 1000 samples

x <- rpois(n=1000, lambda=10)
bin <- seq(0,
           max(x) + 5,
           by=1)

## calculate the probability of abs
p<-dpois(bin, lambda = mean(x))

## dataframe
df_prob <-tibble(bin=bin,
                 prob=p)%>%
  mutate(freq = length(x) *p)

df_x <- tibble(x=x)

## figure

df_x %>%
  ggplot(aes(x = x)) +
  geom_histogram(binwidth = 0.5) +
  geom_line(data = df_prob,
            aes(x = bin,
                y = freq),
            color = "pink") +
  geom_point(data = df_prob,
             aes(x = bin,
                 y = freq),
             color= "pink")



                 


