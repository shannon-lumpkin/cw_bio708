library(tidyverse)
library(patchwork)
df_h0 <- read_csv(here::here("data_raw/data_plant_height.csv"))
mu <- mean(df_h0$height)
sigma2 <- sum((df_h0$height - mu)^2) / nrow(df_h0)

mu50_i <- var50_i <- NULL

for(i in 1:100) {
  df50_i <- df_h0 %>% 
    sample_n(size = 50) 
  (mu50_i[i]<- mean(df50_i$height))
  (var50_i[i] <- var(df50_i$height))
}

mu100_i<- var100_i <- NULL

for(i in 1:100) {
  df100_i <- df_h0 %>% 
    sample_n(size = 100) 
  (mu100_i[i]<- mean(df100_i$height))
  (var100_i[i] <- var(df100_i$height))
}

## create tibbles
df_est <- tibble(mu50 = mu50_i,
                 var50 = var50_i,
                 mu100 = mu100_i,
                 var100 = var100_i)

## histogram for 50
g50_mu <-df_est %>%
  ggplot(aes(x=mu50)) +
  geom_histogram()+
  geom_vline(xintercept = mu)


# histogram for 100
g100_mu <-df_est %>%
  ggplot(aes(x=mu100)) +
  geom_histogram()+
  geom_vline(xintercept = mu) 

#to compare
g50_var <- df_est %>%
  ggplot(aes(x = var50)) +
  geom_histogram(bins = 20) +
  geom_vline (xintercept = sigma2)

g100_var <- df_est %>%
  ggplot(aes(x = var100)) +
  geom_histogram(bins = 20) +
  geom_vline (xintercept = sigma2)
  
g50_var /g100_var


# Q2
# given code
df_h10 <- df_h0 %>% 
  filter(height >= 10)



