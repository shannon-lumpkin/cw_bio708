pacman::p_load(tidyverse, 
               patchwork,
               here)

set.seed(1) # for reproducibility

# hypothetical sample size
n <- 100

# true intercept and slope
b <- c(0.1, 0.5)

# hypothetical explanatory variable
x1 <- rnorm(n = n, mean = 0, sd = 1)

# create a design matrix
X <- model.matrix(~x1)

# expected values of y is a function of x
# %*% means matrix multiplication
# y = X %*% b equals y = b[1] + b[2] * x
# recall linear algebra
y_hat <- X %*% b

# add normal errors
y <- rnorm(n = n, mean = y_hat, sd = 0.5)

# plot
df0 <- tibble(y = y, x1 = x1)

df0 %>% 
  ggplot(aes(y = y,
             x = x1)) + 
  geom_point()

lm(y ~x1,
   data = df0)


# likelihood  -------------------------------------------------------------

#probability of observing 3 with lambda = 3, under poisson
dpois(2, lambda = 3.5)

dpois(3, lambda = 3.5)

dpois(1, lambda = 3.5)

dpois(4, lambda = 3.5)

dpois(10, lambda = 3.5)

# try different lambda values
lambda <- seq(0, 10, by = 0.1)

pr <-dpois (3, lambda = lambda)

df_pois <- tibble(y = 3,
                  lambda = lambda,
                  pr = pr)

df_pois %>%
  ggplot(aes(x =lambda,
             y= pr)) +
  geom_point() +
  geom_line() +
  labs(x= "lambda",
       y = "Pr(k =3)")

df_pois %>%
  arrange(desc(pr)) 

# probability of observing values of 3, 2, 5 at the same tiem

pr<- dpois(c(3, 2, 5), lambda = 3)
prod(pr)

# lambda for 3, 2, 5
y <- c(3, 2, 5)
lambda <- seq(1, 10, by = 0.01) 

pr <- sapply(X =lambda, 
             FUN = function(z) prod(dpois(y, lambda = z))) 

df_pois <- tibble(lambda = lambda,
                  pr = pr) 

df_pois %>%
  ggplot(aes(x =lambda,
             y= pr)) +
  geom_point() +
  geom_line() +
  labs(x= "lambda",
       y = "Pr") 

df_pois %>%
  arrange(desc(pr)) %>%
  print()

mean(c(3, 2, 5))

# load garden plant data
df_count <- read_csv(here("data_raw/data_garden_count.csv"))

m_pois <- glm(count ~ nitrate,
              data = df_count,
              family = "poisson")

logLik(m_pois)





