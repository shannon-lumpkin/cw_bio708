pacman::p_load(tidyverse,
               patchwork,
               here)


# 12.4 load data ----------------------------------------------------------

df_iris <- iris
head(df_iris)


# split by species --------------------------------------------------------

df_setosa <- df_iris %>% filter(Species == "setosa")
df_versicolor <- df_iris %>% filter(Species == "versicolor")
df_virginica <- df_iris %>% filter(Species == "virginica")


# 12.4.1 regression models ----------------------------------------------

df_setosa%>%
  ggplot(aes(x = Petal.Width,
             y= Sepal.Width)) +
  geom_point()

df_versicolor %>%
  ggplot(aes(x = Petal.Width,
             y = Sepal.Width)) +
  geom_point()

df_virginica%>%
  ggplot(aes(x = Petal.Width,
             y = Sepal.Width)) +
  geom_point()


# regression for each species ---------------------------------------------

m_setosa <- lm(Sepal.Width ~ Petal.Width,
               data = df_setosa)

m_versicolor <- lm(Sepal.Width ~ Sepal.Width,
                  data = df_versicolor) 

m_virginica <- lm(Sepal.Width ~ Sepal.Width,
                  data = df_virginica)

# summarize models --------------------------------------------------------

summary(m_setosa)
summary(m_versicolor)
summary(m_virginica) 


# coefs -------------------------------------------------------------------

alpha_setosa <- coef(m_setosa)[1]
beta_setosa <- coef(m_setosa)[2]

alpha_versicolor <- coef(m_versicolor)[1]
beta_versicolor <- coef(m_versicolor)[2]

alpha_virginica <- coef(m_virginica)[1]
beta_virginica <- coef(m_virginica)[2]


# visualize fitted lines --------------------------------------------------

df_setosa %>%
  ggplot(aes(x = Petal.Width,
             y = Sepal.Width)) +
  geom_point() +
  geom_abline(intercept = alpha_setosa,
              slope = beta_setosa)

df_versicolor %>%
  ggplot(aes(x = Petal.Width,
             y = Sepal.Width)) +
  geom_point() +
  geom_abline(intercept = alpha_versicolor,
              slope = beta_versicolor)

df_virginica %>%
  ggplot(aes(x = Petal.Width,
             y = Sepal.Width)) +
  geom_point() +
  geom_abline(intercept = alpha_virginica,
              slope = beta_virginica) 


# 12.4.2 Multiple explanatory variables -----------------------------------

m_setosa_multi <- lm(Sepal.Width ~ Petal.Width + Petal.Length,
                     data = df_setosa) 

m_versicolor_multi <- lm(Sepal.Width ~ Petal.Width + Petal.Length,
                         data = df_versicolor) 

m_virginica_multi <- lm(Sepal.Width ~ Petal.Width + Petal.Length,
                        data = df_virginica)


# summarize multi-variable ------------------------------------------------

summary(m_setosa_multi)
summary(m_versicolor_multi)
summary(m_virginica_multi)


# coef of determination ---------------------------------------------------

eps <- resid(m_setosa_multi)

ss <- sum(eps^2)
ss0 <- sum((df_setosa$Sepal.Width - mean(df_setosa$Sepal.Width))^2)

r_sq <- 1 - ss / ss0

summary(m_setosa_multi) 





