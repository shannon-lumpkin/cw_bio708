# generalized linear model

pacman::p_load(tidyverse, 
               patchwork,
               here)


# count data --------------------------------------------------------------


(df_count <- read_csv(here("data_raw/data_garden_count.csv")))
print(df_count)
m_normal <- lm(count ~ nitrate,
   df_count)

summary(m_normal)

alpha <- coef(m_normal)[1]
beta <- coef(m_normal)[2]

df_count %>% 
  ggplot(aes(x = nitrate,
             y = count)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta)


## random generator from Poisson distriution

(y <- rpois(n = 10, lambda = 2))

## apply Poisson distribution using glm

m_pois <- glm(count ~ nitrate,
    data = df_count,
    family = "poisson")

summary(m_pois)

ggplot(df_count) +
  geom_point(aes(x = nitrate,
                 y = count)) +
  geom_abline(intercecpt = coef(m_pois)[1],
              slope = coef(m_pois)[2])

## visualization of poisson regression

# make predictions
df_pred <- tibble(nitrate = seq(min(df_count$nitrate),
                                max(df_count$nitrate),
                                length = 100))

# y_pois is exponentiated because predict() returns values in log-scale
y_normal <- predict(m_normal, newdata = df_pred)
y_pois <- predict(m_pois, newdata = df_pred) %>% exp()

df_pred <- df_pred %>% 
  mutate(y_normal,
         y_pois)


ggplot(df_count) +
  geom_point(aes(x= nitrate,
                 y= count)) +
  
df_count %>% 
  ggplot(aes(x = nitrate,
             y = count)) +
geom_abline(intercept = coef(m_pois)[1],
            slope = coef(m_pois)[2])

# make predictions
df_pred <- tibble(nitrate = seq(min(df_count$nitrate),
                                max(df_count$nitrate),
                                length = 100))


# make predictions
df_pred <- tibble(nitrate = seq(min(df_count$nitrate),
                                max(df_count$nitrate),
                                length = 100))

# y_pois is exponentiated because predict() returns values in log-scale
y_normal <- predict(m_normal, newdata = df_pred)
y_pois <- predict(m_pois, newdata = df_pred) %>% exp()

df_pred <- df_pred %>% 
  mutate(y_normal,
         y_pois)

# figure
df_count %>% 
  ggplot(aes(x = nitrate,
             y = count)) +
  geom_point() +
  geom_line(data = df_pred,
            aes(y = y_normal),
            linetype = "dashed") +
  geom_line(data = df_pred,
            aes(y = y_pois))

## compare summary output from m_normal and m_pois

summary(m_normal)
summary(m_pois)


# proportional data -------------------------------------------------------

df_mussel <- read_csv(here("data_raw/data_mussel.csv"))
print(df_mussel)

## plot data

df_mussel <- df_mussel %>%
  mutate(prop_fert = n_fertilized / n_examined)
ggplot(df_mussel,
       aes(x = density,
           y = prop_fert)) +
  geom_point() 
  
## cbind is needed for binaomial
cbind(df_mussel$n_fertilized,df_mussel$n_examined - df_mussel$n_fertilized)

## binomial model
m_binom <- glm(cbind(n_fertilized, n_examined - n_fertilized) ~ density,
    data = df_mussel,
    family = "binomial")
              
summary(m_binom)

## how logit fxn works

df_test <- tibble(logit_x = seq(-10, 10, length = 100),
                  x = exp(logit_x) / (1 + exp(logit_x)))

df_test %>% 
  ggplot(aes(x = logit_x,
             y = x)) +
  geom_point() +
  geom_line() 










