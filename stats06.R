pacman::p_load(tidyverse, 
               patchwork,
               here)

# read data ---------------------------------------------------------------

df_algae <- read_csv("data_raw/data_algae.csv")


# 12.1 --------------------------------------------------------------------
# visualize ---------------------------------------------------------------
df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point()


# try regression analysis -------------------------------------------------

m <- lm(biomass ~ conductivity,
        data = df_algae)

summary(m)

alpha <- coef(m)[1]
beta <- coef(m)[2]

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta) 


# 12.2.4 Standard Errors and t-values -------------------------------------
# get t-values ------------------------------------------------------------

se <-sqrt(diag(vcov(m))) #variance/covariance matrix
t_value <-beta / se[2] #t-value

# p-value for slope -------------------------------------------------------

(1 - pt(t_value, df = 48)) + pt(-t_value, df = 48)


# 12.3 --------------------------------------------------------------------
# coefficient of determination --------------------------------------------
eps <- resid(m)

# 12.3.2 visualize errors -------------------------------------------------

df_algae <- df_algae %>% 
  mutate(eps = eps)

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta) + 
  geom_segment(aes(x = conductivity, 
                   xend = conductivity, 
                   y = biomass, 
                   yend = biomass - eps), 
               linetype = "dashed")

ss <-sum(eps^2)
ss0 <- sum((df_algae$biomass - mean(df_algae$biomass))^2)


# ss / ss0 = 1 when the model is really poor ------------------------------
# ss / ss0 = 0 when the model is perfect ----------------------------------

r_sq <- 1 - ss / ss0

# compare with lm output --------------------------------------------------
summary(m)



