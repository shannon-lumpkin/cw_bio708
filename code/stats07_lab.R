pacman::p_load(tidyverse,
               patchwork,
               here)


# 13.4.1 ------------------------------------------------------------------
data ("ToothGrowth")

df_tooth <- ToothGrowth

m_tooth <- lm(len~ supp * dose,
              data = df_tooth)


eps <- resid(m_tooth)
shapiro.test(eps)

summary(m_tooth)


# 13.4.2 ------------------------------------------------------------------

df_pred <- ToothGrowth %>%
  group_by(supp)%>%
  reframe(dose = seq(min(dose),
                     max(dose),
                     length = 100))

y_pred <- predict(m_tooth,
                  newdata = df_pred)

df_pred <- df_pred %>%
  mutate(y = y_pred)

ToothGrowth %>%
  ggplot(aes(x = dose,
             y = len,
             color = supp)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_pred,
            aes(y = y))


# 13.4.3 ------------------------------------------------------------------
# multicollinearity -------------------------------------------------------

## variance-covariance matrix
mv <- rbind(c(1, 0.9),
            c(0.9, 1))

## true regression coefficients
b <- c(0.05, 1.00)

## produce simulated data
set.seed(523)
X <- MASS::mvrnorm(100,
                   mu = c(0, 0),
                   Sigma = mv)

df_y <- tibble(x1 = X[,1],
               x2 = X[,2]) %>% 
  mutate(y = rnorm(nrow(.),
                   mean = 1 + b[1] * x1 + b[2] * x2))


ggplot1 <- ggplot(df_y, aes(x = x1, y = y)) +
  geom_point() +
  labs(x = "x1",
       y = "y",
       title = "Scatterplot of x1 vs y") +
  theme_minimal()

ggplot2 <- ggplot(df_y, aes(x = x2, y = y)) +
  geom_point() +
  labs(x = "x2",
       y = "y",
       title = "Scatterplot of x2 vs y") +
  theme_minimal()

ggplot1 / ggplot2

m <- lm(y ~ x1 + x2, data = df_y)
summary(m)


# look at relationship between x1 and x2 ----------------------------------

df_y %>%
  ggplot(aes(x = x1,
             y = x2)) +
  geom_point()

with(df_y,
     cor(x1, x2))





