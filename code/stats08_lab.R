pacman::p_load(tidyverse, 
               patchwork,
               here)

## GLM exercise
# 14.4.1. (1) -------------------------------------------------------------

url <- "https://raw.githubusercontent.com/aterui/public-proj_fish-richness-shubuto/master/data/data_vpart.csv"
df_fish <- read_csv(url)

m_fish <- glm(n_sp ~ distance + cat_area + hull_area,
              data = df_fish,
              family = "poisson")

summary(m_fish)


# 14.4.1. (2) -------------------------------------------------------------

data("mtcars")

m_trans_binom <- glm(am ~ mpg + hp + wt,
                     data = mtcars,
                     family = "binomial")
summary(m_trans_binom)

## family = gaussian

m_trans_norm <- glm(am ~ mpg + hp + wt,
                    data = mtcars,
                    family = "gaussian")
summary(m_trans_norm)

## Effect size
# 14.4.2  -----------------------------------------------------------------

df_fish <- df_fish %>%
  mutate(std_dist = scale(distance),
         std_cat  = scale(cat_area),
         std_hull = scale(hull_area))

m_fish_std <- glm(n_sp ~ std_dist + std_cat + std_hull,
                  data = df_fish,
                  family = "poisson")

summary(m_fish_std)

## Offset term
# 14.4.3 (1) --------------------------------------------------------------

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_offset.csv"
df_offset <- read_csv(url)


# 14.4.3 (2) --------------------------------------------------------------

df_offset %>%
  ggplot(aes(x = nitrate, y = count)) +
  geom_point() +
  labs(x = "Nitrate", y = "Count")

df_offset %>%
  ggplot(aes(x = area, y = count)) +
  geom_point() +
  labs(x = "Area", y = "Count")

df_offset %>%
  mutate(density = count / area) %>%
  ggplot(aes(x = nitrate, y = density)) +
  geom_point() +
  labs(x = "Nitrate", y = "Count per Area")

m_no_offset <- glm(count ~ nitrate,
                   data = df_offset,
                   family = "poisson")

m_with_offset <- glm(count ~ nitrate + offset(log(area)),
                     data = df_offset,
                     family = "poisson")

summary(m_no_offset)
summary(m_with_offset)


## Overdispersion
# 14.4.4 ------------------------------------------------------------------

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_tadpole.csv"
df_tadpole <- read_csv(url)

df_tadpole %>%
  ggplot(aes(x = aqveg, y = tadpole)) +
  geom_point() +
  labs(x = "Aquatic vegetation cover", y = "Tadpole count")

df_tadpole %>%
  ggplot(aes(x = permanence, y = tadpole)) +
  geom_point() +
  labs(x = "Pond permanence (days)", y = "Tadpole count")

m_tadpole <- glm(tadpole ~ aqveg + permanence,
                 data = df_tadpole,
                 family = "poisson")

summary(m_tadpole) 





















