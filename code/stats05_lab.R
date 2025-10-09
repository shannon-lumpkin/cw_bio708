pacman::p_load(tidyverse,
               patchwork,
               here)

# 11.5.1 #1 ---------------------------------------------------------------

df_pg <- PlantGrowth 

colnames(df_pg)
distinct(df_pg, group) 

df_pg %>%
  ggplot(aes(x = group,
             y = weight)) +
  geom_violin(draw_quantiles = 0.5,
              alpha = 0.2,
              fill = "steelblue") +
  geom_jitter(alpha = 0.1) + 
  theme_bw()


# 11.5.1.#2 ---------------------------------------------------------------

m <- aov(weight ~ group,
         data = df_pg)

summary(m)


# between group variability -----------------------------------------------

mu <- mean(df_pg$weight)

s_b <- df_pg%>%
  group_by(group) %>%
  summarize(mu_g = mean(weight),
            n=n()) %>%
  mutate(dev_g = (mu_g - mu)^2,
         ss_g = dev_g * n) %>%
  pull(ss_g) %>%
  sum()


# within-group variability ------------------------------------------------

s_w <- df_pg %>%
  group_by(group) %>%
  mutate(mu_g = mean(weight)) %>%
  ungroup() %>%
  mutate(dev_i = (weight - mu_g)^2) %>%
  pull(dev_i) %>%
  sum() 


# sum squared to variance -------------------------------------------------

(s2_b <- s_b / (n_distinct(df_pg$group) -1))
(s2_w <- s_w / (nrow(df_pg) - (n_distinct(df_pg$group))))



# install pwr_package -----------------------------------------------------


## n group = 3
## f = 0.5
## power = 0.8

pwr::pwr.anova.test(k = 3,
                    f = 0.5, sig.level = 0.05,
                    power = 0.8)

##change k , f, power, one at a time
##see how these changes affect the number of samples you may need

pwr::pwr.anova.test(k = 50,
                    f = 0.5, sig.level = 0.05,
                    power = 0.8)

pwr::pwr.anova.test(k = 3,
                    f = 1.5, sig.level = 0.05,
                    power = 0.8)

pwr::pwr.anova.test(k = 3,
                    f = 0.5, sig.level = 0.05,
                    power = 0.2)



pwr::pwr.anova.test(k=3,
                    n =5,
                    f = 0.5,
                    sig.level = 0.05)


## leave power blank 

pwr::pwr.anova.test(k = 3,
                    n = 5,
                    f = 0.5,
                    sig.level = )


## different levels of k, n, f 

pwr::pwr.anova.test(k = 50,
                    n = 5,
                    f = 0.5,
                    sig.level = 0.05)

pwr::pwr.anova.test(k = 3,
                    n = 100,
                    f = 0.5,
                    sig.level = 0.05)

pwr::pwr.anova.test(k = 3,
                    n = 5,
                    f = 0.25,
                    sig.level = 0.05)












