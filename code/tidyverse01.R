


# Row Manipulation --------------------------------------------------------

# Subset Rows -------------------------------------------------------------
data("iris")
library(tidyverse)

set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()

print(iris_sub)

filter(iris_sub, Species == "virginica")
filter(iris_sub, Species %in% c("virginica", "versicolor"))
filter (iris_sub, Species != "virginica")
filter(iris_sub, !(Species %in% c("virginica", "versicolor")))
filter(iris_sub, Sepal.Length >5)
filter(iris_sub, Sepal.Length >= 5)
filter(iris_sub, Sepal.Length < 5)
filter(iris_sub, Sepal.Length <= 5)
filter(iris_sub, Sepal.Length < 5 & Species == "setosa")
filter(iris_sub, Sepal.Length < 5, Species == "setosa")
filter(iris_sub, Sepal.Length < 5 | Species == "setosa")

# Arrange Rows ------------------------------------------------------------

arrange(iris_sub, Sepal.Length)
arrange(iris_sub, desc(Sepal.Length))

# Column Manipulation -----------------------------------------------------

# Select Columns ----------------------------------------------------------

select(iris_sub, Sepal.Length)
select(iris_sub, c(Sepal.Length, Sepal.Width))
select(iris_sub, -Sepal.Length)
select(iris_sub, -c(Sepal.Length, Sepal.Width))
select(iris_sub, starts_with("Sepal"))
select(iris_sub, -starts_with("Sepal"))
select(iris_sub, ends_with("Width"))
select(iris_sub, -ends_with("Width"))

# Add Columns -------------------------------------------------------------

(x_max <- nrow(iris_sub))
x <- 1:x_max
mutate(iris_sub, row_id = x)
mutate (iris_sub, sl_two_times = 2 * Sepal.Length)

# piping ------------------------------------------------------------------

df_vir <- filter(iris_sub, Species == "virginica")
df_vir_sl <- select(df_vir, Sepal.Length)
print(df_vir_sl)
df_vir_sl <- iris_sub %>%
  filter(Species == "virginica") %>%
  select(Sepal.Length)
print(df_vir_sl)

df_sl <- select(iris_sub, Sepal.Length)
df_sl_2times <- mutate(df_sl, twice = 2 * Sepal.Length)

df_tw <- iris_sub %>%
  select(Sepal.Length) %>%
  mutate(twice = 2* Sepal.Length)


  
  


