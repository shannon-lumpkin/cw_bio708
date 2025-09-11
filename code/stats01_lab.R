library(tidyverse)

# 7.3.1 Comparing Central Tendency Measures
# Q1 means/median
z<-exp(rnorm(n=1000, mean=0, sd=0.1))

# arithmetic mean 
mu_z<- mean(z) 

# geometric
# taking the mean in log scale
mu_g<- exp(mean(log(mu_z)))

# median
mu_m<- median(z)

# Q2 Drawing a histogram
z_df<- tibble(z=z)
z_df%>%
ggplot(aes(x=z)) +
  geom_histogram()

# Q3 Adding the vertical lines of means/median
z_df%>%
  ggplot(aes(x=z)) +
  geom_histogram()+ 
  geom_vline(xintercept= mu_z) +
  geom_vline(xintercept= mu_g) +
  geom_vline(xintercept= mu_m)

g1<- z_df%>%
  ggplot(aes(x=z)) +
  geom_histogram()+
  geom_vline(xintercept= mu_z,
             color= "salmon") +
  geom_vline(xintercept= mu_g,
             color= "blue") +
  geom_vline(xintercept= mu_m,
             color= "green")

# Q4
## visualization

# Q5 new vector
z_rev<- -z + max(z) + 0.1

# Q1 means/median
#arithmetic mean
mu_z_rev<-mean(z_rev)

#geometric mean
mu_g_rev<-exp(mean(log(z_rev)))

#median
mu_m_rev<-median(z_rev)

# Q2 drawing a histogram
z_rev_df<- tibble(z_rev=z_rev)
z_rev_df%>%
  ggplot(aes(x=z_rev)) +
  geom_histogram()

# Q3 Adding the vertical lines of means/median
# renaming means/median first
z_rev_df%>%
  ggplot(aes(x=z_rev)) +
  geom_histogram()+ 
  geom_vline(xintercept= mu_z_rev) +
  geom_vline(xintercept= mu_g_rev) +
  geom_vline(xintercept= mu_m_rev)

# Q4 Adding color to the lines and naming g2
g2<- z_rev_df%>%
  ggplot(aes(x=z_rev)) +
  geom_histogram()+
  geom_vline(xintercept= mu_z_rev,
             color= "salmon") +
  geom_vline(xintercept= mu_g_rev,
             color= "blue") +
  geom_vline(xintercept= mu_m,
             color= "green")



# 7.3.2 comparing variation measures
w <- rnorm(100, mean = 10, sd = 1) #unit g
head(w) # show first 10 elements in w

#Q1
## gram to milligram
m<- 1000*w #unit: milligram

#Q2
s2_w<- sum((w-mean(w))^2) / length (w)
s_w <- sqrt(s2_w)

s2_m<- sum((m-mean(m))^2) / length (m)
s_m <- sqrt(s2_m)

##MAD
mad_w<- median(abs(w-median(w)))
mad_m<- median(abs(m-median(m)))

#Q3
##CV
cv_w<-s_w / mean(w) #num has a unit of [g], denom [g]
cv_m<-s_m / mean(m) # num has a unit of [mg], denom [mg]

## MAD / median

madr_w<- mad_w / median(w)
madr_m<- mad_m / median(m)

               


