#install.packages("HistData")
library("HistData")
library("dplyr")
library("ggplot2")

data("GaltonFamilies")
galton_heights <- GaltonFamilies %>% 
  filter(childNum == 1 & gender == 'male') %>%
  select(father,childHeight) %>%
  rename(son = childHeight)

galton_heights %>% 
  summarize(mean(father),sd(father),mean(son),sd(son))

galton_heights %>% 
  ggplot(aes(x=father,y=son)) + geom_point(alpha=0.5)
ggsave('figs/galton_father_son_scatterplot.png')


set.seed(0)
R <- sample_n(galton_heights,25,replace=TRUE) %>%
  summarize(cor(father,son))

B <- 10000
N <- 25
R_mc <- replicate(B,{
  sample_n(galton_heights,N,replace=TRUE) %>%
    summarize(r=cor(father,son)) %>% .$r
})

mean(R_mc)
sd(R_mc)

data.frame(R) %>% ggplot(aes(R)) + 
  geom_histogram(binwidth = .05, color="black")

ggsave('figs/samplecorrelation.png')


conditional_avg <- galton_heights %>% filter(round(father) == 72) %>% 
  summarize(avg=mean(son)) %>% .$avg
x  <- mean(galton_heights$son)
sd <-sd(galton_heights$son)
abs(x-conditional_avg)/sd

galton_heights %>% mutate(father_strata= factor(round(father)))  %>%
  ggplot(aes(father_strata,son)) + geom_boxplot() + geom_point()
ggsave('figs/father_strata.png')

galton_heights %>% mutate(father = round(father))  %>%
  group_by(father) %>%
  summarize(son_conditional_avg = mean(son)) %>%
  ggplot(aes(father,son_conditional_avg)) + geom_point()
ggsave('figs/father_linear.png')

r <- galton_heights %>% summarize(r = cor(father,son)) %>% .$r
galton_heights %>% mutate(father = round(father))  %>%
  group_by(father) %>%
  summarize(son = mean(son)) %>%
  mutate(z_father=scale(father),z_son=scale(son)) %>%
  ggplot(aes(z_father,z_son)) + geom_point() +
  geom_abline(intercept=0,slope=r)
ggsave('figs/scaled_father_son_corr.png')


mu_x = mean(galton_heights$father)
mu_y = mean(galton_heights$son) 
sd_x = sd(galton_heights$father)
sd_y = sd(galton_heights$son)
rho <- galton_heights %>% summarize(r = cor(father,son)) %>% .$r
m = rho*(sd_y/sd_x)
b = mu_y - m*mu_x
galton_heights %>%
  ggplot(aes(father,son)) + geom_point() +
  geom_abline(intercept=b,slope=m)
ggsave('figs/father_son_regression.png')

galton_heights %>%
  ggplot(aes(scale(father),scale(son))) + geom_point() +
  geom_abline(intercept=0,slope=m)
ggsave('figs/scaled_father_son_regression.png')

dat <- galton_heights %>%
  mutate(father_strata = round(father)) %>%
  filter(father_strata >= 66 & father_strata <= 72)

#   facet_wrap(~BB_strata)
dat %>%
  ggplot() + 
  stat_qq(aes(sample=son)) + 
  facet_wrap(~father_strata)
ggsave('figs/father_strata.png')
