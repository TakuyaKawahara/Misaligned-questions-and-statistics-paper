
library(dplyr)
library(ggplot2)
library(latex2exp)
library(patchwork)


a <- data.frame();
for (m in -2:2) {
  for (n in -2:2){
    for (o in -2:2) {
      for (p in -2:2){
        for (q in -2:2) {
          for (r in -2:2){
            a2 <- data.frame(
              theta_1=m*1.5,
              theta_2=n*1.5,
              theta_3=o*1.5,
              beta_1=p*1.5,
              beta_2=q*1.5,
              beta_3=r*1.5
            )
            a <- bind_rows(a, a2)
          }}}}}}

### EXPLORE
min <- -0.3
max <- 0.6
theta_0 <- -3
beta_0 <- -3
pL <- 0.5
a2 <- a %>%
  mutate(
    mu11 = exp(theta_0 + theta_1 + theta_2 + theta_3)/(1+exp(theta_0 + theta_1 + theta_2 + theta_3)),
    mu10 = exp(theta_0 + theta_1)/(1+exp(theta_0 + theta_1)),
    mu01 = exp(theta_0 + theta_2)/(1+exp(theta_0 + theta_2)),
    mu00 = exp(theta_0 )/(1+exp(theta_0 )),
    pi11 = exp(beta_0 + beta_1 + beta_2 + beta_3)/(1+exp(beta_0 + beta_1 + beta_2 + beta_3)),
    pi10 = exp(beta_0 + beta_1 )/(1+exp(beta_0 + beta_1 )),
    pi01 = exp(beta_0 + beta_2 )/(1+exp(beta_0 + beta_2 )),
    pi00 = exp(beta_0  )/(1+exp(beta_0 )),
    cde = mu11*pL + mu10*(1-pL) - mu01*pL - mu00*(1-pL),
    sde = mu11*(1-pi11)*pL + mu10*(1-pi10)*(1-pL) - mu01*(1-pi11)*pL - mu00*(1-pi10)*(1-pL),
    sign = cde*sde,
    ratio = sde/cde,
    dif = sde - cde
  )
  prob11 <- a2 %>% select(mu11, pi11) %>% rename(mu = mu11, pi = pi11) %>% mutate(y=(1-pi)*mu)
  prob10 <- a2 %>% select(mu10, pi10) %>% rename(mu = mu10, pi = pi10) %>% mutate(y=(1-pi)*mu)
  prob01 <- a2 %>% select(mu01, pi01) %>% rename(mu = mu01, pi = pi01) %>% mutate(y=(1-pi)*mu)
  prob00 <- a2 %>% select(mu00, pi00) %>% rename(mu = mu00, pi = pi00) %>% mutate(y=(1-pi)*mu)
  prob <- bind_rows(prob11, prob10, prob01, prob00)
  summary(prob)


### EXPLORE
## Only Y is rare situation, assume interaction for Y model
min <- -0.3
max <- 0.3
theta_0 <- -3
beta_0 <- -3
pL <- 0.5
a2 <- a %>%
  mutate(
    mu11 = exp(theta_0 + theta_1 + theta_2 + theta_3)/(1+exp(theta_0 + theta_1 + theta_2 + theta_3)),
    mu10 = exp(theta_0 + theta_1)/(1+exp(theta_0 + theta_1)),
    mu01 = exp(theta_0 + theta_2)/(1+exp(theta_0 + theta_2)),
    mu00 = exp(theta_0 )/(1+exp(theta_0 )),
    pi11 = exp(beta_0 + beta_1 + beta_2 + beta_3)/(1+exp(beta_0 + beta_1 + beta_2 + beta_3)),
    pi10 = exp(beta_0 + beta_1 )/(1+exp(beta_0 + beta_1 )),
    pi01 = exp(beta_0 + beta_2 )/(1+exp(beta_0 + beta_2 )),
    pi00 = exp(beta_0  )/(1+exp(beta_0 )),
    cde = mu11*pL + mu10*(1-pL) - mu01*pL - mu00*(1-pL),
    sde = mu11*(1-pi11)*pL + mu10*(1-pi10)*(1-pL) - mu01*(1-pi11)*pL - mu00*(1-pi10)*(1-pL),
    sign = cde*sde,
    ratio = sde/cde,
    dif = sde - cde,
    abssum = abs(cde)+abs(sde)
  )
###Fixing D parameters
## theta
a3 <- a2 %>% 
  filter(beta_1 == 1.5, beta_2 == 1.5, beta_3 == 3, theta_3 == -3)
  a4 <- a3 %>% filter(sign < 0)
d5 <- ggplot(data=a3, aes(cde, sde, color=factor(theta_1))) + 
  geom_point(aes(shape=factor(theta_2))) +
  geom_abline(slope=1, intercept=0) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  theme_light() +
  xlab("Controlled direct effect") +
  ylab("Separable direct effect") +
  theme(
    plot.title = element_text(size=18),
    axis.title = element_text(size=15),
    axis.text = element_text(size=13)
  )  +
  coord_fixed(xlim=c(min, max),ylim=c(min, max))     + 
  ggtitle('(a) All coefficients are nonzero')  +
  guides(color = guide_legend(title=TeX("$\\theta_1$"), order = 1),
         shape = guide_legend(title=TeX("$\\theta_2$"), order = 2))
d5
a3 <- a2 %>% 
  filter(beta_1 == 0, beta_2 == 1.5, beta_3 == 0, theta_3 == -3)
d6 <- ggplot(data=a3, aes(cde, sde, color=factor(theta_1))) + 
  geom_point(aes(shape=factor(theta_2))) +
  geom_abline(slope=1, intercept=0) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  theme_light() +
  xlab("Controlled direct effect") +
  ylab("Separable direct effect") +
  theme(
    plot.title = element_text(size=18),
    axis.title = element_text(size=15),
    axis.text = element_text(size=13)
  )  +
  coord_fixed(xlim=c(min, max),ylim=c(min, max))     + 
  ggtitle('(b) No effect of A on D') +
  guides(color = guide_legend(title=TeX("$\\theta_1$"), order = 1),
         shape = guide_legend(title=TeX("$\\theta_2$"), order = 2))
d6
a3 <- a2 %>% 
  filter(beta_1 == 1.5, beta_2 == 0, beta_3 == 0, theta_3 == -3)
d7 <- ggplot(data=a3, aes(cde, sde, color=factor(theta_1))) + 
  geom_point(aes(shape=factor(theta_2))) +
  geom_abline(slope=1, intercept=0) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  theme_light() +
  xlab("Controlled direct effect") +
  ylab("Separable direct effect") +
  theme(
    plot.title = element_text(size=18),
    axis.title = element_text(size=15),
    axis.text = element_text(size=13)
  )  +
  coord_fixed(xlim=c(min, max),ylim=c(min, max))     + 
  ggtitle('(c) No effect of L on D') +
  guides(color = guide_legend(title=TeX("$\\theta_1$"), order = 1),
         shape = guide_legend(title=TeX("$\\theta_2$"), order = 2))
d7
a3 <- a2 %>% 
  filter(beta_1 == 1.5, beta_2 == 1.5, beta_3 == 0, theta_3 == -3)
d8 <- ggplot(data=a3, aes(cde, sde, color=factor(theta_1))) + 
  geom_point(aes(shape=factor(theta_2))) +
  geom_abline(slope=1, intercept=0) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  theme_light() +
  xlab("Controlled direct effect") +
  ylab("Separable direct effect") +
  theme(
    plot.title = element_text(size=18),
    axis.title = element_text(size=15),
    axis.text = element_text(size=13)
  )  +
  coord_fixed(xlim=c(min, max),ylim=c(min, max))     + 
  ggtitle('(d) No interaction of A-L on D') +
  guides(color = guide_legend(title=TeX("$\\theta_1$"), order = 1),
         shape = guide_legend(title=TeX("$\\theta_2$"), order = 2))
d8
fixbeta2 <- (d5+d6)/(d7+d8)
fixbeta2
ggsave("Fig9(extreme)3_theta_int.png", dpi=300, width = 27, height = 27, units = "cm")






### EXPLORE
## Only Y is rare situation, assume interaction for D model
min <- -0.1
max <- 0.25
theta_0 <- -3
beta_0 <- -3
pL <- 0.5
a2 <- a %>%
  mutate(
    mu11 = exp(theta_0 + theta_1 + theta_2 + theta_3)/(1+exp(theta_0 + theta_1 + theta_2 + theta_3)),
    mu10 = exp(theta_0 + theta_1)/(1+exp(theta_0 + theta_1)),
    mu01 = exp(theta_0 + theta_2)/(1+exp(theta_0 + theta_2)),
    mu00 = exp(theta_0 )/(1+exp(theta_0 )),
    pi11 = exp(beta_0 + beta_1 + beta_2 + beta_3)/(1+exp(beta_0 + beta_1 + beta_2 + beta_3)),
    pi10 = exp(beta_0 + beta_1 )/(1+exp(beta_0 + beta_1 )),
    pi01 = exp(beta_0 + beta_2 )/(1+exp(beta_0 + beta_2 )),
    pi00 = exp(beta_0  )/(1+exp(beta_0 )),
    cde = mu11*pL + mu10*(1-pL) - mu01*pL - mu00*(1-pL),
    sde = mu11*(1-pi11)*pL + mu10*(1-pi10)*(1-pL) - mu01*(1-pi11)*pL - mu00*(1-pi10)*(1-pL),
    sign = cde*sde,
    ratio = sde/cde,
    dif = sde - cde,
    abssum = abs(cde)+abs(sde)
  )
###Fixing Y parameters
## beta
a3 <- a2 %>% 
  filter(theta_1 == 1.5, theta_2 == 3, theta_3 == -3, beta_3 == 3)
  a4 <- a3 %>% filter(sign < 0)
d5 <- ggplot(data=a3, aes(cde, sde, color=factor(beta_1), shape=factor(beta_2))) + 
  geom_point() +
  geom_abline(slope=1, intercept=0) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  theme_light() +
  xlab("Controlled direct effect") +
  ylab("Separable direct effect") +
  theme(
    plot.title = element_text(size=18),
    axis.title = element_text(size=15),
    axis.text = element_text(size=13)
  )  +
  coord_fixed(xlim=c(min, max),ylim=c(min, max))     + 
  ggtitle('(a) All coefficients are nonzero') +
  guides(color = guide_legend(title=TeX("$\\beta_1$"), order = 1),
         shape = guide_legend(title=TeX("$\\beta_2$"), order = 2))
d5
a3 <- a2 %>% 
  filter(theta_1 == 0, theta_2 == 3, theta_3 == 0, beta_3 == 3)
d6 <- ggplot(data=a3, aes(cde, sde, color=factor(beta_1))) + 
  geom_point(aes(shape=factor(beta_2))) +
  geom_abline(slope=1, intercept=0) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  theme_light() +
  xlab("Controlled direct effect") +
  ylab("Separable direct effect") +
  theme(
    plot.title = element_text(size=18),
    axis.title = element_text(size=15),
    axis.text = element_text(size=13)
  )  +
  coord_fixed(xlim=c(min, max),ylim=c(min, max))     + 
  ggtitle('(b) No effect of A on Y') +
  guides(color = guide_legend(title=TeX("$\\beta_1$"), order = 1),
         shape = guide_legend(title=TeX("$\\beta_2$"), order = 2))
d6
a3 <- a2 %>% 
  filter(theta_1 == 1.5, theta_2 == 0, theta_3 == 0, beta_3 == 3)
d7 <- ggplot(data=a3, aes(cde, sde, color=factor(beta_1))) + 
  geom_point(aes(shape=factor(beta_2))) +
  geom_abline(slope=1, intercept=0) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  theme_light() +
  xlab("Controlled direct effect") +
  ylab("Separable direct effect") +
  theme(
    plot.title = element_text(size=18),
    axis.title = element_text(size=15),
    axis.text = element_text(size=13)
  )  +
  coord_fixed(xlim=c(min, max),ylim=c(min, max))     + 
  ggtitle('(c) No effect of L on Y') +
  guides(color = guide_legend(title=TeX("$\\beta_1$"), order = 1),
         shape = guide_legend(title=TeX("$\\beta_2$"), order = 2))
d7
a3 <- a2 %>% 
  filter(theta_1 == 1.5, theta_2 == 3, theta_3 == 0, beta_3 == 3)
d8 <- ggplot(data=a3, aes(cde, sde, color=factor(beta_1))) + 
  geom_point(aes(shape=factor(beta_2))) +
  geom_abline(slope=1, intercept=0) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  theme_light() +
  xlab("Controlled direct effect") +
  ylab("Separable direct effect") +
  theme(
    plot.title = element_text(size=18),
    axis.title = element_text(size=15),
    axis.text = element_text(size=13)
  )  +
  coord_fixed(xlim=c(min, max),ylim=c(min, max))     + 
  ggtitle('(d) No interaction of A-L on Y') +
  guides(color = guide_legend(title=TeX("$\\beta_1$"), order = 1),
         shape = guide_legend(title=TeX("$\\beta_2$"), order = 2))
d8
fixbeta <- (d5+d6)/(d7+d8)
fixbeta
ggsave("Fig11(extreme)5_beta_int.png", dpi=300, width = 27, height = 27, units = "cm")









