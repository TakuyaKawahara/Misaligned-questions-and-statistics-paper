
library(dplyr)
library(ggplot2)
library(latex2exp)
library(patchwork)


a <- data.frame();
for (m in -2:2) {
  for (n in -2:2){
    for (o in -4:4) {
      for (p in -2:2){
        for (q in -2:2) {
          for (r in -4:4){
            a2 <- data.frame(
              theta_1=m/4,
              theta_2=n/4,
              theta_3=o/4,
              beta_1=p/4,
              beta_2=q/4,
              beta_3=r/4
            )
            a <- bind_rows(a, a2)
          }}}}}}


# ALL MARGINAL GRAPHS
# both rare
theta_0 <- -5
beta_0 <- -5
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
    sign = as.factor(ifelse(cde*sde<0, 1, 0)),
    ratio = sde/cde,
    dif = sde - cde
  )
  prob11 <- a2 %>% select(mu11, pi11) %>% rename(mu = mu11, pi = pi11) %>% mutate(y=(1-pi)*mu)
  prob10 <- a2 %>% select(mu10, pi10) %>% rename(mu = mu10, pi = pi10) %>% mutate(y=(1-pi)*mu)
  prob01 <- a2 %>% select(mu01, pi01) %>% rename(mu = mu01, pi = pi01) %>% mutate(y=(1-pi)*mu)
  prob00 <- a2 %>% select(mu00, pi00) %>% rename(mu = mu00, pi = pi00) %>% mutate(y=(1-pi)*mu)
  prob <- bind_rows(prob11, prob10, prob01, prob00)
  summary(prob)
d1 <- ggplot(data=a2, aes(cde, sde, colour=sign)) + 
  geom_point() +
  geom_abline(slope=1, intercept=0) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Controlled direct effect") +
  ylab("Separable direct effect") +
  theme(
    plot.title = element_text(size=22),
    axis.title = element_text(size=21),
    axis.text = element_text(size=20),
    legend.position = "none"
    ) +
  coord_fixed(xlim=c(-0.01, 0.02),ylim=c(-0.01, 0.02))  + 
  ggtitle("(a) Both rare")+
  scale_color_manual(values=(c("black", "red")))
d1
# Only Y is rare
theta_0 <- -5
beta_0 <- -1
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
    sign = as.factor(ifelse(cde*sde<0, 1, 0)),
    ratio = sde/cde,
    dif = sde - cde
  )
d2 <- ggplot(data=a2, aes(cde, sde, colour=sign)) + 
  geom_point() +
  geom_abline(slope=1, intercept=0) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Controlled direct effect") +
  ylab("Separable direct effect") +
  theme(
    plot.title = element_text(size=22),
    axis.title = element_text(size=21),
    axis.text = element_text(size=20),
    legend.position = "none"
  ) +
  coord_fixed(xlim=c(-0.01, 0.02),ylim=c(-0.01, 0.02))  + 
  ggtitle("(b) Event of interest rare")+
  scale_color_manual(values=(c("black", "red")))
d2
# Only D is rare
theta_0 <- -1
beta_0 <- -5
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
    sign = as.factor(ifelse(cde*sde<0, 1, 0)),
    ratio = sde/cde,
    dif = sde - cde
  )
d3 <- ggplot(data=a2, aes(cde, sde, colour=sign)) + 
  geom_point() +
  geom_abline(slope=1, intercept=0) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Controlled direct effect") +
  ylab("Separable direct effect") +
  theme(
    plot.title = element_text(size=22),
    axis.title = element_text(size=21),
    axis.text = element_text(size=20),
    legend.position = "none"
    ) +
  coord_fixed(xlim=c(-0.25, 0.25),ylim=c(-0.25, 0.25))  + 
  ggtitle("(c) Competing event rare")+
  scale_color_manual(values=(c("black", "red")))
d3
## Both are not rare
theta_0 <- -1
beta_0 <- -1
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
    sign = as.factor(ifelse(cde*sde<0, 1, 0)),
    ratio = sde/cde,
    dif = sde - cde
  )
  prob11 <- a2 %>% select(mu11, pi11) %>% rename(mu = mu11, pi = pi11) %>% mutate(y=(1-pi)*mu)
  prob10 <- a2 %>% select(mu10, pi10) %>% rename(mu = mu10, pi = pi10) %>% mutate(y=(1-pi)*mu)
  prob01 <- a2 %>% select(mu01, pi01) %>% rename(mu = mu01, pi = pi01) %>% mutate(y=(1-pi)*mu)
  prob00 <- a2 %>% select(mu00, pi00) %>% rename(mu = mu00, pi = pi00) %>% mutate(y=(1-pi)*mu)
  prob <- bind_rows(prob11, prob10, prob01, prob00)
  summary(prob)
d4 <- ggplot(data=a2, aes(cde, sde, colour=sign)) + 
  geom_point() +
  geom_abline(slope=1, intercept=0) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Controlled direct effect") +
  ylab("Separable direct effect") +
  theme(
    plot.title = element_text(size=22),
    axis.title = element_text(size=21),
    axis.text = element_text(size=20),
    legend.position = "none"
  ) +
  coord_fixed(xlim=c(-0.25, 0.25),ylim=c(-0.25, 0.25))  + 
  ggtitle("(d) Neither rare")+
  scale_color_manual(values=(c("black", "red")))
d4
marginal <- (d1+d2)/(d3+d4)
marginal
ggsave("Fig1_Marginal.png", dpi=300, width = 27, height = 27, units = "cm")
