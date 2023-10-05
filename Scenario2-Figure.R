
library(dplyr)
library(ggplot2)
library(latex2exp)
library(patchwork)


b <- data.frame();
for (m in -1:1){
  for (n in -1:1){
    for (o in -1:1){
      for (p in 1:2){
        for (q in 1:2){
          for (r in 1:2){
            for (s in -1:1){
              for (t in -1:1){
                for (u in -1:1){
                  for (v in 1:2){
                    for (w in 1:2){
                      for (x in 1:2){
              b2 <- data.frame(
                theta_1=m/2,
                theta_2=n/2,
                theta_3=o/2,
                theta_4=p/4,
                theta_5=q/4,
                theta_6=r/4,
                beta_1=s/2,
                beta_2=t/2,
                beta_3=u/2,
                beta_4=v/4,
                beta_5=w/4,
                beta_6=x/4
              )
              b <- bind_rows(b, b2)
            }}}}}}}}}}}}


## Non-rare case
beta_0 <- -1
theta_0 <- -1
phi_0 <- -0.5
phi_1 <- 0
pU <- 0.3
b2 <- b %>%
  mutate(
    mu111 = exp(theta_0 + theta_1 + theta_2 + theta_3 + theta_4 + theta_5 + theta_6)
    /(1+exp(theta_0 + theta_1 + theta_2 + theta_3 + theta_4 + theta_5 + theta_6)),
    mu110 = exp(theta_0 + theta_1 + theta_2 + theta_3 )
    /(1+exp(theta_0 + theta_1 + theta_2 + theta_3 )),
    mu101 = exp(theta_0 + theta_1 + theta_4 + theta_5)
    /(1+exp(theta_0 + theta_1 + theta_4 + theta_5)),
    mu100 = exp(theta_0 + theta_1)/(1+exp(theta_0 + theta_1)),
    mu011 = exp(theta_0 + theta_2 + theta_4 + theta_6)
    /(1+exp(theta_0 + theta_2 + theta_4 + theta_6)),
    mu010 = exp(theta_0 + theta_2)/(1+exp(theta_0 + theta_2)),
    mu001 = exp(theta_0 + theta_4 )/(1+exp(theta_0 + theta_4)),
    mu000 = exp(theta_0 )/(1+exp(theta_0 )),
    pi111 = exp(beta_0 + beta_1 + beta_2 + beta_3 + beta_4 + beta_5 + beta_6)
    /(1+exp(beta_0 + beta_1 + beta_2 + beta_3 + beta_4 + beta_5 + beta_6)),
    pi110 = exp(beta_0 + beta_1 + beta_2 + beta_3 )
    /(1+exp(beta_0 + beta_1 + beta_2 + beta_3 )),
    pi101 = exp(beta_0 + beta_1 + beta_4 + beta_5)
    /(1+exp(beta_0 + beta_1 + beta_4 + beta_5)),
    pi100 = exp(beta_0 + beta_1)/(1+exp(beta_0 + beta_1)),
    pi011 = exp(beta_0 + beta_2 + beta_4 + beta_6)
    /(1+exp(beta_0 + beta_2 + beta_4 + beta_6)),
    pi010 = exp(beta_0 + beta_2)/(1+exp(beta_0 + beta_2)),
    pi001 = exp(beta_0 + beta_4 )/(1+exp(beta_0 + beta_4)),
    pi000 = exp(beta_0 )/(1+exp(beta_0 )),
    g11 = exp(phi_0 + phi_1)/(1+exp(phi_0 + phi_1))*pU,
    g10 = exp(phi_0 + phi_1)/(1+exp(phi_0 + phi_1))*(1-pU),
    g01 = 1/(1+exp(phi_0 + phi_1))*pU,
    g00 = 1/(1+exp(phi_0 + phi_1))*(1-pU),
    pL = g11 + g10,
    mu11 = (mu111*(1-pi111)*(g11) + mu110*(1-pi110)*(g10))/((1-pi111)*(g11) + (1-pi110)*(g10)),
    mu10 = (mu101*(1-pi101)*(g01) + mu100*(1-pi100)*(g00))/((1-pi101)*(g01) + (1-pi100)*(g00)),
    mu01 = (mu011*(1-pi011)*(g11) + mu010*(1-pi010)*(g10))/((1-pi011)*(g11) + (1-pi010)*(g10)),
    mu00 = (mu001*(1-pi001)*(g01) + mu000*(1-pi000)*(g00))/((1-pi001)*(g01) + (1-pi000)*(g00)),
    pi11 = (pi111*(g11) + pi110*(g10))/((g11) + (g10)),
    pi10 = (pi101*(g01) + pi100*(g00))/((g01) + (g00)),
    pi01 = (pi011*(g11) + pi010*(g10))/((g11) + (g10)),
    pi00 = (pi001*(g01) + pi000*(g00))/((g01) + (g00)),
    cde = mu11*pL + mu10*(1-pL) - mu01*pL - mu00*(1-pL),
    sde = mu11*(1-pi11)*pL + mu10*(1-pi10)*(1-pL) - mu01*(1-pi11)*pL - mu00*(1-pi10)*(1-pL),
    sign = cde*sde,
    ratio = sde/cde,
    dif = sde - cde,
    tcde = mu111*g11 + mu110*g10 + mu101*g01 + mu100*g00 
    - (mu011*g11 + mu010*g10 + mu001*g01 + mu000*g00),
    tsde = mu111*(1-pi111)*g11 + mu110*(1-pi110)*g10 + mu101*(1-pi101)*g01 + mu100*(1-pi100)*g00
    - (mu011*(1-pi111)*g11 + mu010*(1-pi110)*g10 + mu001*(1-pi101)*g01 + mu000*(1-pi100)*g00),
    bias_cde = cde - tcde,
    bias_sde = sde - tsde,
    bias_cde_1 = (mu11*pL + mu10*(1-pL)) - (mu111*g11 + mu110*g10 + mu101*g01 + mu100*g00),
    bias_cde_0 = (mu01*pL + mu00*(1-pL)) - (mu011*g11 + mu010*g10 + mu001*g01 + mu000*g00),
    bias_sde_1 = (mu11*(1-pi11)*pL + mu10*(1-pi10)*(1-pL)) -
      (mu111*(1-pi111)*(g11) + mu110*(1-pi110)*(g10) + mu101*(1-pi101)*(g01) + mu100*(1-pi100)*(g00)),
    bias_sde_0 = (mu01*(1-pi11)*pL + mu00*(1-pi10)*(1-pL)) -
      (mu011*(1-pi111)*(g11) + mu010*(1-pi110)*(g10) + mu001*(1-pi101)*(g01) + mu000*(1-pi100)*(g00))
  )
prob11 <- b2 %>% select(mu11, pi11) %>% rename(mu = mu11, pi = pi11) %>% mutate(y=(1-pi)*mu)
prob10 <- b2 %>% select(mu10, pi10) %>% rename(mu = mu10, pi = pi10) %>% mutate(y=(1-pi)*mu)
prob01 <- b2 %>% select(mu01, pi01) %>% rename(mu = mu01, pi = pi01) %>% mutate(y=(1-pi)*mu)
prob00 <- b2 %>% select(mu00, pi00) %>% rename(mu = mu00, pi = pi00) %>% mutate(y=(1-pi)*mu)
prob <- bind_rows(prob11, prob10, prob01, prob00)
summary(prob)
d1 <- ggplot(data=b2, aes(tcde, tsde)) + 
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
  ) +
  coord_fixed()  + 
  ggtitle('(a) Plot of the effects')
d1

b3_cde <- b2 %>% 
  mutate(bias = bias_cde, bias1 = bias_cde_1, bias0 = bias_cde_0, est = "CDE", pbias = bias_cde/cde*100 )
b3_sde <- b2 %>% 
  mutate(bias = bias_sde, bias1 = bias_sde_1, bias0 = bias_sde_0, est = "SDE", pbias = bias_sde/sde*100 )
b4 <- bind_rows(b3_cde, b3_sde)
b5_1 <- b3_cde %>% select(bias_cde_1) %>% mutate(type="3cde1", bias=bias_cde_1)
b5_2 <- b3_cde %>% select(bias_cde_0) %>% mutate(type="4cde0", bias=bias_cde_0)
b5_3 <- b3_sde %>% select(bias_sde_1) %>% mutate(type="5sde1", bias=bias_sde_1)
b5_4 <- b3_sde %>% select(bias_sde_0) %>% mutate(type="6sde0", bias=bias_sde_0)
b5_5 <- b3_cde %>% select(bias_cde) %>% mutate(type="1cde", bias=bias_cde)
b5_6 <- b3_sde %>% select(bias_sde) %>% mutate(type="2sde", bias=bias_sde)
b5 <- bind_rows(b5_1, b5_2, b5_3, b5_4, b5_5, b5_6)


g2 <- ggplot(b5, aes(type, bias))+
  geom_boxplot() + 
  geom_jitter(width = 0.1, height = 0) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab(" ") +
  ylab("(Biased observed data function) \n — (Correct identifying function)") +
  theme(
    axis.title.y = element_text(size=15),
    axis.text.y = element_text(size=13),
    axis.text.x = element_text(angle=10,hjust=0.9, size = 13)
  ) +
  scale_y_continuous(labels = scales::comma) + #limits = c(-0.006, 0.002)
  geom_vline(xintercept=2.5) + 
  scale_x_discrete(labels=c(parse(text = TeX('$\\tilde{\\psi}-\\psi$')),
                            parse(text = TeX('$\\tilde{\\gamma}(a_D=1)-\\gamma(a_D=1)$')),
                            parse(text = TeX('$\\tilde{Pr}(Y^{a=1, d=0}=1)-Pr(Y^{a=1, d=0}=1)$')),
                            parse(text = TeX('$\\tilde{Pr}(Y^{a=0, d=0}=1)-Pr(Y^{a=0, d=0}=1)$')),
                            parse(text = TeX('$\\tilde{Pr}(Y^{a_Y=1, a_D=1}=1)-Pr(Y^{a_Y=1, a_D=1}=1)$')),
                            parse(text = TeX('$\\tilde{Pr}(Y^{a_Y=0, a_D=1}=1)-Pr(Y^{a_Y=0, a_D=1}=1)$'))))                            
                            # parse(text = TeX('$\\tilde{Pr}(Y^{a=1, d=0}) - Pr(Y^{a=1, d=0})$')),
                            # parse(text = TeX('$\\tilde{Pr}(Y^{a=0, d=0}) - Pr(Y^{a=0, d=0})$')),
                            # parse(text = TeX('$\\tilde{Pr}(Y^{a_Y=1, a_d=0}) - Pr(Y^{a_Y=1, a_d=0})$')),
                            # parse(text = TeX('$\\tilde{Pr}(Y^{a_Y=0, a_d=0}) - Pr(Y^{a_Y=0, a_d=0})$'))))
g2
ggsave("Fig15_U_nonrare.png", dpi=300, width = 27, height = 13, units = "cm")

