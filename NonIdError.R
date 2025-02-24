
library(dplyr)
library(ggplot2)
#library(latex2exp)
library(patchwork)


## functions
tevent <- function(c_0, c_1, c_2, c_3, c_4, c_5, c_6, a, l, u){
  linear_pred <- (c_0 + c_1*a  + c_2*l + c_3*a*l + c_4*u + c_5*a*u + c_6*l*u)
  output <- exp(linear_pred)/(1+exp(linear_pred))
  return(output)
}
tcompeting <- function(c_0, c_1, c_2, c_3, c_4, c_5, c_6, a, l, u){
  linear_pred <- (c_0 + c_1*a  + c_2*l + c_3*a*l + c_4*u + c_5*a*u + c_6*l*u)
  output <- exp(linear_pred)/(1+exp(linear_pred))
  return(output)
}
tcov <- function(c_0, c_1, u){
  linear_pred <- (c_0 + c_1*u)
  output <- exp(linear_pred)/(1+exp(linear_pred))
  return(output)
}



# m <- c(-1, 1)
# n <- c(-1, 1)
# o <- c(-1, 1)
# p <- c(-1, 1)
# q <- c(-1, 1)
# r <- c(-1, 1)
# s <- c(-1, 1)
# t <- c(-1, 1)
# u <- c(-1, 1)
# v <- c(-1, 1)
# w <- c(-1, 1)
# x <- c(-1, 1)

# m <- c(-1, 1,2)
# n <- c(-1, 1,2)
# o <- c(-1, 1,2)
# p <- c(-1, 1,2)
# q <- c(-1, 1,2)
# r <- c(-1, 1,2)
# s <- c(-1, 1,2)
# t <- c(-1, 1,2)
# u <- c(-1, 1,2)
# v <- c(-1, 1,2)
# w <- c(-1, 1,2)
# x <- c(-1, 1,2)

m <- c(-2, -1, 1,2)
n <- c(-2, -1, 1,2)
o <- c(-2, -1, 1,2)
p <- c(-2, -1, 1,2)
q <- c(-2, -1, 1,2)
r <- c(-2, -1, 1,2)
s <- c(-2, -1, 1,2)
t <- c(-2, -1, 1,2)
u <- c(-2, -1, 1,2)
v <- c(-2, -1, 1,2)
w <- c(-2, -1, 1,2)
x <- c(-2, -1, 1,2)

var <- data.frame(m,n,o,p,q,r,s,t,u,v,w,x)
sim <- expand.grid(var)
b <- sim %>% 
  mutate(
    theta_1=m/2,
    theta_2=n/2,
    theta_3=o/2,
    theta_4=p/2,
    theta_5=q/2,
    theta_6=r/2,
    beta_1=s/2,
    beta_2=t/2,
    beta_3=u/2,
    beta_4=v/2,
    beta_5=w/2,
    beta_6=x/2
  )

## U exists and D is frequent
beta_0 <- -1
theta_0 <- -1
pL <- 0.5
phi_0 <- 0
phi_1 <- 0
pU <- 0.5
if(phi_0 == 0 & phi_1 == 0){
  tg11 <-  pL*pU
  tg10 <-  pL*(1-pU)
  tg01 <-  (1-pL)*pU
  tg00 <-  (1-pL)*(1-pU)
} else{
  tg11 <-  tcov(c_0 = phi_0, c_1 = phi_1, u = 1)*pU
  tg10 <-  tcov(c_0 = phi_0, c_1 = phi_1, u = 0)*(1-pU)
  tg01 <-  (1-tcov(c_0 = phi_0, c_1 = phi_1, u = 1))*pU
  tg00 <-  (1-tcov(c_0 = phi_0, c_1 = phi_1, u = 0))*(1-pU)
}
b2 <- b %>%
  mutate(
    tmu111 = tevent(c_0 = theta_0, c_1 = theta_1, c_2 = theta_2, c_3 = theta_3, c_4 = theta_4, c_5 = theta_5, c_6 = theta_6, a = 1, l = 1, u = 1),
    tmu110 = tevent(c_0 = theta_0, c_1 = theta_1, c_2 = theta_2, c_3 = theta_3, c_4 = theta_4, c_5 = theta_5, c_6 = theta_6, a = 1, l = 1, u = 0),
    tmu101 = tevent(c_0 = theta_0, c_1 = theta_1, c_2 = theta_2, c_3 = theta_3, c_4 = theta_4, c_5 = theta_5, c_6 = theta_6, a = 1, l = 0, u = 1),
    tmu100 = tevent(c_0 = theta_0, c_1 = theta_1, c_2 = theta_2, c_3 = theta_3, c_4 = theta_4, c_5 = theta_5, c_6 = theta_6, a = 1, l = 0, u = 0),
    tmu011 = tevent(c_0 = theta_0, c_1 = theta_1, c_2 = theta_2, c_3 = theta_3, c_4 = theta_4, c_5 = theta_5, c_6 = theta_6, a = 0, l = 1, u = 1),
    tmu010 = tevent(c_0 = theta_0, c_1 = theta_1, c_2 = theta_2, c_3 = theta_3, c_4 = theta_4, c_5 = theta_5, c_6 = theta_6, a = 0, l = 1, u = 0),
    tmu001 = tevent(c_0 = theta_0, c_1 = theta_1, c_2 = theta_2, c_3 = theta_3, c_4 = theta_4, c_5 = theta_5, c_6 = theta_6, a = 0, l = 0, u = 1),
    tmu000 = tevent(c_0 = theta_0, c_1 = theta_1, c_2 = theta_2, c_3 = theta_3, c_4 = theta_4, c_5 = theta_5, c_6 = theta_6, a = 0, l = 0, u = 0),
    tpi111 =  tcompeting(c_0 = beta_0, c_1 = beta_1, c_2 = beta_2, c_3 = beta_3, c_4 = beta_4, c_5 = beta_5, c_6 = beta_6, a = 1, l = 1, u = 1),
    tpi110 =  tcompeting(c_0 = beta_0, c_1 = beta_1, c_2 = beta_2, c_3 = beta_3, c_4 = beta_4, c_5 = beta_5, c_6 = beta_6, a = 1, l = 1, u = 0),
    tpi101 =  tcompeting(c_0 = beta_0, c_1 = beta_1, c_2 = beta_2, c_3 = beta_3, c_4 = beta_4, c_5 = beta_5, c_6 = beta_6, a = 1, l = 0, u = 1),
    tpi100 =  tcompeting(c_0 = beta_0, c_1 = beta_1, c_2 = beta_2, c_3 = beta_3, c_4 = beta_4, c_5 = beta_5, c_6 = beta_6, a = 1, l = 0, u = 0),
    tpi011 =  tcompeting(c_0 = beta_0, c_1 = beta_1, c_2 = beta_2, c_3 = beta_3, c_4 = beta_4, c_5 = beta_5, c_6 = beta_6, a = 0, l = 1, u = 1),
    tpi010 =  tcompeting(c_0 = beta_0, c_1 = beta_1, c_2 = beta_2, c_3 = beta_3, c_4 = beta_4, c_5 = beta_5, c_6 = beta_6, a = 0, l = 1, u = 0),
    tpi001 =  tcompeting(c_0 = beta_0, c_1 = beta_1, c_2 = beta_2, c_3 = beta_3, c_4 = beta_4, c_5 = beta_5, c_6 = beta_6, a = 0, l = 0, u = 1),
    tpi000 =  tcompeting(c_0 = beta_0, c_1 = beta_1, c_2 = beta_2, c_3 = beta_3, c_4 = beta_4, c_5 = beta_5, c_6 = beta_6, a = 0, l = 0, u = 0),

    
    obsmu11 = ((tmu111)*(1-tpi111)*pL*pU + (tmu110)*(1-tpi110)*pL*(1-pU)) / ((1-tpi111)*pL*pU + (1-tpi110)*pL*(1-pU)),
    obsmu10 = ((tmu101)*(1-tpi101)*(1-pL)*pU + (tmu100)*(1-tpi100)*(1-pL)*(1-pU)) / ((1-tpi101)*(1-pL)*pU + (1-tpi100)*(1-pL)*(1-pU)),
    obsmu01 = ((tmu011)*(1-tpi011)*pL*pU + (tmu010)*(1-tpi010)*pL*(1-pU)) / ((1-tpi011)*pL*pU + (1-tpi010)*pL*(1-pU)),
    obsmu00 = ((tmu001)*(1-tpi001)*(1-pL)*pU + (tmu000)*(1-tpi000)*(1-pL)*(1-pU)) / ((1-tpi001)*(1-pL)*pU + (1-tpi000)*(1-pL)*(1-pU)),
    obspi11 = (tpi111*pL*pU + tpi110*pL*(1-pU)) / (pL*pU + pL*(1-pU)),
    obspi10 = (tpi101*(1-pL)*pU + tpi100*(1-pL)*(1-pU)) / ((1-pL)*pU + (1-pL)*(1-pU)),
    obspi01 = (tpi011*pL*pU + tpi010*pL*(1-pU)) / (pL*pU + pL*(1-pU)),
    obspi00 = (tpi001*(1-pL)*pU + tpi000*(1-pL)*(1-pU)) / ((1-pL)*pU + (1-pL)*(1-pU)),
    tilde_cde1 = obsmu11 * pL + obsmu10 * (1-pL), 
    tilde_cde0 = obsmu01 * pL + obsmu00 * (1-pL),
    tilde_cde1l1 = obsmu11 ,
    tilde_cde1l0 = obsmu10 ,
    tilde_cde0l1 = obsmu01 ,
    tilde_cde0l0 = obsmu00,
    tilde_sde11 = obsmu11 * (1-obspi11) * pL + obsmu10 * (1-obspi10) *(1-pL),
    tilde_sde10 = obsmu01 * (1-obspi11) * pL + obsmu00 * (1-obspi10) *(1-pL),
    tilde_sde01 = obsmu11 * (1-obspi01) * pL + obsmu10 * (1-obspi00) *(1-pL),
    tilde_sde00 = obsmu01 * (1-obspi01) * pL + obsmu00 * (1-obspi00) *(1-pL),
 
    tcde1 = tmu111*tg11 + tmu110*tg10 + tmu101*tg01 + tmu100*tg00,
    tcde0 = tmu011*tg11 + tmu010*tg10 + tmu001*tg01 + tmu000*tg00,
    tcde1l1 = tmu111*(tg11/(tg11+tg10)) + tmu110*(tg10/(tg11+tg10)),
    tcde1l0 = tmu101*(tg01/(tg01+tg00)) + tmu100*(tg00/(tg01+tg00)),
    tcde0l1 = tmu011*(tg11/(tg11+tg10)) + tmu010*(tg10/(tg11+tg10)),
    tcde0l0 = tmu001*(tg01/(tg01+tg00)) + tmu000*(tg00/(tg01+tg00)),
    tsde11 = tmu111*(1-tpi111)*tg11 + tmu110*(1-tpi110)*tg10 + tmu101*(1-tpi101)*tg01 + tmu100*(1-tpi100)*tg00,
    tsde01 = tmu011*(1-tpi111)*tg11 + tmu010*(1-tpi110)*tg10 + tmu001*(1-tpi101)*tg01 + tmu000*(1-tpi100)*tg00,
    tsde10 = tmu111*(1-tpi011)*tg11 + tmu110*(1-tpi010)*tg10 + tmu101*(1-tpi001)*tg01 + tmu100*(1-tpi000)*tg00,
    tsde00 = tmu011*(1-tpi011)*tg11 + tmu010*(1-tpi010)*tg10 + tmu001*(1-tpi001)*tg01 + tmu000*(1-tpi000)*tg00,
    
    tcde = tcde1 - tcde0,
    tsde1 = tsde11 - tsde01,
    tsde0 = tsde10 - tsde00,

    # dif_cdesde1 = hat_cde - hat_sde1,
    # dif_cdesde0 = hat_cde - hat_sde0,
    # dif_sde1sde0 = hat_sde1 - hat_sde0,
    # bias_cdecde = hat_cde - tcde,
    # bias_sde1sde1 = hat_sde1 - tsde1,
    # bias_sde0sde0 = hat_sde0 - tsde0,
    # bias_cdesde1 = hat_cde - tsde1,
    # bias_cdesde0 = hat_cde - tsde0,
    tilde_cde = tilde_cde1 - tilde_cde0,
    tilde_sde1 = tilde_sde11 - tilde_sde10,
    tilde_sde0 = tilde_sde01 - tilde_sde00,
    bias1 = tcde - tsde0,
    bias2 = tilde_cde - tcde,
    # bias3 = hat_cde - tilde_cde,
    bias2_sde0 = tilde_sde0 - tsde0,
    # bias3_sde0 = hat_sde0 - tilde_sde0,
    # bias3_cde_1 = est_abs$hat_cde1 - tilde_cde1,
    # bias3_cde_0 = est_abs$hat_cde0 - tilde_cde0,
    # bias3_cde_1l1 = est_abs$hat_cde1l1 - tilde_cde1l1,
    # bias3_cde_1l0 = est_abs$hat_cde1l0 - tilde_cde1l0,
    # bias3_cde_0l1 = est_abs$hat_cde0l1 - tilde_cde0l1,
    # bias3_cde_0l0 = est_abs$hat_cde0l0 - tilde_cde0l0,
    
    sign = as.factor(ifelse(tcde*tsde0<0, 1, 0)),
    ratio = tsde0/tcde,
    dif = tsde0 - tcde,
  )
max(b2$tpi111)
d1 <- ggplot(data=b2, aes(tcde, tilde_cde)) + 
  geom_point() +
  geom_abline(slope=1, intercept=0) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Factual data function") +
  ylab("Observed data function") +
  theme(
    plot.title = element_text(size=20),
    axis.title = element_text(size=21),
    axis.text = element_text(size=20),
    legend.position = "none"
  ) +
  coord_fixed(xlim=c(-0.5, 0.5),ylim=c(-0.5, 0.5))  + 
  ggtitle("Controlled direct effect")
#d1

  
d2 <- ggplot(data=b2, aes(tsde0, tilde_sde0)) + 
  geom_point() +
  geom_abline(slope=1, intercept=0) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Factual data function") +
  ylab(" ") +
  theme(
    plot.title = element_text(size=20),
    axis.title = element_text(size=21),
    axis.text = element_text(size=20),
    legend.position = "none"
  ) +
  coord_fixed(xlim=c(-0.5, 0.5),ylim=c(-0.5, 0.5))  + 
  ggtitle(expression(paste("Separable direct effect", " (", {a[D]},"=0)", sep="")))
#d2

d3 <- ggplot(data=b2, aes(tsde0, tilde_sde0)) + 
  geom_point() +
  geom_abline(slope=1, intercept=0) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Factual data function") +
  ylab(" ") +
  theme(
    plot.title = element_text(size=20),
    axis.title = element_text(size=21),
    axis.text = element_text(size=20),
    legend.position = "none"
  ) +
  coord_fixed(xlim=c(-0.5, 0.5),ylim=c(-0.5, 0.5))  + 
  ggtitle(expression(paste("Separable direct effect", " (", {a[D]},"=1)", sep="")))
#d3

all <- (d1+d2+d3)
all

current_day <- Sys.time()
format_day <- format(current_day, "%Y%m%d_%H%M%S")
#ggsave(paste("figures/NonIdError_", format_day, ".pdf", sep=""), dpi=300, width = 42, height = 14, units = "cm")
ggsave(paste("figures/NonIdError_", format_day, ".png", sep=""), dpi=300, width = 42, height = 14, units = "cm")

