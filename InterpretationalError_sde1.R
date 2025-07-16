
library(dplyr)
library(ggplot2)
library(patchwork)


## functions
tevent <- function(c_0, c_1, c_2, c_3, c_4, c_5, c_6, a, l, u){
  linear_pred <- (c_0 + c_1*a + c_2*l + c_3*a*l + c_4*u + c_5*a*u + c_6*l*u)
  output <- exp(linear_pred)/(1+exp(linear_pred))
  return(output)
}
tcompeting <- function(c_0, c_1, c_2, c_3, c_4, c_5, c_6, a, l, u){
  linear_pred <- (c_0 + c_1*a + c_2*l + c_3*a*l + c_4*u + c_5*a*u + c_6*l*u)
  output <- exp(linear_pred)/(1+exp(linear_pred))
  return(output)
}

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

## setting1: U exists and D is rare
beta_0 <- -9
theta_0 <- -1
pL <- 0.5
pU <- 0.5
  tg11 <-  pL*pU
  tg10 <-  pL*(1-pU)
  tg01 <-  (1-pL)*pU
  tg00 <-  (1-pL)*(1-pU)
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

    sign = as.factor(ifelse(tcde*tsde1<0, 1, 0)),
    dif = tcde - tsde1
  )
max(b2$tpi111)
d1 <- ggplot(data=b2, aes(tsde1, dif)) + 
  geom_point() +
#  geom_abline(slope=1, intercept=0) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab(" ") +
  ylab(expression(paste(CDE[0] - SDE[0]^{a[D]== 1}))) +
  theme(
    plot.title = element_text(size=20),
    axis.title = element_text(size=21),
    axis.text = element_text(size=20),
    legend.position = "none"
  ) +
  coord_fixed(xlim=c(-0.45, 0.45),ylim=c(-0.45, 0.45))  + 
  ggtitle("(a) D is rare and U exists")
  #scale_color_manual(values=(c("black", "red")))
  #d1



## setting 2 : U exists and D is non-rare
beta_0 <- -1
theta_0 <- -1
pL <- 0.5
phi_0 <- 0
phi_1 <- 0
pU <- 0.5
  tg11 <-  pL*pU
  tg10 <-  pL*(1-pU)
  tg01 <-  (1-pL)*pU
  tg00 <-  (1-pL)*(1-pU)
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
    
    sign = as.factor(ifelse(tcde*tsde1<0, 1, 0)),
    dif = tcde - tsde1
  )
max(b2$tpi111)
d2 <- ggplot(data=b2, aes(tsde1, dif)) + 
  geom_point() +
#  geom_abline(slope=1, intercept=0) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab(" ") +
  ylab(" ") +
  theme(
    plot.title = element_text(size=20),
    axis.title = element_text(size=21),
    axis.text = element_text(size=20),
    legend.position = "none"
  ) +
  coord_fixed(xlim=c(-0.45, 0.45),ylim=c(-0.45, 0.45))  + 
  ggtitle("(b) D is non-rare and U exists")
  #scale_color_manual(values=(c("black", "red")))
#d2


## setting 3 : U not exists and D is rare
beta_0 <- -6
theta_0 <- -1
pL <- 0.5
phi_0 <- 0
phi_1 <- 0
pU <- 0
  tg11 <-  pL*pU
  tg10 <-  pL*(1-pU)
  tg01 <-  (1-pL)*pU
  tg00 <-  (1-pL)*(1-pU)
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

    sign = as.factor(ifelse(tcde*tsde1<0, 1, 0)),
    dif = tcde - tsde1
  )
max(b2$tpi110)
d3 <- ggplot(data=b2, aes(tsde1, dif)) + 
  geom_point() +
  #  geom_abline(slope=1, intercept=0) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab(expression(paste(SDE[0]^{a[D]== 1}))) +
  ylab(expression(paste(CDE[0] - SDE[0]^{a[D]== 1}))) +
  theme(
    plot.title = element_text(size=20),
    axis.title = element_text(size=21),
    axis.text = element_text(size=20),
    legend.position = "none"
  ) +
  coord_fixed(xlim=c(-0.45, 0.45),ylim=c(-0.45, 0.45))  + 
  ggtitle("(c) D is rare and U is absent")
  #scale_color_manual(values=(c("black", "red")))
#d3



## setting 4 : U not exists and D is non-rare
beta_0 <- -1
theta_0 <- -1
pL <- 0.5
pU <- 0
  tg11 <-  pL*pU
  tg10 <-  pL*(1-pU)
  tg01 <-  (1-pL)*pU
  tg00 <-  (1-pL)*(1-pU)
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

    sign = as.factor(ifelse(tcde*tsde1<0, 1, 0)),
    dif = tcde - tsde1
  )
max(b2$tpi111)
d4 <- ggplot(data=b2, aes(tsde1, dif)) + 
  geom_point() +
#  geom_abline(slope=1, intercept=0) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab(expression(paste(SDE[0]^{a[D]== 1}))) +
  ylab(" ") +
  theme(
    plot.title = element_text(size=20),
    axis.title = element_text(size=21),
    axis.text = element_text(size=20),
    legend.position = "none"
  ) +
  coord_fixed(xlim=c(-0.45, 0.45),ylim=c(-0.45, 0.45))  + 
  ggtitle("(d) D is non-rare and U is absent")
  #scale_color_manual(values=(c("black", "red")))
#d4


all <- (d1+d2)/(d3+d4)
all

current_day <- Sys.time() 
format_day <- format(current_day, "%Y%m%d_%H%M%S")
ggsave(paste("figures/InterpretationalError_sde1", format_day, ".png", sep=""), dpi=300, width = 27, height = 27, units = "cm")

