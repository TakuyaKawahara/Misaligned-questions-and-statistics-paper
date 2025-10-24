
library(dplyr)
library(ggplot2)
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


## setting 1 : Both Y and D may depend on U, SDE(ad=0)
  beta_0 <- -1
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
    tsde11 = tmu111*(1-tpi111)*tg11 + tmu110*(1-tpi110)*tg10 + tmu101*(1-tpi101)*tg01 + tmu100*(1-tpi100)*tg00,
    tsde01 = tmu011*(1-tpi111)*tg11 + tmu010*(1-tpi110)*tg10 + tmu001*(1-tpi101)*tg01 + tmu000*(1-tpi100)*tg00,
    tsde10 = tmu111*(1-tpi011)*tg11 + tmu110*(1-tpi010)*tg10 + tmu101*(1-tpi001)*tg01 + tmu100*(1-tpi000)*tg00,
    tsde00 = tmu011*(1-tpi011)*tg11 + tmu010*(1-tpi010)*tg10 + tmu001*(1-tpi001)*tg01 + tmu000*(1-tpi000)*tg00,
    
    tcde = tcde1 - tcde0,
    tsde1 = tsde11 - tsde01,
    tsde0 = tsde10 - tsde00,
    
    sign = as.factor(ifelse(tcde*tsde0<0, 1, 0)),
    ratio = tsde0/tcde,
    dif = tsde0 - tcde,
  )
d1 <- ggplot(data=b2, aes(tcde, tsde0, colour=sign)) + 
  geom_point() +
  geom_abline(slope=1, intercept=0) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab(" ") +
  ylab(expression(paste("Separable direct effect", " (", {a[D]},"=0)", sep=""))) +
  theme(
    plot.title = element_text(size=20),
    axis.title = element_text(size=21),
    axis.text = element_text(size=20),
    legend.position = "none"
  ) +
  coord_fixed(xlim=c(-0.45, 0.45),ylim=c(-0.45, 0.45))  + 
  ggtitle("Both Y and D may depend on U")+
  scale_color_manual(values=(c("black", "black")))



## setting 2 : Neither Y nor D depend on U, SDE(ad=0)
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
    tsde11 = tmu111*(1-tpi111)*tg11 + tmu110*(1-tpi110)*tg10 + tmu101*(1-tpi101)*tg01 + tmu100*(1-tpi100)*tg00,
    tsde01 = tmu011*(1-tpi111)*tg11 + tmu010*(1-tpi110)*tg10 + tmu001*(1-tpi101)*tg01 + tmu000*(1-tpi100)*tg00,
    tsde10 = tmu111*(1-tpi011)*tg11 + tmu110*(1-tpi010)*tg10 + tmu101*(1-tpi001)*tg01 + tmu100*(1-tpi000)*tg00,
    tsde00 = tmu011*(1-tpi011)*tg11 + tmu010*(1-tpi010)*tg10 + tmu001*(1-tpi001)*tg01 + tmu000*(1-tpi000)*tg00,
    
    tcde = tcde1 - tcde0,
    tsde1 = tsde11 - tsde01,
    tsde0 = tsde10 - tsde00,
    
    sign = as.factor(ifelse(tcde*tsde0<0, 1, 0)),
    ratio = tsde0/tcde,
    dif = tsde0 - tcde,
  )
d2 <- ggplot(data=b2, aes(tcde, tsde0, colour=sign)) + 
  geom_point() +
  geom_abline(slope=1, intercept=0) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("") +
  ylab("") +
  theme(
    plot.title = element_text(size=20),
    axis.title = element_text(size=21),
    axis.text = element_text(size=20),
    legend.position = "none"
  ) +
  coord_fixed(xlim=c(-0.45, 0.45),ylim=c(-0.45, 0.45))  + 
  ggtitle("Neither Y nor D depend on U")+
  scale_color_manual(values=(c("black", "black")))


## setting 3 : Both Y and D may depend on U, SDE(ad=1)
  beta_0 <- -1
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
    tsde11 = tmu111*(1-tpi111)*tg11 + tmu110*(1-tpi110)*tg10 + tmu101*(1-tpi101)*tg01 + tmu100*(1-tpi100)*tg00,
    tsde01 = tmu011*(1-tpi111)*tg11 + tmu010*(1-tpi110)*tg10 + tmu001*(1-tpi101)*tg01 + tmu000*(1-tpi100)*tg00,
    tsde10 = tmu111*(1-tpi011)*tg11 + tmu110*(1-tpi010)*tg10 + tmu101*(1-tpi001)*tg01 + tmu100*(1-tpi000)*tg00,
    tsde00 = tmu011*(1-tpi011)*tg11 + tmu010*(1-tpi010)*tg10 + tmu001*(1-tpi001)*tg01 + tmu000*(1-tpi000)*tg00,
    
    tcde = tcde1 - tcde0,
    tsde1 = tsde11 - tsde01,
    tsde0 = tsde10 - tsde00,
    
    sign = as.factor(ifelse(tcde*tsde0<0, 1, 0)),
    ratio = tsde0/tcde,
    dif = tsde0 - tcde,
  )
d3 <- ggplot(data=b2, aes(tcde, tsde1, colour=sign)) + 
  geom_point() +
  geom_abline(slope=1, intercept=0) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Controlled direct effect") +
  ylab(expression(paste("Separable direct effect", " (", {a[D]},"=1)", sep=""))) +
  theme(
    plot.title = element_text(size=20),
    axis.title = element_text(size=21),
    axis.text = element_text(size=20),
    legend.position = "none"
  ) +
  coord_fixed(xlim=c(-0.45, 0.45),ylim=c(-0.45, 0.45))  + 
  ggtitle("")+
  scale_color_manual(values=(c("black", "black")))


## setting 4 : Neither Y nor D depend on U, SDE(ad=1)
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
    tsde11 = tmu111*(1-tpi111)*tg11 + tmu110*(1-tpi110)*tg10 + tmu101*(1-tpi101)*tg01 + tmu100*(1-tpi100)*tg00,
    tsde01 = tmu011*(1-tpi111)*tg11 + tmu010*(1-tpi110)*tg10 + tmu001*(1-tpi101)*tg01 + tmu000*(1-tpi100)*tg00,
    tsde10 = tmu111*(1-tpi011)*tg11 + tmu110*(1-tpi010)*tg10 + tmu101*(1-tpi001)*tg01 + tmu100*(1-tpi000)*tg00,
    tsde00 = tmu011*(1-tpi011)*tg11 + tmu010*(1-tpi010)*tg10 + tmu001*(1-tpi001)*tg01 + tmu000*(1-tpi000)*tg00,
    
    tcde = tcde1 - tcde0,
    tsde1 = tsde11 - tsde01,
    tsde0 = tsde10 - tsde00,
    
    sign = as.factor(ifelse(tcde*tsde0<0, 1, 0)),
    ratio = tsde0/tcde,
    dif = tsde0 - tcde,
  )
d4 <- ggplot(data=b2, aes(tcde, tsde1, colour=sign)) + 
  geom_point() +
  geom_abline(slope=1, intercept=0) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Controlled direct effect") +
  ylab(" ") +
  theme(
    plot.title = element_text(size=20),
    axis.title = element_text(size=21),
    axis.text = element_text(size=20),
    legend.position = "none"
  ) +
  coord_fixed(xlim=c(-0.45, 0.45),ylim=c(-0.45, 0.45))  + 
  ggtitle("")+
  scale_color_manual(values=(c("black", "black")))

all <- (d1+d2)/(d3+d4)

current_day <- Sys.time() 
format_day <- format(current_day, "%Y%m%d_%H%M%S")
ggsave(paste("figures/EstimandError_non_rare_", format_day, ".png", sep=""), dpi=300, width = 27, height = 27, units = "cm")

