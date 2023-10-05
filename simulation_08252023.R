
library("dplyr")
library("xtable")
library("rstudioapi")
library("here")

n <- 4e6

sim <-  function(rep, num, theta_0, theta_1, theta_2, theta_3, theta_4, theta_5, theta_6, beta_0, beta_1, beta_2, beta_3, beta_4, beta_5, beta_6, pL, pU, phi_0, phi_1){

  ## functions
  cov <- function(c_0, c_1){
    linear_pred <- (c_0 + c_1*(I(u==1)))
    output <- exp(linear_pred)/(1+exp(linear_pred))
    return(output)
  }
  event <- function(c_0, c_1, c_2, c_3, c_4, c_5, c_6){
    linear_pred <- (c_0 + c_1*(I(a==1)+ I(a==3))  + c_2*l + c_3*(I(a==1)+ I(a==3))*l + c_4*u + c_5*((a==1)+ I(a==3))*u + c_6*l*u)
    output <- exp(linear_pred)/(1+exp(linear_pred))
    return(output)
  }
  competing <- function(c_0, c_1, c_2, c_3, c_4, c_5, c_6){
    linear_pred <- (c_0 + c_1*(I(a==1)+ I(a==2))  + c_2*l + c_3*(I(a==1)+ I(a==2))*l + c_4*u + c_5*((a==1)+ I(a==2))*u + c_6*l*u)
    output <- exp(linear_pred)/(1+exp(linear_pred))
    return(output)
  }  
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
  ## data frame for stacked results
  bias_all <- data.frame()
  est_contrast_all <- data.frame()
  est_abs_all <- data.frame()

  for (i in 1:rep){
    set.seed(i)
    if(phi_0 == 0 & phi_1 == 0){
      u <- rbinom(n = n, size = 1, prob = pU)
      l <- rbinom(n = n, size = 1, prob = pL)
    } else{
      u <- rbinom(n = n, size = 1, prob = pU)
      ul <- data.frame(u) %>% 
        mutate(
          l = rbinom(n = n, size = 1, prob = cov(c_0 = phi_0, c_1 = phi_1))
        )
      l <- ul$l
    }
    
    a <- sample(c(0,1,2,3), size = n, replace=TRUE)
    sim1 <- data.frame(ind = 1:n, a, l, u)
    sim1 <- sim1 %>% 
      mutate(
        prob_d = competing(c_0 = beta_0, c_1 = beta_1, c_2 = beta_2, c_3 = beta_3, c_4 = beta_4, c_5 = beta_5, c_6 = beta_6),
        d = rbinom(n = n, size = 1, prob=prob_d),
        prob_y = event(c_0 = theta_0, c_1 = theta_1, c_2 = theta_2, c_3 = theta_3, c_4 = theta_4, c_5 = theta_5, c_6 = theta_6), 
        y = ifelse(d==0, rbinom(n = n, size = 1, prob=prob_y), 0),
      )
    ## Models are estimated from data
    pi11 <- mean(sim1[which(sim1$a==1 & sim1$l==1),]$d)
    pi10 <- mean(sim1[which(sim1$a==1 & sim1$l==0),]$d)
    pi01 <- mean(sim1[which(sim1$a==0 & sim1$l==1),]$d)
    pi00 <- mean(sim1[which(sim1$a==0 & sim1$l==0),]$d)
    mu111 <- ifelse(pU != 0, mean(sim1[which(sim1$d==0 & sim1$a==1 & sim1$l==1 & sim1$u==1),]$y), 0)
    mu110 <- mean(sim1[which(sim1$d==0 & sim1$a==1 & sim1$l==1 & sim1$u==0),]$y)
    mu101 <- ifelse(pU != 0, mean(sim1[which(sim1$d==0 & sim1$a==1 & sim1$l==0 & sim1$u==1),]$y), 0)
    mu100 <- mean(sim1[which(sim1$d==0 & sim1$a==1 & sim1$l==0 & sim1$u==0),]$y)
    mu011 <- ifelse(pU != 0, mean(sim1[which(sim1$d==0 & sim1$a==0 & sim1$l==1 & sim1$u==1),]$y), 0)
    mu010 <- mean(sim1[which(sim1$d==0 & sim1$a==0 & sim1$l==1 & sim1$u==0),]$y)
    mu001 <- ifelse(pU != 0, mean(sim1[which(sim1$d==0 & sim1$a==0 & sim1$l==0 & sim1$u==1),]$y), 0)
    mu000 <- mean(sim1[which(sim1$d==0 & sim1$a==0 & sim1$l==0 & sim1$u==0),]$y)
    pi111 <- ifelse(pU != 0, mean(sim1[which(sim1$a==1 & sim1$l==1 & sim1$u==1),]$d), 0)
    pi110 <- mean(sim1[which(sim1$a==1 & sim1$l==1 & sim1$u==0),]$d)
    pi101 <- ifelse(pU != 0, mean(sim1[which(sim1$a==1 & sim1$l==0 & sim1$u==1),]$d), 0)
    pi100 <- mean(sim1[which(sim1$a==1 & sim1$l==0 & sim1$u==0),]$d)
    pi011 <- ifelse(pU != 0, mean(sim1[which(sim1$a==0 & sim1$l==1 & sim1$u==1),]$d), 0)
    pi010 <- mean(sim1[which(sim1$a==0 & sim1$l==1 & sim1$u==0),]$d)
    pi001 <- ifelse(pU != 0, mean(sim1[which(sim1$a==0 & sim1$l==0 & sim1$u==1),]$d), 0)
    pi000 <- mean(sim1[which(sim1$a==0 & sim1$l==0 & sim1$u==0),]$d)
    
    ## Estimation
    sim2 <- sim1 %>% 
      mutate(
        w_c = I(a==1 & l==1)/(1-pi11) + I(a==1 & l==0)/(1-pi10) + I(a==0 & l==1)/(1-pi01) + I(a==0 & l==0)/(1-pi00),
        w_s1 = I(a==1 & l==1)*(1-pi11)/(1-pi11) + I(a==1 & l==0)*(1-pi10)/(1-pi10) + I(a==0 & l==1)*(1-pi11)/(1-pi01) + I(a==0 & l==0)*(1-pi10)/(1-pi00),
        # w_s1 : weight for SDE(1) (aD=1 case)
        w_s0 = I(a==1 & l==1)*(1-pi01)/(1-pi11) + I(a==1 & l==0)*(1-pi00)/(1-pi10) + I(a==0 & l==1)*(1-pi01)/(1-pi01) + I(a==0 & l==0)*(1-pi00)/(1-pi00),
        # w_s0 : weight for SDE(0) (aD=0 case)
        cde = ifelse(d==0, w_c*y, 0),
        sde1 = ifelse(d==0, w_s1*y, 0),
        sde0 = ifelse(d==0, w_s0*y, 0)
      )
    est_abs <- data.frame(
      hat_cde1 = mean(sim2[which(sim2$a==1),]$cde),
      hat_cde0 = mean(sim2[which(sim2$a==0),]$cde),
      hat_sde11 = mean(sim2[which(sim2$a==1),]$sde1),
      hat_sde10 = mean(sim2[which(sim2$a==1),]$sde0),
      hat_sde01 = mean(sim2[which(sim2$a==0),]$sde1),
      hat_sde00 = mean(sim2[which(sim2$a==0),]$sde0)
    )
    est_contrast <- est_abs %>% 
      mutate(
        hat_cde = hat_cde1 - hat_cde0,
        hat_sde1 = hat_sde11 - hat_sde01,
        hat_sde0 = hat_sde10 - hat_sde00,
      ) %>% 
      select(hat_cde, hat_sde1, hat_sde0)

    param <- data.frame(
      theta_0 = theta_0,
      theta_1 = theta_1,
      theta_2 = theta_2,
      theta_3 = theta_3,
      theta_4 = theta_4,
      theta_5 = theta_5,
      theta_6 = theta_6,
      beta_0 = beta_0,
      beta_1 = beta_1,
      beta_2 = beta_2,
      beta_3 = beta_3,
      beta_4 = beta_4,
      beta_5 = beta_5,
      beta_6 = beta_6,
      pL = pL,
      pU = pU,
      phi_0 = phi_0, 
      phi_1 = phi_1
    )

    tmu111 <- tevent(c_0 = theta_0, c_1 = theta_1, c_2 = theta_2, c_3 = theta_3, c_4 = theta_4, c_5 = theta_5, c_6 = theta_6, a = 1, l = 1, u = 1)
    tmu110 <- tevent(c_0 = theta_0, c_1 = theta_1, c_2 = theta_2, c_3 = theta_3, c_4 = theta_4, c_5 = theta_5, c_6 = theta_6, a = 1, l = 1, u = 0)
    tmu101 <- tevent(c_0 = theta_0, c_1 = theta_1, c_2 = theta_2, c_3 = theta_3, c_4 = theta_4, c_5 = theta_5, c_6 = theta_6, a = 1, l = 0, u = 1)
    tmu100 <- tevent(c_0 = theta_0, c_1 = theta_1, c_2 = theta_2, c_3 = theta_3, c_4 = theta_4, c_5 = theta_5, c_6 = theta_6, a = 1, l = 0, u = 0)
    tmu011 <- tevent(c_0 = theta_0, c_1 = theta_1, c_2 = theta_2, c_3 = theta_3, c_4 = theta_4, c_5 = theta_5, c_6 = theta_6, a = 0, l = 1, u = 1)
    tmu010 <- tevent(c_0 = theta_0, c_1 = theta_1, c_2 = theta_2, c_3 = theta_3, c_4 = theta_4, c_5 = theta_5, c_6 = theta_6, a = 0, l = 1, u = 0)
    tmu001 <- tevent(c_0 = theta_0, c_1 = theta_1, c_2 = theta_2, c_3 = theta_3, c_4 = theta_4, c_5 = theta_5, c_6 = theta_6, a = 0, l = 0, u = 1)
    tmu000 <- tevent(c_0 = theta_0, c_1 = theta_1, c_2 = theta_2, c_3 = theta_3, c_4 = theta_4, c_5 = theta_5, c_6 = theta_6, a = 0, l = 0, u = 0)
    tpi111 <-  tcompeting(c_0 = beta_0, c_1 = beta_1, c_2 = beta_2, c_3 = beta_3, c_4 = beta_4, c_5 = beta_5, c_6 = beta_6, a = 1, l = 1, u = 1)
    tpi110 <-  tcompeting(c_0 = beta_0, c_1 = beta_1, c_2 = beta_2, c_3 = beta_3, c_4 = beta_4, c_5 = beta_5, c_6 = beta_6, a = 1, l = 1, u = 0)
    tpi101 <-  tcompeting(c_0 = beta_0, c_1 = beta_1, c_2 = beta_2, c_3 = beta_3, c_4 = beta_4, c_5 = beta_5, c_6 = beta_6, a = 1, l = 0, u = 1)
    tpi100 <-  tcompeting(c_0 = beta_0, c_1 = beta_1, c_2 = beta_2, c_3 = beta_3, c_4 = beta_4, c_5 = beta_5, c_6 = beta_6, a = 1, l = 0, u = 0)
    tpi011 <-  tcompeting(c_0 = beta_0, c_1 = beta_1, c_2 = beta_2, c_3 = beta_3, c_4 = beta_4, c_5 = beta_5, c_6 = beta_6, a = 0, l = 1, u = 1)
    tpi010 <-  tcompeting(c_0 = beta_0, c_1 = beta_1, c_2 = beta_2, c_3 = beta_3, c_4 = beta_4, c_5 = beta_5, c_6 = beta_6, a = 0, l = 1, u = 0)
    tpi001 <-  tcompeting(c_0 = beta_0, c_1 = beta_1, c_2 = beta_2, c_3 = beta_3, c_4 = beta_4, c_5 = beta_5, c_6 = beta_6, a = 0, l = 0, u = 1)
    tpi000 <-  tcompeting(c_0 = beta_0, c_1 = beta_1, c_2 = beta_2, c_3 = beta_3, c_4 = beta_4, c_5 = beta_5, c_6 = beta_6, a = 0, l = 0, u = 0)
    tg11 <- tcov(c_0 = phi_0, c_1 = phi_1, u = 1)*pU
    tg10 <- tcov(c_0 = phi_0, c_1 = phi_1, u = 0)*(1-pU)
    tg01 <- (1-tcov(c_0 = phi_0, c_1 = phi_1, u = 1))*pU
    tg00 <- (1-tcov(c_0 = phi_0, c_1 = phi_1, u = 0))*(1-pU)
    
    true_abs <- data.frame(
      tcde1 = tmu111*tg11 + tmu110*tg10 + tmu101*tg01 + tmu100*tg00,
      tcde0 = tmu011*tg11 + tmu010*tg10 + tmu001*tg01 + tmu000*tg00,
      tsde11 = tmu111*(1-tpi111)*tg11 + tmu110*(1-tpi110)*tg10 + tmu101*(1-tpi101)*tg01 + tmu100*(1-tpi100)*tg00,
      tsde01 = tmu011*(1-tpi111)*tg11 + tmu010*(1-tpi110)*tg10 + tmu001*(1-tpi101)*tg01 + tmu000*(1-tpi100)*tg00,
      tsde10 = tmu111*(1-tpi011)*tg11 + tmu110*(1-tpi010)*tg10 + tmu101*(1-tpi001)*tg01 + tmu100*(1-tpi000)*tg00,
      tsde00 = tmu011*(1-tpi011)*tg11 + tmu010*(1-tpi010)*tg10 + tmu001*(1-tpi001)*tg01 + tmu000*(1-tpi000)*tg00 
    )
    true_contrast <- true_abs %>% 
      mutate(
        tcde = tcde1 - tcde0,
        tsde1 = tsde11 - tsde01,
        tsde0 = tsde10 - tsde00
      ) %>% 
      select(tcde, tsde1, tsde0)
    bias <- bind_cols(est_contrast, true_contrast) %>% 
      mutate(
        dif_cdesde1 = hat_cde - hat_sde1,
        dif_cdesde0 = hat_cde - hat_sde0,
        dif_sde1sde0 = hat_sde1 - hat_sde0,
        bias_cdecde = hat_cde - tcde,
        bias_sde1sde1 = hat_sde1 - tsde1,
        bias_sde0sde0 = hat_sde0 - tsde0,
        bias_cdesde1 = hat_cde - tsde1,
        bias_cdesde0 = hat_cde - tsde0
      )
    bias_all <- bind_rows(bias_all, bias)
    est_contrast_all <- bind_rows(est_contrast_all, est_contrast)
  }
  alloutput <- bind_rows(est_abs_all, bias_all)
  write.csv(alloutput, here("results", "tables", paste(num, "_alloutput.csv", sep="")), fileEncoding = "CP932")
  
  res <- data.frame(
    mean_cde = mean(est_contrast_all$hat_cde),
    mean_sde1 = mean(est_contrast_all$hat_sde1),
    mean_sde0 = mean(est_contrast_all$hat_sde0),
    true_cde = true_contrast$tcde,
    true_sde1 = true_contrast$tsde1,
    true_sde0 = true_contrast$tsde0,
    dif_cdesde1 = mean(bias_all$dif_cdesde1),
    dif_cdesde0 = mean(bias_all$dif_cdesde0),
    dif_sde1sde0 = mean(bias_all$dif_sde1sde0),
    bias_cdecde = mean(bias_all$bias_cdecde),
    bias_sde1sde1 = mean(bias_all$bias_sde1sde1),
    bias_sde0sde0 = mean(bias_all$bias_sde0sde0),
    bias_cdesde1 = mean(bias_all$bias_cdesde1),
    bias_cdesde0 = mean(bias_all$bias_cdesde0),
    evar_cde = var(est_contrast_all$hat_cde),
    evar_sde1 = var(est_contrast_all$hat_sde1),
    evar_sde0 = var(est_contrast_all$hat_sde0)
  )
  result <- bind_cols(param, res)
  return(result)
} 



## For scenario 1, set pL nonzero, pU=0, phi_0=0, phi_1=0
## For scenari 2, set pL nonzero, pU nonzero, phi_0=0, phi_1=0
## For appendix scenario, set pL=0, pU nonzero, phi_0 nonzero, phi_1 nonzero

#scenario 1, both rare
system.time({
#scenario 1, nonrare
result1 <- sim(rep=20000, num=1, theta_0=-1, theta_1=-0.5, theta_2=0.5, theta_3=1, theta_4=0, theta_5=0, theta_6=0, 
               beta_0=-1, beta_1=0.5, beta_2=0.5, beta_3=1, beta_4=0, beta_5=0, beta_6=0, pL=0.5, pU=0, phi_0=0, phi_1=0)
result2 <- sim(rep=20000, num=2, theta_0=-1, theta_1=0, theta_2=0, theta_3=1, theta_4=0, theta_5=0, theta_6=0, 
               beta_0=-1, beta_1=0.5, beta_2=0.5, beta_3=1, beta_4=0, beta_5=0, beta_6=0, pL=0.5, pU=0, phi_0=0, phi_1=0)
result3 <- sim(rep=20000, num=3, theta_0=-1, theta_1=0.5, theta_2=0.5, theta_3=1, theta_4=0, theta_5=0, theta_6=0, 
               beta_0=-1, beta_1=0.5, beta_2=0.5, beta_3=1, beta_4=0, beta_5=0, beta_6=0, pL=0.5, pU=0, phi_0=0, phi_1=0)

result4 <- sim(rep=20000, num=4, theta_0=-1, theta_1=-0.5, theta_2=0.5, theta_3=1, theta_4=0, theta_5=0, theta_6=0, 
               beta_0=-1, beta_1=-0.5, beta_2=-0.5, beta_3=1, beta_4=0, beta_5=0, beta_6=0, pL=0.5, pU=0, phi_0=0, phi_1=0)
result5 <- sim(rep=20000, num=5, theta_0=-1, theta_1=-0.5, theta_2=0.5, theta_3=1, theta_4=0, theta_5=0, theta_6=0, 
                beta_0=-1, beta_1=0, beta_2=0, beta_3=1, beta_4=0, beta_5=0, beta_6=0, pL=0.5, pU=0, phi_0=0, phi_1=0)

#scenario 2 nonrare
result6 <- sim(rep=20000, num=6, theta_0=-1, theta_1=-0.5, theta_2=0.5, theta_3=0.5, theta_4=0.5, theta_5=0.5, theta_6=0.5,
                 beta_0=-1, beta_1=0.5, beta_2=0.5, beta_3=0.5, beta_4=0.5, beta_5=0.5, beta_6=0.5, pL=0.5, pU=0.3, phi_0=0, phi_1=0)
result7 <- sim(rep=20000, num=7, theta_0=-1, theta_1=-0.5, theta_2=0.5, theta_3=0.5, theta_4=0.5, theta_5=0.5, theta_6=0.5,
                 beta_0=-1, beta_1=0.5, beta_2=0.5, beta_3=0.5, beta_4=-0.5, beta_5=-0.5, beta_6=-0.5, pL=0.5, pU=0.3, phi_0=0, phi_1=0)
result8 <- sim(rep=20000, num=8, theta_0=-1, theta_1=-0.5, theta_2=0.5, theta_3=0.5, theta_4=-0.5, theta_5=-0.5, theta_6=-0.5,
                 beta_0=-1, beta_1=0.5, beta_2=0.5, beta_3=0.5, beta_4=0.5, beta_5=0.5, beta_6=0.5, pL=0.5, pU=0.3, phi_0=0, phi_1=0)

})

result_all <- bind_rows(result1, result2, result3, result4, result5, result6, result7, result8)

des3 <- c("Scenario 1", "", "", "", "", "", "Scenario 2", "")
table <- bind_cols(des3, result_all) %>% 
  select(theta_1, theta_2, theta_3, theta_4, theta_5, theta_6, 
         beta_1, beta_2, beta_3, beta_4, beta_5, beta_6, mean_cde, mean_sde1, bias_cdecde, bias_sde1sde1, bias_cdesde1) %>% 
  mutate(mean_cde=round(mean_cde, digits=4),
         mean_sde1=round(mean_sde1, digits=4),
         bias_cdecde=signif(bias_cdecde, digits=2),
         bias_sde1sde1=signif(bias_sde1sde1, digits=2),
         bias_cdesde1=signif(bias_cdesde1, digits=2)
    )
write.csv(table, here("results", "tables", "tables.csv"), fileEncoding = "CP932")
write.csv(result_all, here("results", "tables", "result_all.csv"), fileEncoding = "CP932")
sink(file=here("results", "tables", "xtable.txt")) 
print(xtable(digits=c(0,1,1,1,1,1,1,1,1,1,1,1,1,3,3,3,3,3),table, type="latex")) 
sink(file=NULL)