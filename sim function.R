
sim <-  function(rep, n, num, theta_0, theta_1, theta_2, theta_3, theta_4, theta_5, theta_6, 
                 beta_0, beta_1, beta_2, beta_3, beta_4, beta_5, beta_6, pL, pU){
  
  ## functions
  event <- function(c_0, c_1, c_2, c_3, c_4, c_5, c_6){
    linear_pred <- (c_0 + c_1*a  + c_2*l + c_3*a*l + c_4*u + c_5*a*u + c_6*l*u)
    output <- exp(linear_pred)/(1+exp(linear_pred))
    return(output)
  }
  competing <- function(c_0, c_1, c_2, c_3, c_4, c_5, c_6){
    linear_pred <- (c_0 + c_1*a  + c_2*l + c_3*a*l + c_4*u + c_5*a*u + c_6*l*u)
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
  
  ## dataframe for stacked results
  output_all <- data.frame()
  
  for (i in 1:rep){
    set.seed(i+rep*num)
      u <- rbinom(n = n, size = 1, prob = pU)
      l <- rbinom(n = n, size = 1, prob = pL)

    a <- sample(c(0,1), size = n, replace=TRUE)
    sim1 <- data.frame(ind = 1:n, a, l, u)
    sim1 <- sim1 %>% 
      mutate(
        prob_d = competing(c_0 = beta_0, c_1 = beta_1, c_2 = beta_2, c_3 = beta_3, c_4 = beta_4, c_5 = beta_5, c_6 = beta_6),
        d = rbinom(n = n, size = 1, prob=prob_d),
        prob_y = event(c_0 = theta_0, c_1 = theta_1, c_2 = theta_2, c_3 = theta_3, c_4 = theta_4, c_5 = theta_5, c_6 = theta_6), 
        y = ifelse(d==0, rbinom(n = n, size = 1, prob=prob_y), 0),
      )
    ## Saturated model for the competing event
    pi11 <- mean(sim1[which(sim1$a==1 & sim1$l==1),]$d)
    pi10 <- mean(sim1[which(sim1$a==1 & sim1$l==0),]$d)
    pi01 <- mean(sim1[which(sim1$a==0 & sim1$l==1),]$d)
    pi00 <- mean(sim1[which(sim1$a==0 & sim1$l==0),]$d)
    
    ## Estimation
    sim2 <- sim1 %>% 
      mutate(
        w_c =  if_else(a==1 & l==1,1/(1-pi11),
                       if_else(a==1 & l==0,1/(1-pi10),
                               if_else(a==0 & l==1,1/(1-pi01),
                                       if_else(a==0 & l==0,1/(1-pi00),NA)))),
        w_s1 =  if_else(a==1 & l==1,(1-pi11)/(1-pi11),
                        if_else(a==1 & l==0,(1-pi10)/(1-pi10),
                                if_else(a==0 & l==1,(1-pi11)/(1-pi01),
                                        if_else(a==0 & l==0,(1-pi10)/(1-pi00),NA)))),
        w_s0 =  if_else(a==1 & l==1,(1-pi01)/(1-pi11),
                        if_else(a==1 & l==0,(1-pi00)/(1-pi10),
                                if_else(a==0 & l==1,(1-pi01)/(1-pi01),
                                        if_else(a==0 & l==0,(1-pi00)/(1-pi00),NA)))),   
        cde = ifelse(d==0, w_c*y, 0),
        sde1 = ifelse(d==0, w_s1*y, 0),
        sde0 = ifelse(d==0, w_s0*y, 0)
      )
    # Estimated weights
    est_weights <- data.frame(
      wcde_11 = 1/(1-pi11),
      wcde_10 = 1/(1-pi10),
      wcde_01 = 1/(1-pi01),
      wcde_00 = 1/(1-pi00),
      wsde1_11 = (1-pi11)/(1-pi11),
      wsde1_10 = (1-pi10)/(1-pi10),
      wsde1_01 = (1-pi11)/(1-pi01),
      wsde1_00 = (1-pi10)/(1-pi00),
      wsde0_11 = (1-pi01)/(1-pi11),
      wsde0_10 = (1-pi00)/(1-pi10),
      wsde0_01 = (1-pi01)/(1-pi01),
      wsde0_00 = (1-pi00)/(1-pi00)
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
  
      
    ## True values
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
      pU = pU
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
      tg11 <- pL*pU
      tg10 <- pL*(1-pU)
      tg01 <- (1-pL)*pU
      tg00 <- (1-pL)*(1-pU)

    obsmu11 <- ((tmu111)*(1-tpi111)*pL*pU + (tmu110)*(1-tpi110)*pL*(1-pU)) / ((1-tpi111)*pL*pU + (1-tpi110)*pL*(1-pU))
    obsmu10 <- ((tmu101)*(1-tpi101)*(1-pL)*pU + (tmu100)*(1-tpi100)*(1-pL)*(1-pU)) / ((1-tpi101)*(1-pL)*pU + (1-tpi100)*(1-pL)*(1-pU))
    obsmu01 <- ((tmu011)*(1-tpi011)*pL*pU + (tmu010)*(1-tpi010)*pL*(1-pU)) / ((1-tpi011)*pL*pU + (1-tpi010)*pL*(1-pU))
    obsmu00 <- ((tmu001)*(1-tpi001)*(1-pL)*pU + (tmu000)*(1-tpi000)*(1-pL)*(1-pU)) / ((1-tpi001)*(1-pL)*pU + (1-tpi000)*(1-pL)*(1-pU))
    obspi11 <- (tpi111*pL*pU + tpi110*pL*(1-pU)) / (pL*pU + pL*(1-pU))
    obspi10 <- (tpi101*(1-pL)*pU + tpi100*(1-pL)*(1-pU)) / ((1-pL)*pU + (1-pL)*(1-pU))
    obspi01 <- (tpi011*pL*pU + tpi010*pL*(1-pU)) / (pL*pU + pL*(1-pU))
    obspi00 <- (tpi001*(1-pL)*pU + tpi000*(1-pL)*(1-pU)) / ((1-pL)*pU + (1-pL)*(1-pU))
    tilde_cde1 <- obsmu11 * pL + obsmu10 * (1-pL) 
    tilde_cde0 <- obsmu01 * pL + obsmu00 * (1-pL)
    tilde_sde11 <- obsmu11 * (1-obspi11) * pL + obsmu10 * (1-obspi10) *(1-pL)
    tilde_sde10 <- obsmu01 * (1-obspi11) * pL + obsmu00 * (1-obspi10) *(1-pL)
    tilde_sde01 <- obsmu11 * (1-obspi01) * pL + obsmu10 * (1-obspi00) *(1-pL)
    tilde_sde00 <- obsmu01 * (1-obspi01) * pL + obsmu00 * (1-obspi00) *(1-pL)
    
    true_abs <- data.frame(
      tcde1 = tmu111*tg11 + tmu110*tg10 + tmu101*tg01 + tmu100*tg00,
      tcde0 = tmu011*tg11 + tmu010*tg10 + tmu001*tg01 + tmu000*tg00,
      tsde11 = tmu111*(1-tpi111)*tg11 + tmu110*(1-tpi110)*tg10 + tmu101*(1-tpi101)*tg01 + tmu100*(1-tpi100)*tg00,
      tsde01 = tmu011*(1-tpi111)*tg11 + tmu010*(1-tpi110)*tg10 + tmu001*(1-tpi101)*tg01 + tmu000*(1-tpi100)*tg00,
      tsde10 = tmu111*(1-tpi011)*tg11 + tmu110*(1-tpi010)*tg10 + tmu101*(1-tpi001)*tg01 + tmu100*(1-tpi000)*tg00,
      tsde00 = tmu011*(1-tpi011)*tg11 + tmu010*(1-tpi010)*tg10 + tmu001*(1-tpi001)*tg01 + tmu000*(1-tpi000)*tg00,
      obs_cde = tilde_cde1 - tilde_cde0,
      obs_sde1 = tilde_sde11 - tilde_sde10,
      obs_sde0 = tilde_sde01 - tilde_sde00,
      obs_cde1 = tilde_cde1,
      obs_cde0 = tilde_cde0,
      obs_sde11 = tilde_sde11,
      obs_sde10 = tilde_sde10,
      obs_sde01 = tilde_sde01,
      obs_sde00 = tilde_sde00
    )
    true_contrast <- true_abs %>% 
      mutate(
        tcde = tcde1 - tcde0,
        tsde1 = tsde11 - tsde01,
        tsde0 = tsde10 - tsde00
      ) %>% 
      select(tcde, tsde1, tsde0)

    output <- bind_cols(true_contrast, true_abs, est_contrast, est_abs, est_weights)
    output_all <- bind_rows(output_all, output)
  }
  
  
  result <- bind_cols(param, output_all)
  return(result)
} 

