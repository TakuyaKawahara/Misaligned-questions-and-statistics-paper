# Define a data.frame of simulation parameters ----------------------------

param <- data.frame(theta_0=-1, theta_1=-2, theta_2=1, theta_3=3, theta_4=0, theta_5=0, theta_6=0,
                    beta_0=-10, beta_1=1, beta_2=16, beta_3=-1, beta_4=0, beta_5=0, beta_6=0, pL=0.1, pU=0, phi_0=0, phi_1=0)
filename <- 2
slots <- 4
setting <- data.frame(num = rep(1:slots),
                      rep = 5000,
                      n = 100000
) 
parametersDF <- merge(setting,param)


current_day <- Sys.time() 
format_day <- format(current_day, "%Y%m%d_%H%M%S")
zz <- file(paste("log_scenario", filename,"_", format_day, ".txt", sep=""), open="wt")
sink(file=zz, split =T) 

library(tictoc)
tic("sleeping")
print("start computing...")

library("dplyr")
library("here")
library("doSNOW")
library("parallel")

# Load a sim function ----------------------------
source( "sim function.R" )


cluster <- makeCluster(slots, type = "SOCK")
registerDoSNOW(cluster)

result_all <- foreach(scenario = 1:slots, 
                      .combine = 'rbind'
                      , .packages=c('dplyr', 'here')
) %dopar% {
  
  rep <- parametersDF[scenario, "rep"]
  n <- parametersDF[scenario, "n"]
  num <- parametersDF[scenario, "num"]
  theta_0 <- parametersDF[scenario, "theta_0"]
  theta_1 <- parametersDF[scenario, "theta_1"]
  theta_2 <- parametersDF[scenario, "theta_2"]
  theta_3 <- parametersDF[scenario, "theta_3"]
  theta_4 <- parametersDF[scenario, "theta_4"]
  theta_5 <- parametersDF[scenario, "theta_5"]
  theta_6 <- parametersDF[scenario, "theta_6"]
  beta_0 <- parametersDF[scenario, "beta_0"]
  beta_1 <- parametersDF[scenario, "beta_1"]
  beta_2 <- parametersDF[scenario, "beta_2"]
  beta_3 <- parametersDF[scenario, "beta_3"]
  beta_4 <- parametersDF[scenario, "beta_4"]
  beta_5 <- parametersDF[scenario, "beta_5"]
  beta_6 <- parametersDF[scenario, "beta_6"]
  pL <- parametersDF[scenario, "pL"]
  pU <- parametersDF[scenario, "pU"]
  phi_0 <- parametersDF[scenario, "phi_0"]
  phi_1 <- parametersDF[scenario, "phi_1"]
  sim(rep = rep,
      n = n,
      num = num,
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
      phi_1 = phi_1)
}
stopCluster(cluster)



result_all %>% select(wcde_11, wcde_10, wcde_01, wcde_00, wsde0_11, wsde0_10) %>% summary()
result_all %>% select(bias_cdecde, bias_cdesde0, bias1_sde0, bias2, bias3, bias3_cde_1,bias3_cde_0, bias2_sde0,bias3_sde0
                      , bias3_cde_1l1, bias3_cde_1l0, bias3_cde_0l1, bias3_cde_0l0 ) %>% summary()
result_all %>% select(bias3, bias3_sde0, bias3_cde_1l1, bias3_cde_1l0, bias3_cde_0l1, bias3_cde_0l0) %>% var()


print("...end computing")
toc()
sink()

# Save result_all ----------------------------
write.csv(result_all, file=paste("result_all_scenario", filename, "_", format_day, ".csv", sep=""), fileEncoding = "CP932")

