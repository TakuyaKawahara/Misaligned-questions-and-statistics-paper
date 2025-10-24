# Define a data.frame of simulation parameters ----------------------------

param <- data.frame(theta_0=-1, theta_1=-2, theta_2=1, theta_3=3, theta_4=0, theta_5=0, theta_6=0,
                    beta_0=-1, beta_1=1, beta_2=7, beta_3=-1, beta_4=0, beta_5=0, beta_6=0, pL=0.1, pU=0.5)
filename <- 4
slots <- 4
setting <- data.frame(num = rep(1:slots),
                      rep = 5000,
                      n = 100000
) 
parametersDF <- merge(setting,param)


current_day <- Sys.time() 
format_day <- format(current_day, "%Y%m%d_%H%M%S")

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
      pU = pU)
}
stopCluster(cluster)



# Save result_all ----------------------------
write.csv(result_all, file=paste("result_all_scenario", filename, "_", format_day, ".csv", sep=""), fileEncoding = "CP932")

