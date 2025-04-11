
library(dplyr)
library(xtable)


scenario1 <- read.csv("result_all_scenario1_20250411_110444.csv") 
scenario2 <- read.csv("result_all_scenario2_20250407_074342.csv")
scenario3 <- read.csv("result_all_scenario3_20250407_074342.csv")
scenario4 <- read.csv("result_all_scenario4_20250407_074342.csv")
scenario5 <- read.csv("result_all_scenario5_20250407_074342.csv")
scenario6 <- read.csv("result_all_scenario6_20250407_080833.csv")


biascalc <- function(ds){
  res <- data.frame(
    theta_0 = ds[1, "theta_0"], 
    theta_1 = ds[1, "theta_1"],
    theta_2 = ds[1, "theta_2"],
    theta_3 = ds[1, "theta_3"],
    theta_4 = ds[1, "theta_4"],
    theta_5 = ds[1, "theta_5"],
    theta_6 = ds[1, "theta_6"],
    beta_0 = ds[1, "beta_0"],
    beta_1 = ds[1, "beta_1"],
    beta_2 = ds[1, "beta_2"],
    beta_3 = ds[1, "beta_3"],
    beta_4 = ds[1, "beta_4"],
    beta_5 = ds[1, "beta_5"],
    beta_6 = ds[1, "beta_6"],
    pL = ds[1, "pL"],
    pU = ds[1, "pU"],
    phi_0 = ds[1, "phi_0"],
    phi_1 = ds[1, "phi_1"],
    
    var_cde = var(ds$hat_cde)*19999/20000,
    rvar_cde = sqrt(var(ds$hat_cde)*19999/20000),
    mse_cde_sde0 = mean((ds$hat_cde - ds$tsde0)**2),
    rmse_cde_sde0 = sqrt(mean((ds$hat_cde - ds$tsde0)**2)),
    mse_cde_sde1 = mean((ds$hat_cde - ds$tsde1)**2),
    rmse_cde_sde1 = sqrt(mean((ds$hat_cde - ds$tsde1)**2)),
    
    var_sde0 = var(ds$hat_sde0)*19999/20000,
    rvar_sde0 = sqrt(var(ds$hat_sde0)*19999/20000),
    mse_sde0_sde0 = mean((ds$hat_sde0 - ds$tsde0)**2),
    rmse_sde0_sde0 = sqrt(mean((ds$hat_sde0 - ds$tsde0)**2)),
    
    var_sde1 = var(ds$hat_sde1)*19999/20000,
    rvar_sde1 = sqrt(var(ds$hat_sde1)*19999/20000),
    mse_sde1_sde1 = mean((ds$hat_sde1 - ds$tsde1)**2),
    rmse_sde1_sde1 = sqrt(mean((ds$hat_sde1 - ds$tsde1)**2)),
    
    mwcde_11 = max(ds$wcde_11),
    mwcde_10 = max(ds$wcde_10),
    mwcde_01 = max(ds$wcde_01),
    mwcde_00 = max(ds$wcde_00),
    mwsde0_11 = max(ds$wsde0_11),
    mwsde0_10 = max(ds$wsde0_10),
    mwsde1_01 = max(ds$wsde1_01),
    mwsde1_00 = max(ds$wsde1_00)
    
  )
  res <- res %>% mutate(
    maxweight_cde = max(mwcde_11, mwcde_10, mwcde_01, mwcde_00),
    maxweight_sde0 = max(mwsde0_11, mwsde0_10),
    maxweight_sde1 = max(mwsde1_01, mwsde1_00)
  )
  return(res)
}

# col1 : near positivity violation
# col2 : unmeasured common cause present
# col3 : competing events rare
bias1 <- biascalc(scenario1) %>% mutate(col1="No", col2="No", col3="Yes")
bias2 <- biascalc(scenario2) %>% mutate(col1="Yes", col2="No", col3="Yes")
bias3 <- biascalc(scenario3) %>% mutate(col1="No", col2="No", col3="No")
bias4 <- biascalc(scenario4) %>% mutate(col1="Yes", col2="No", col3="No")
bias5 <- biascalc(scenario5) %>% mutate(col1="No", col2="Yes", col3="No")
bias6 <- biascalc(scenario6) %>% mutate(col1="Yes", col2="Yes", col3="No")


table1_sde0  <- bind_rows(bias1, bias3, bias5, bias2, bias4, bias6) %>% 
  select(col1, col2, col3, rvar_sde0 , rvar_cde) 
print(xtable(digits=c(0,0,0,0,3,3), table1_sde0, type="latex"), include.rownames=FALSE)

table2_sde0  <- bind_rows(bias1, bias3, bias5, bias2, bias4, bias6) %>% 
  select(col1, col2, col3, rmse_sde0_sde0 , rmse_cde_sde0) 
print(xtable(digits=c(0,0,0,0,3,3), table2_sde0, type="latex"), include.rownames=FALSE)

table3_sde1 <- bind_rows(bias1, bias3, bias5, bias2, bias4, bias6) %>%
  select(col1, col2, col3, rvar_sde1, rmse_sde1_sde1, rmse_cde_sde1)
print(xtable(digits=c(0,0,0,0,3,3,3), table3_sde1, type="latex"), include.rownames=FALSE)

table_params <- bind_rows(bias1, bias3, bias5, bias2, bias4, bias6) %>%
  select(col1, col2, col3, pL, pU, theta_0, theta_1, theta_2, theta_3, theta_4, theta_5, theta_6, beta_0, beta_1, beta_2, beta_3, beta_4, beta_5, beta_6)
print(xtable(digits=c(0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0), table_params, type="latex"), include.rownames=FALSE)

