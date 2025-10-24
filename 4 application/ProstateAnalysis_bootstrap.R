
library(tidyverse)
library(gtsummary)
library(Hmisc)
library(splines)
library(here)
library(survival)
library(latex2exp)


data0 <- read.csv(here::here("data", "prostate.csv"))  %>% 
  mutate(
    rx = as_factor(rx)
  ) %>% 
  filter(rx %in% c("placebo", "5.0 mg estrogen"))

Estimates_ProstateDataset <- function(bootdata){
data <- bootdata %>%
  mutate(
    allCause = if_else(status != "alive", 1, 0)
  ) %>%
  mutate(
    eventType = case_when(
      status == "alive" ~ "alive",
      status == "dead - prostatic ca" ~ "pdeath",
      TRUE ~ "odeath"
    ) ,
    # set factor levels
    eventType = factor(eventType, levels = c("alive", "pdeath", "odeath"))
  ) %>% 
  mutate(
    rx = abs(3 - as.integer(rx) ),          # DES: A=1 and placebo: A=0
    eventType = as.integer(eventType) - 1,  #0: censoring, 1: pdeath, 2: odeath
    hgBinary = if_else(hg < 12, 1, 0),
    ageCat = Hmisc::cut2(age, c(0, 60, 75, 100)), 
    normalAct = if_else(pf == "normal activity", 1, 0),
    eventCens = if_else(eventType == 0, 1, 0),
    eventProst = if_else(eventType == 1, 1, 0),
    Tstart = -0.01 # for long format transformation later
  ) 
cutTimes <- c(0:49) 


#### Create long format dataset ####
data_prost_cens_long <- data %>% 
  survSplit(
    cut = cutTimes,
    start = "Tstart",
    end = "dtime",
    event = "eventCens")

data_prost_long <- data %>% 
  survSplit(
    cut = cutTimes,
    start = "Tstart",
    end = "dtime",
    event = "allCause") %>% 
  mutate(
    eventCens = data_prost_cens_long$eventCens,
    # prostate cancer mortality indicator
    prostateDeath = if_else(allCause == 1 & eventType == 1, 1, 0),
    # other causes of death indicator
    otherDeath = if_else(allCause == 1 & eventType == 2, 1, 0)
  ) %>% 
  mutate( 
    # unless censored or died from other than prostate cancer causes - the outcome is defined, otherwise - undefined
    prostateDeath = if_else(eventCens == 1, NA_real_, prostateDeath),
    otherDeath = if_else(eventCens == 1, NA_real_, otherDeath),
    prostateDeath = if_else(otherDeath == 1, NA_real_, prostateDeath)
  ) %>% 
  filter(dtime < length(cutTimes))



#### Create T0 data: data at time point 0 ####
T0 <- data_prost_long %>% filter(dtime == 0)

# number of individuals at baseline
n <- n_distinct(data_prost_long$patno)



## --- Step 1. Fit pooled logistic model for the competing event hazard
# Model for mortality from other causes
odeath_fit <-
#  glm(otherDeath ~ dtime + I(dtime ^ 2) + normalAct + ageCat + hx + hgBinary + rx + rx*(normalAct + ageCat + hx + hgBinary),
  glm(otherDeath ~ dtime + I(dtime ^ 2) + normalAct + ageCat + hx + hgBinary + rx + rx*(hx),
      data = data_prost_long,
      family = binomial(link = "logit")
  ) ## Competing event model, which was used in Young 2020 Stat Med
odeath_fit


## For separable direct effect, we use rx_c which takes the same value as the original treatment arm rx,
## rx_c is used to calculate weights in the following steps
# create variable for A_d
data_prost_long <- data_prost_long %>% 
  mutate(
    rx_c = rx
  )
odeath_fit_c <-
#  glm(otherDeath ~ dtime + I(dtime ^ 2) + normalAct + ageCat + hx + hgBinary + rx_c + rx_c*(normalAct + ageCat + hx + hgBinary),
  glm(otherDeath ~ dtime + I(dtime ^ 2) + normalAct + ageCat + hx + hgBinary + rx_c + rx_c*(hx),
      data = data_prost_long,
      family = binomial(link = "logit")
  )

#### Step 2-CDE. Calculation of the weights for controlled direct effect
## Compute IPCWs for other than prostate cancer death as competing event ####
data_prost_long <- data_prost_long %>% 
  mutate(
    pred_odeath = 1 - predict(odeath_fit, newdata = ., type = 'response'),
  ) %>% 
  group_by(patno) %>% 
  #cumulative probability of other death per person over all months
  mutate(
    cum_pred_odeath = cumprod(pred_odeath),
  ) %>% 
  ungroup() %>% 
  mutate(
    ipcw_odeath_unstab = (1 - otherDeath)/cum_pred_odeath,
  )
summary(data_prost_long$ipcw_odeath_unstab)

#### Step 3-CDE. Estimation of the controlled direct effect with utility function

### utility functions for IPW estimators (from Young 2020 StatMed, pgform_ipcws)
nonParametricCumHaz <- function(weightVector, inputdata, grp, outcomeProstate=TRUE){
  outputHazards <- rep(NA, length.out=length(cutTimes))
  counter <- 1 
  for(i in cutTimes){
    if(outcomeProstate){
      indices <- inputdata$dtime==i & inputdata$rx == grp & inputdata$eventCens==0 & inputdata$otherDeath==0 
      eventIndicator <- indices & inputdata$prostateDeath==1 
    }else{
      indices <- inputdata$dtime==i & inputdata$rx == grp & inputdata$eventCens==0
      eventIndicator <- indices & inputdata$otherDeath==1 
    }
    outputHazards[counter] <- sum(weightVector[eventIndicator]) / sum(weightVector[indices])
    counter <- counter+1
  }
  return(outputHazards)
}
nonParametricCumInc <- function(hazard1,hazard2,competing=FALSE){
  inc <- rep(NA, length.out=length(cutTimes))
  cumulativeSurvival <- c(1, cumprod( (1-hazard1) * (1-hazard2) ))
  counter <- 1 
  for(i in 1:length(cutTimes)){
    if(!competing){
      inc[i] <- hazard1[i] * (1-hazard2[i]) * cumulativeSurvival[i]
    }else{
      inc[i] <- hazard1[i] * cumulativeSurvival[i]
    }
  }
  cumInc <- cumsum(inc)
  return(cumInc)
}

#### Cumulative hazard and cumulative incidence for each treatment regime ####
ipcw_cod_unstab <- data_prost_long$ipcw_odeath_unstab
cde1_hazardP <- nonParametricCumHaz(ipcw_cod_unstab, inputdata=data_prost_long, grp=1, outcomeProstate=TRUE)
cde1_hazardO <- rep(0,length.out=(length(cutTimes)))
cde1 <- nonParametricCumInc(cde1_hazardP,cde1_hazardO)

cde0_hazardP <- nonParametricCumHaz(ipcw_cod_unstab, inputdata=data_prost_long, grp=0, outcomeProstate=TRUE)
cde0_hazardO <- rep(0,length.out=(length(cutTimes)))
cde0 <- nonParametricCumInc(cde0_hazardP,cde0_hazardO)

cde<-cde1[c(11,23,35,47)]-cde0[c(11,23,35,47)]
#print(cde)
cde1[c(11,23,35,47)]
cde0[c(11,23,35,47)]

cde_all<-cde1-cde0
#print(cde_all)





#### Step 2-SDE. Calculation of the weights for separable direct effect
## Cloning for treated
data_prost_long1 <- data_prost_long %>% 
  mutate(
    rx_c = 1
  )
data_prost_long1 <- data_prost_long1 %>% 
  mutate(
    pred_odeath1 = 1 - predict(odeath_fit_c, newdata = data_prost_long1, type = 'response'),
    hazardOcompare = predict(odeath_fit_c, newdata = data_prost_long1, type = 'response'),
  ) %>% 
  group_by(patno) %>% 
  #cumulative probability of other death per person over all months if a_D = 1
  mutate(
    cum_pred_odeath1 = cumprod(pred_odeath1)
  ) %>%
  ungroup() %>% 
  select(pred_odeath1, cum_pred_odeath1,hazardOcompare)

## Cloning for untreated
data_prost_long0 <- data_prost_long %>% 
  mutate(
    rx_c = 0
  ) %>% 
  mutate(
    pred_odeath0 = 1 - predict(odeath_fit_c, newdata = ., type = 'response'),
    pred_odeath0_comp = predict(odeath_fit_c, newdata = ., type = 'response'),
  ) %>% 
  group_by(patno) %>% 
  #cumulative probability of other death per person over all months if a_D = 0
  mutate(
    cum_pred_odeath0 = cumprod(pred_odeath0)
  ) %>% 
  ungroup() %>% 
  select(pred_odeath0, cum_pred_odeath0, pred_odeath0_comp)

## Other input data
data_other <- data_prost_long %>% 
  select(rx, dtime, eventCens, otherDeath, prostateDeath)

data_prost_long_sde <- bind_cols(data_prost_long1, data_prost_long0, data_other) %>% 
  mutate(
    sde1_odeath_unstab = 1/cum_pred_odeath1,
    sde0_odeath_unstab = 1/cum_pred_odeath0,
    w_s1 = I(rx==1)*cum_pred_odeath1/cum_pred_odeath1 + I(rx==0)*cum_pred_odeath1/cum_pred_odeath0,
    # w_s1 : weight for aD=1
    w_s0 = I(rx==1)*cum_pred_odeath0/cum_pred_odeath1 + I(rx==0)*cum_pred_odeath0/cum_pred_odeath0,
    # w_s0 : weight for aD=0
  )  
summary(data_prost_long_sde$w_s1)
summary(data_prost_long_sde$w_s0)

#### Step 3-SDE. Estimation of the separable direct effect with utility function
## Cumulative hazard and cumulative incidence for each treatment regime ##
# Cumulative hazard and cumulative incidence for a_Y, a_D
discrete_cuminc_prost <- function(weight_vector, inputdata, grp, outcome_y=TRUE,follow_up=1:max_time){
  event_vec <- rep(NA, length.out=length(follow_up))
  counter <- 1 
  # count number of individuals in grp (that is, we cound those who were present at baseline)
  n_grp <- sum(inputdata$dtime==0 & inputdata$rx==grp)
  for(i in follow_up){
    if(outcome_y){
      indices <- inputdata$dtime==i & inputdata$rx == grp & inputdata$eventCens==0 & inputdata$otherDeath==0 
      eventIndicator <- indices & inputdata$prostateDeath==1 
    }else{
      indices <- inputdata$dtime==i & inputdata$rx == grp & inputdata$eventCens==0  
      eventIndicator <- indices &  inputdata$otherDeath==1
    }
    event_vec[counter] <- sum(weight_vector[eventIndicator]) / n_grp
    counter <- counter+1
  }
  output_cuminc <- cumsum(event_vec)
  return(output_cuminc)
  return(data.frame(cumIncTreated,cumIncTreatAy,cumIncPlacebo,cumIncTreatedIPW,cumIncTreatAyIPW,cumIncPlaceboIPW))
}  
sde11 <- discrete_cuminc_prost(data_prost_long_sde$w_s1, data_prost_long_sde, grp = 1, follow_up = cutTimes)
sde00 <- discrete_cuminc_prost(data_prost_long_sde$w_s0, data_prost_long_sde, grp = 0, follow_up = cutTimes)
sde10 <- discrete_cuminc_prost(data_prost_long_sde$w_s0, data_prost_long_sde, grp = 1, follow_up = cutTimes)
sde01 <- discrete_cuminc_prost(data_prost_long_sde$w_s1, data_prost_long_sde, grp = 0, follow_up = cutTimes)

sde1 <- sde11[c(11,23,35,47)]-sde01[c(11,23,35,47)]
sde0 <- sde10[c(11,23,35,47)]-sde00[c(11,23,35,47)]
sde11[c(11,23,35,47)]
sde00[c(11,23,35,47)]
sde10[c(11,23,35,47)]
sde01[c(11,23,35,47)]

sde1_all <- sde11-sde01
sde0_all <- sde10-sde00


list(
  sde11 = sde11,
  sde10 = sde10,
  sde01 = sde01,
  sde00 = sde00,
  sde1_all = sde1_all,
  sde0_all = sde0_all,
  cde1 = cde1,
  cde0 = cde0,
  cde_all = cde_all,
  max_w_s1 = max(data_prost_long_sde$w_s1, na.rm = TRUE),
  max_w_s0 = max(data_prost_long_sde$w_s0, na.rm = TRUE),
  max_ipcw = max(data_prost_long$ipcw_odeath_unstab, na.rm = TRUE)
)

}


## -------------------------  Original results
original_results <- Estimates_ProstateDataset(data0)

## Graph 
cutTimes <- c(0:49) 
ana_wide <- bind_cols(Months = cutTimes+1, SDE1 = original_results$sde1_all, SDE0 = original_results$sde0_all,  CDE = original_results$cde_all)
ana <- gather(data = ana_wide, key = Effect, value = RD, SDE1:CDE, factor_key = FALSE)
ana$Effect <- factor(ana$Effect, levels=c("SDE0", "SDE1", "CDE")) # Reorder Effect levels
g1 <- ggplot( ) + 
  geom_step(data = ana, aes(x = Months, y = RD, color = Effect, linetype = Effect), linewidth = 1.2) +
  theme_bw() +
  theme(
    plot.title = element_text(size=25),
    axis.title = element_text(size=25),
    axis.text = element_text(size=20), 
    legend.title=element_blank(), #change legend title font size
    legend.text = element_text(size=16), #change legend text font size
    legend.text.align = 0, #align legend left
    legend.position = c(0.25, 0.1),
    legend.background = element_rect(fill="white", color="white")
  ) +
  geom_hline(yintercept=0) + 
  xlab("Months") +
  ylab("Risk difference")+
  scale_color_manual(values = c(CDE = "darkgoldenrod1", SDE1 = "blue3", SDE0 = "#1C86EE"), 
                     labels=c(CDE="Controlled direct effect", 
                              SDE1=TeX("Separable direct effect for $a_D$=1"), 
                              SDE0=TeX("Separable direct effect for $a_D$=0"))
  )+
  scale_linetype_manual(values=c(CDE="solid", SDE1="solid", SDE0="solid"),
                        guide = 'none') 
g1
#ggsave("Prostate results all.png", dpi=300, width = 24, height = 15, units = "cm")


## ------------------------- Obtain Bootstrap Variances
n_boot <- 1000
n <- nrow(data0)
seed <- 123

targets <- c("sde11", "sde10", "sde01", "sde00", "sde1_all", "sde0_all", "cde1", "cde0", "cde_all")
scalar_targets <- c("max_w_s1", "max_w_s0", "max_ipcw")
boot_list <- lapply(1:length(targets), function(i) matrix(NA, nrow = n_boot, ncol = length(cutTimes)))
names(boot_list) <- targets
boot_scalar <- lapply(scalar_targets, function(x) numeric(n_boot))
names(boot_scalar) <- scalar_targets

set.seed(seed)
for (i in 1:n_boot){
  message("Bootstrap: ", i, "/", n_boot)
  
  idx <- sample(1:n, n, replace = TRUE)
  bootsample <- data0[idx, ]
  bootres <- Estimates_ProstateDataset(bootsample)
  
  for (j in seq_along(targets)) {
    boot_list[[j]][i, ] <- bootres[[targets[j]]]
  }
  
  for (k in seq_along(scalar_targets)) {
    val <- bootres[[scalar_targets[k]]]
    boot_scalar[[k]][i] <- ifelse(is.numeric(val), val, NaN)
  }
}

bootstrap_summary <- function(bootstrap_matrix, time_indices) {

  selected_mat <- bootstrap_matrix[, time_indices, drop = FALSE]

  # 95% confidence interval
  estimate <- colMeans(selected_mat, na.rm = TRUE)
  se <- apply(selected_mat, 2, sd, na.rm = TRUE)
  var <- se^2
  ci_lower <- apply(selected_mat, 2, quantile, probs = 0.025, na.rm = TRUE)
  ci_upper <- apply(selected_mat, 2, quantile, probs = 0.975, na.rm = TRUE)
  
  result <- data.frame(
    time_index = time_indices,
    estimate = estimate,
    se = se,
    var = var,
    lower95 = ci_lower,
    upper95 = ci_upper
  )
  
  return(result)
}


bootstrap_summary(boot_list$sde1_all, c(11, 23, 35, 47))
bootstrap_summary(boot_list$sde0_all, c(11, 23, 35, 47))
bootstrap_summary(boot_list$cde_all, c(11, 23, 35, 47))



## ------------------------- Plot estimates and confidence intervals 
res_sde0 <- 
  bind_cols(
    tibble::tibble(pointestimate = original_results$sde0_all[c(11, 23, 35, 47)]),
    bootstrap_summary(boot_list$sde0_all, c(11, 23, 35, 47))
  ) %>% 
  mutate(Effect = "SDE0")
res_sde1 <- 
  bind_cols(
    tibble::tibble(pointestimate = original_results$sde1_all[c(11, 23, 35, 47)]),
    bootstrap_summary(boot_list$sde1_all, c(11, 23, 35, 47))
  ) %>% 
  mutate(Effect = "SDE1")
res_cde <- 
  bind_cols(
    tibble::tibble(pointestimate = original_results$cde_all[c(11, 23, 35, 47)]),
    bootstrap_summary(boot_list$cde_all, c(11, 23, 35, 47))
  ) %>% 
  mutate(Effect = "CDE")
plot <- bind_rows(res_sde0, res_sde1, res_cde) %>% 
  mutate(Months = as.factor(time_index+1))
View(plot)

ggplot(plot, aes(x = pointestimate, y = Months, color = Effect)) + #estimate from original dataset
#ggplot(plot, aes(x = estimate, y = Months, color = Effect)) + #estimate from average of bootstrap samples
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbarh(aes(xmin = lower95, xmax = upper95),
                 height = 0.3, position = position_dodge(width = 0.6)) +
  geom_vline(xintercept = seq(-0.3, 0.2, by = 0.1), 
             color = "grey80", linetype = "solid", linewidth = 0.3) +
  geom_vline(xintercept = 0, 
             color = "black", linetype = "solid", linewidth = 0.3) +
  labs(x = "Risk Difference", y = "Months", color = "Estimand") +
  theme_bw()  +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank()
  ) + 
  scale_color_manual(values = c(SDE1 = "blue3", SDE0 = "#1C86EE", CDE = "darkgoldenrod1"), 
                     limits = c("SDE1", "SDE0", "CDE"),
                     labels=c(SDE1=TeX("Separable direct effect for $a_D$=1"), 
                              SDE0=TeX("Separable direct effect for $a_D$=0"),
                              CDE="Controlled direct effect")
  )

