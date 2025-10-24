**Updated: Oct 25, 2025**

# Misaligned questions and statistics with competing events
This repository contains R code for reproducing the results presented in the paper:  
**"Illustrating implications of misaligned causal questions and statistics in settings with competing events and interest in treatment mechanism"**  
by *Takuya Kawahara, Sean McGrath, and Jessica G. Young*.

---

## Repository Contents

### 1. Implications of Estimand Error (Section 4.1)

Scripts:
- `EstimandError_non_rare.R` → generates **Figure 3**
- `EstimandError_rare.R` → generates **Web Figure 1**

### 2. Relative Implications of Non-identification Error (Section 4.2)

Script:
- `NonIdError.R` → generates **Figure 4**

### 3. Variance Implications under Near Positivity Violations: A Simulation Study (Section 4.3)

Scripts:
- Define parameters in `simmult1-6.R`
- Then run `sim_function.R` (iterates **20,000** times)  
  → reproduces **Table 1**

### 4. Comparative Empirical Analysis Using a Real Randomized Trial of Estrogen Therapy (Section 5)

Script:
- `ProstateAnalysis_bootstrap.R`  
  analyzes the dataset `prostate.csv` (located in the `data/` folder)  
  → generates **Figure 5**
