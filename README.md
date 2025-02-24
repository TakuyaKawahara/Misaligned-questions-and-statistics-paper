Updated: Feb 24, 2025

# Estimands-versus-algorithms-for-competing-events
R codes which reproduce the results of the paper "Illustrating implications of misaligned questions and statistics with
competing events and interest in treatment mechanism" (working paper)

# Contents
Figure 3 is produced by IdentitySlippage_ad=0.R

Figure 4 is produced by NonIdError.R

Figure 5 is produced by ProstateAnalysis.R, along with prostate.csv

Figure A1 is produced by IdentitySlippage_ad=1.R

Simulation results (Table 1 and Table A2) are produced by the following codes:
- Each simmult1.R - simmult6.R defines a simulation scenario, runs core simulation code (sim function.R), and outputs .csv of 20000 simulation runs
- csv.s are combined by createTable_20241108
