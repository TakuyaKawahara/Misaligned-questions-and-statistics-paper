Updated: Feb 24, 2025

# Estimands-versus-algorithms-for-competing-events
R codes which reproduce the results of the paper "Illustrating implications of misaligned questions and statistics with
competing events and interest in treatment mechanism" (working paper)

# Contents
Figure 3 is produced by IdentitySlippage_ad=0.r
Figure 4 is produced by NonIdError.r
Figure 5 is produced by ProstateAnalysis.r, along with prostate.csv
Figure A1 is produced by IdentitySlippage_ad=1.r

Simulation results (Table 1 and Table A2) are produced by the following codes:
- each simmult1.r - simmult6.r defines a simulation scenario, runs core simulation (sim function.r), and outputs .csv of 20000 simulation runs
- csv.s are combined by createTable_20241108
