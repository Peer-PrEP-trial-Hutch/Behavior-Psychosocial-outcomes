# set-up ----
rm(list=ls())
library(pacman)

dataws=getwd()


p_load(writexl, readxl, janitor, openxlsx,
       tidyverse, dplyr, scales, 
       likert, labelled,ggpubr, grid,
       compareGroups, labelled,
       
       sandwich,lmtest,
       
       lme4, lqmm,quantreg,
       
       sjstats,
       
       gee,geepack, broom,
       
       car,
       
       Amelia, mice,
       
       jtools
)



# Table 1 ----
## index peers ----


labels_df_total=c(
  # Demographics
  p1_s0_arm="Study arm",
  p1_s1_q1="Age*",
  p1_s1_q4="Marital status",
  p1_s1_q6="Relationship status",
  p1_s1_q2="Years of school*",
  p1_s1_q3="Currently in school",
  p1_s1_q7="Monthly income*",
  
  # Sexual and reproducttive behaviors
  p1_s2_q17="Age of first sex",
  p1_s2_q7="No. of sexual partners*",
  new_sexpart_dico="New sexual partner (past 3 months)",
  p1_s2_q18="Ever being pregnant",
  p1_s2_q12="Emergency contraception (6 months)",
  p1_s2_q13="Emergency contraception twice (past 6 months)",
  
  # Behaviors associated witth HIV
  p1_s2_q10="Unprotected sex with partner of unknown/positive HIV status (past 6 months)",
  p1_s2_q11="STI (past 6 months)",
  p1_s2_q14="PEP use more than twice (past 6 months)",
  p1_s2_q15="Sex under influence of drugs (past 6 months)",
  p1_s2_q16="Sex exchange (past 6 months)",
  
  
  # Prior PrEP use
  p1_s5_q1_dico="Prior PrEP use"
  
  
)

## referred peers ----


# Table 2 ----





# Figure 2 ----


# Figure 3 ----