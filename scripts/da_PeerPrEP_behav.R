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

ds_pbl=readxl::read_excel(file.path(dataws,paste0("datasets/1. df_pbl_behav.xlsx")))

ds_cfu=readxl::read_excel(file.path(dataws,paste0("datasets/4. ds_cfu_behav.xlsx")))



source(file.path(dataws,"functions/multi_table.R"))
source(file.path(dataws,"functions/is_dup.R"))
source(file.path(dataws,"functions/toNA.R"))
source(file.path(dataws,"functions/toDate.R"))


propmiss = function(dataframe) {
  m = sapply(dataframe, function(x) {
    data.frame(
      nmiss=sum(is.na(x)),
      n=length(x),
      propmiss=sum(is.na(x))/length(x))
  })
  d = data.frame(t(m))
  d = sapply(d, unlist)
  d = as.data.frame (d)
  d$variable = row.names (d)
  row.names(d) = NULL
  d = cbind(d[ncol(d)], d[-ncol(d)])
  return(d[order(d$propmiss),])
}




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

vars=c(
  "p1_s0_arm",
  "p1_s1_q1",
  "p1_s1_q4",
  "p1_s1_q6",
  "p1_s1_q2",
  "p1_s1_q3",
  "p1_s1_q7",
  "p1_s2_q17",
  "p1_s2_q7",
  "new_sexpart_dico",
  "p1_s2_q18",
  "p1_s2_q12",
  "p1_s2_q13",
  "p1_s2_q10",
  "p1_s2_q11",
  "p1_s2_q14",
  "p1_s2_q15",
  "p1_s2_q16",
  "p1_s5_q1_dico"
)


vars_c=paste(vars,collapse = "+")
vars_f=as.formula(paste("p1_s0_arm~",vars_c))

df_table_total_in_f=ds_pbl[,vars]

#ds_pbl %>% dplyr::select(all_of(vars))
df_table_total_in=set_variable_labels(df_table_total_in_f, 
                                      .labels = labels_df_total)
#label(df_table_total_in)

#compareGroups(year ~ age + triglyc, data=regicor, method = c(triglyc=2), Q1=0, Q3=1)

df_table_total_out=compareGroups(vars_f, data = df_table_total_in,
                                 method=c(p1_s1_q1=2,p1_s1_q2=2,p1_s2_q7=2,p1_s1_q7=2,p1_s2_q6=2,prep_first=2,
                                          age_gap=2,p1_s2_q18a=2,p1_s2_q8=2,p1_s2_q17=2), simplify = T) %>% 
  createTable(digits = 0,
              show.all = TRUE,
              show.n=T)






## referred peers ----


labels_df_total_cfu=c(
  # Demographics
  c2_s0_arm= "Study arm",
  c2_s1_q1 = "Age*",
  c2_s1_q4 = "Marital status",
  c2_s1_q5 = "Relationship status",
  c2_s1_q2 = "Years in school*",
  c2_s1_q3 = "Currently in school",
  c2_s1_q6 = "Monthly income",
  c2_s1_q7_1 = "Enrolled in another HIV study",
  
  # Sexual and reproductive behaviors
  c2_s2_q17 = "Age of first sex*",
  c2_s2_q6 = "No. of sexual partners*",
  new_sexpart_dico = "New sexual partner (past 3 months)",
  c2_s2_q18 = "Ever being pregnant",
  c2_s2_q12 = "Prior emergency contraception use (past 6 months)",
  c2_s2_q13 = "Emergency contraception twice (past 6 months)",
  
  # Behaviors associated with HIV
  c2_s2_q10 = "Unprotected sex with partner of unknown/positive HIV status (past 6 months)",
  c2_s2_q11 = "STI (past 6 months)",
  c2_s2_q14 = "PEP use more than twice (past 6 months)",
  c2_s2_q15 = "Sex under influence of drugs (past 6 months)",
  c2_s2_q16 = "Sex exchange (past 6 months)",
  
  # Prior PrEP use
  c2_s6_q1_1_rec = "Prior PrEP use",
  c2_s6_q1_1_1 = "Currently using PrEP for HIV prevention"
)



vars=c(
  "c2_s0_arm",
  "c2_s1_q1",
  "c2_s1_q4",
  "c2_s1_q5",
  "c2_s1_q2",
  "c2_s1_q3",
  "c2_s1_q6",
  "c2_s1_q7_1",
  "c2_s2_q17",
  "c2_s2_q6",
  "new_sexpart_dico",
  "c2_s2_q18",
  "c2_s2_q12",
  "c2_s2_q13",
  "c2_s2_q10",
  "c2_s2_q11",
  "c2_s2_q14",
  "c2_s2_q15",
  "c2_s2_q16",
  "c2_s6_q1_1_rec",
  "c2_s6_q1_1_1"
       
)

vars_c=paste(vars,collapse = "+")
vars_c_f=as.formula(paste("~",vars_c))
vars_f=as.formula(paste("c2_s0_arm~",vars_c))


df_table_total_in_cfu=ds_cfu %>% dplyr::select(all_of(vars))
df_table_total_in_cfu=set_variable_labels(df_table_total_in_cfu, 
                                          .labels = labels_df_total_cfu)



df_table_total_cfu_out=compareGroups(vars_f, data = df_table_total_in_cfu,
                                     method = c(c2_s1_q1=2, c2_s2_q6=2, c2_s1_q2=2, age_gap=2,c2_s1_q6=2,
                                                c2_s2_q18a=2,c2_s2_q17=2,prep_first=2,c2_s2_q7=2),simplify=T) %>% 
  createTable(show.n = T,
              show.all = T,
              digits=0)





# Table 2 ----





# Figure 2 ----


# Figure 3 ----