##################
# main_result_for_table3 #
##################

# パッケージ読み込み ---------------------------------------------------------------
library(tidyverse)
library(haven)
library(modi)
library(Hmisc)
library(estimatr)


# データ読み込み -----------------------------------------------------------------
df_china <- 
  read_dta("Autor-Dorn-Hanson-ChinaSyndrome-FileArchive/dta/workfile_china.dta")


# iv regress --------------------------------------------------------------
# What's the original code?
# ivregress 2sls d_sh_empl_mfg (d_tradeusch_pw=d_tradeotch_pw_lag) t2 [aw=timepwt48], cluster(statefip) first

# do we have required variables?
df_china |> 
  select(d_sh_empl_mfg, d_tradeusch_pw, d_tradeotch_pw_lag, t2)

# run 6 models
# eststo: ivregress 2sls d_sh_empl_mfg (d_tradeusch_pw=d_tradeotch_pw_lag) t2 [aw=timepwt48], cluster(statefip) first
# eststo: ivregress 2sls d_sh_empl_mfg (d_tradeusch_pw=d_tradeotch_pw_lag) l_shind_manuf_cbp t2 [aw=timepwt48], cluster(statefip) first
# eststo: ivregress 2sls d_sh_empl_mfg (d_tradeusch_pw=d_tradeotch_pw_lag) l_shind_manuf_cbp reg* t2 [aw=timepwt48], cluster(statefip) first
# eststo: ivregress 2sls d_sh_empl_mfg (d_tradeusch_pw=d_tradeotch_pw_lag) l_shind_manuf_cbp reg* l_sh_popedu_c l_sh_popfborn l_sh_empl_f t2 [aw=timepwt48], cluster(statefip) first
# eststo: ivregress 2sls d_sh_empl_mfg (d_tradeusch_pw=d_tradeotch_pw_lag) l_shind_manuf_cbp reg* l_sh_routine33 l_task_outsource t2 [aw=timepwt48], cluster(statefip) first
# eststo: ivregress 2sls d_sh_empl_mfg (d_tradeusch_pw=d_tradeotch_pw_lag) l_shind_manuf_cbp reg* l_sh_popedu_c l_sh_popfborn l_sh_empl_f l_sh_routine33 l_task_outsource t2 [aw=timepwt48], cluster(statefip) first
# model 1
model1 <- 
  iv_robust(
    d_sh_empl_mfg ~ d_tradeusch_pw + t2 | d_tradeotch_pw_lag + t2,
    data = df_china,
    weights = timepwt48,
    clusters = statefip,
    se_type = "stata"
  ) 
summary(model1)

# model 2
model2 <- 
  iv_robust(
    d_sh_empl_mfg ~ d_tradeusch_pw + l_shind_manuf_cbp + t2 | d_tradeotch_pw_lag + l_shind_manuf_cbp + t2,
    data = df_china,
    weights = timepwt48,
    clusters = statefip,
    se_type = "stata"
  ) 
summary(model2)

# model 3
model3 <- 
  iv_robust(
    d_sh_empl_mfg ~ d_tradeusch_pw + l_shind_manuf_cbp + t2 | d_tradeotch_pw_lag + l_shind_manuf_cbp + t2,
    data = df_china,
    weights = timepwt48,
    clusters = statefip,
    se_type = "stata"
  ) 
summary(model3)


