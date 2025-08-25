##################
# visualize data #
##################

# パッケージ読み込み ---------------------------------------------------------------
library(tidyverse)
library(haven)
library(modi)
library(Hmisc)
library(estimatr)


# データ読み込み -----------------------------------------------------------------
df_china_long <- 
  read_dta("Autor-Dorn-Hanson-ChinaSyndrome-FileArchive/dta/workfile_china_long.dta")


# check data --------------------------------------------------------------
df_china_long |> 
  summary()


# first stage regression --------------------------------------------------
#primitive ver.
lm(d_tradeusch_pw ~ d_tradeotch_pw_lag + l_shind_manuf_cbp,
   data = df_china_long
   ) |> 
  summary()

#weightをかける
df_china_long_w <- df_china_long |>
  mutate(
    d_tradeusch_pw_weighted = d_tradeusch_pw * timepwt48,
    d_tradeotch_pw_lag_weighted = d_tradeotch_pw_lag * timepwt48,
    l_shind_manuf_cbp_weighted = l_shind_manuf_cbp * timepwt48
  )
# turns out this was not necesarry...


# regression
model <- lm_robust(d_tradeusch_pw ~ d_tradeotch_pw_lag + l_shind_manuf_cbp,
                   data = df_china_long,
                   clusters = statefip,
                   weights = timepwt48,
                   se_type = "stata")

# see the result
model |> 
  summary()

model |> 
  broom::tidy()

# scatter plot
df_china_long |> 
  ggplot(aes(x = d_tradeotch_pw_lag, y = d_tradeusch_pw)) + 
  geom_point() +
  geom_smooth(method = "lm_robust")


x_fit <-
  lm_robust(d_tradeusch_pw ~ l_shind_manuf_cbp,
                   data = df_china_long,
                   clusters = statefip,
                   weights = timepwt48,
                   se_type = "stata")

x_res = x_fit

z_fit <-
  lm_robust(d_tradeotch_pw_lag ~ l_shind_manuf_cbp,
            data = df_china_long,
            clusters = statefip,
            weights = timepwt48,
            se_type = "stata")
