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

# make residuals ----------------------------------------------------------

# fit our model for X
x_fit <-
  lm(d_tradeusch_pw ~ l_shind_manuf_cbp,
     data = df_china_long,
     weights = timepwt48)

# save the residuals for X
x_res <- resid(x_fit)
length(x_res)

# fit our model for Z
z_fit <-
  lm(d_tradeotch_pw_lag ~ l_shind_manuf_cbp,
     data = df_china_long,
     weights = timepwt48)

# save the residuals for Z
z_res <- resid(z_fit)
length(z_res)

# fit our model for y
y_fit <-
  lm(d_pct_manuf ~ l_shind_manuf_cbp,
     data = df_china_long,
     weights = timepwt48)

# save the residuals for y
y_res <- resid(y_fit)
length(y_res)

# incorporate residuals into our dataset
df_china_long <- df_china_long |> 
  mutate(x_res = x_res,
         z_res = z_res,
         y_res = y_res)
summary(df_china_long)

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
# is this good enough???

# incorporate residuals into our dataset
df_china_long <- df_china_long |> 
  mutate(x_res = x_res,
         z_res = z_res)
summary(df_china_long)

# scatter plot (revenge)
df_china_long |> 
  ggplot(aes(x = z_res, y = x_res)) + 
  geom_point() +
  geom_smooth(method = "lm",
              mapping = aes(weight = timepwt48),
              se = FALSE)


# second stage regression -------------------------------------------------
# regression 
#model <- 
lm_robust(d_pct_manuf ~ d_tradeotch_pw_lag + l_shind_manuf_cbp,
                   data = df_china_long,
                   clusters = statefip,
                   weights = timepwt48,
                   se_type = "stata")

# scatter plot (revenge)
df_china_long |> 
  ggplot(aes(x = z_res, y = y_res)) + 
  geom_point() +
  geom_smooth(method = "lm",
              mapping = aes(weight = timepwt48),
              se = FALSE)
