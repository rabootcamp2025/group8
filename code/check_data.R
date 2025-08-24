##############
# check data #
##############

# パッケージ読み込み ---------------------------------------------------------------
library(tidyverse)
library(haven)
library(modi)
library(Hmisc)


# データ読み込み -----------------------------------------------------------------
df_china <- 
  read_dta("Autor-Dorn-Hanson-ChinaSyndrome-FileArchive/dta/workfile_china.dta")

# All variables with the prefix “l_” refer to start-of-period values while variables
# with the prefix “d_” correspond to 10-year equivalent changes. This file is the basis for all
# Tables in the paper except Tables 1 and 2

df_china |>
  select(starts_with("d_")) |> 
  names()

df_china |> 
  select(d_tradeusce_pw ) |> 
  summary()

# データの確認のため
df_china |>
  select(yr, d_tradeusce_pw) |>
  group_by(yr) |>
  summarise(mean = mean(d_tradeusce_pw))

# App Table1 panel A
df_china |>
  group_by(yr) |>
  dplyr::summarize(
    w_10 = weighted.quantile(d_tradeusch_pw, timepwt48, prob = 0.10),
    w_25 = weighted.quantile(d_tradeusch_pw, timepwt48, prob = 0.25),
    w_50 = weighted.quantile(d_tradeusch_pw, timepwt48, prob = 0.5),
    w_75 = weighted.quantile(d_tradeusch_pw, timepwt48, prob = 0.75),
    w_90 = weighted.quantile(d_tradeusch_pw, timepwt48, prob = 0.9)
  )

# App Table1 panel B

# Create a subset data for getting top 40 populated cities in 1990
df_china_top40 <- df_china |>
  select(yr, czone, city, d_tradeusch_pw, l_popcount) |> 
  mutate(pop1990 = if_else(yr == 1990, l_popcount, 0)) |> 
  arrange(desc(pop1990)) |> 
  mutate(
    pop40 = if_else(yr == 1990 & row_number() <= 40, 1, 0)
  ) |> 
  group_by(czone) |> 
  mutate(top40 = sum(pop40)) |> # copying "1" for data in 2000
  ungroup()
  

# 1990-2000
df_china |> 
  filter(yr == 1990) |> 
  select(czone, city, d_tradeusch_pw, l_popcount) |> 
  arrange(desc(l_popcount)) |> 
  slice(1:40) |>
  arrange(desc(d_tradeusch_pw))

# App Table1 panel B
# 2000-2007
df_china |> 
  filter(yr == 2000) |> 
  select(czone, city, d_tradeusch_pw, l_popcount) |> 
  arrange(desc(l_popcount)) |> 
  slice(1:40) |>
  arrange(desc(d_tradeusch_pw))
