##############
# check data #
##############

# パッケージ読み込み ---------------------------------------------------------------
library(tidyverse)
library(haven)


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

df_china |> 
  select(yr, d_tradeusce_pw) |> 
  group_by(yr) |> 
  summarise(mean = mean(d_tradeusce_pw))
