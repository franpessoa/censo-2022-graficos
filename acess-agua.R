library(tidyverse)
library(janitor)
library(plotly)
library(stringr)

d <- read_csv("./dados/acesso-agua.csv") |> janitor::clean_names() |> 
  dplyr::rename(loc = x1) |> 
  mutate(
    across(starts_with("n") | contains("lig") | contains("encanada"), 
           function (x) str_replace_all(x, "[\\.\\s,]", "")
    )) |> 
  mutate(across(starts_with("n") | contains("lig") | contains("encanada"), as.numeric))
  
