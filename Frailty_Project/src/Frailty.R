library(tidyverse)

raw_frailty <- read.csv("C:/Users/yuqin/Desktop/Frailty_Project/data_raw/raw_frailty.csv")

frailtyPos <- raw_frailty %>% 
  filter(frailty == 'Y')

write_csv(frailtyPos, "C:/Users/yuqin/Desktop/Frailty_Project/data_clean/clean_frailty.csv")
