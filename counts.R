library(readxl)
library(tidyverse)
library(here)

data <- read_excel(here("data_raw/Countingsheet.xlsx"), sheet = "Sheet2")
