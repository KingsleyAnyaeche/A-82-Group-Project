#load data set without column name
library(tidyverse)
library(tidyr)
library(readxl)
Golf_csv <- read_excel("Golf.csv.xlsx")

#Make the first row the column name
colnames(Golf_csv) <- as.character(unlist(Golf_csv[1, ]))

#Remove the header row from the data
Golf_csv <- Golf_csv [-1, ]

#Convert numbers to numeric
Golf_csv <- Golf_csv |> mutate(across(everything(), as.numeric))

#confirm the column name
colnames(Golf_csv)