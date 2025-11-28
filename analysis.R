#load data set and import tidyverse and tidyr library
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

#change the dataset column name into ball type(Current/without coating and new/with coating) and driving distance using pivot_longer() function
Golf_csv <- pivot_longer(Golf_csv, cols = Current : New, names_to = "Ball_types", values_to = "Driving_distance" )
colnames(Golf_csv)

#Plot folder
if(!dir.exists("plots")){
  dir.create("plots")
}

#The box plot base plot
png("plots/boxplot_distance.png", width = 600, height =400 )
boxplot(
  Driving_distance ~ Ball_types,
  data = Golf_csv,
  main = "Driving distance of golf ball types",
  xlab = "Ball types (current/without coating and new/with coating)",
  ylab = "Driving distance (m)",
  col = c("yellow", "red")
)
dev.off()

#The histogram base plot
png("plots/histogram_distance.png", width = 600, height = 400)
hist(
  Golf_csv$Driving_distance, 
  main = "Distribution of driving distances across golf ball types",
  xlab = "Driving distance (m)",
  col = "steelblue",
  border = "black",
)
dev.off()

#The t-test
result_t <- t.test(Driving_distance ~ Ball_types, data = Golf_csv)
result_t

if(!dir.exists("appendix")){
  dir.create("appendix")
}

sink("appendix/Rscript.log")
print(result_t)
sink()

