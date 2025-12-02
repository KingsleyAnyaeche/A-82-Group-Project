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

#changed the data set column name into golf ball type(Current/without coating and new/with coating) and driving distance using pivot_longer() function
Golf_csv <- pivot_longer(Golf_csv, cols = Current : New, names_to = "Golf_ball_types", values_to = "Driving_distance" )
colnames(Golf_csv)

#Changed current and New to "without coating" and "with coating" to match our research question
Golf_csv$Golf_ball_types <- recode(
  Golf_csv$Golf_ball_types, "Current" = "Without coating", "New" = "With coating"
)

#Confirmed changes
table(Golf_csv$Golf_ball_types)

#The box plot base plot
png("plots/boxplot_distance.png", width = 600, height =400 )
boxplot(
  Driving_distance ~ Golf_ball_types,
  data = Golf_csv,
  main = " BOX PLOT
  Driving Distance (m) of Golf Ball Types",
  xlab = "Golf ball types",
  ylab = "Driving distance (m)",
  col = c("yellow", "red")
)
dev.off()

#The histogram base plot
png("plots/histogram_distance.png", width = 600, height = 400)
h <- hist(
  Golf_csv$Driving_distance,
  main = "HISTOGRAM
  Distribution of Golf Ball Driving Distances (m)",
  xlab = "Driving distance (m)",
  ylab = "Frequency",
  col = "steelblue",
  border = "black",
  las = 2
)

#Overlay a normal curve
mn   <- mean(Golf_csv$Driving_distance, na.rm = TRUE)
stID <- sd(Golf_csv$Driving_distance, na.rm = TRUE)
range_x <- range(Golf_csv$Driving_distance, na.rm = TRUE)
x <- seq(range_x[1], range_x[2], length.out = 200)
y <- dnorm(x, mean = mn, sd = stID)
y <- y * diff(h$mids[1:2]) * length(Golf_csv$Driving_distance)
lines(x, y, col = "red", lwd = 2)

dev.off()

#Independent two sample t-test
result_t <- t.test(
  Driving_distance ~ Golf_ball_types, 
  data = Golf_csv,
  var.equal = TRUE
)

if(!dir.exists("appendix")){
  dir.create("appendix")
}

sink("appendix/Rscript.log")
print(result_t)
sink()

