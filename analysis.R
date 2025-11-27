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
 
#The box plot
box_p <- ggplot(Golf_csv, aes(x = Ball_types, y = Driving_distance, fill = Ball_types)) + geom_boxplot() + labs( title = "Driving distance by ball type",
  x = "Ball types (coated and uncoated)",
  y = "Driving Distance",
) +
  theme_minimal()
ggsave("plots/boxplot_distance.png", box_p, width = 6, height = 4)

#The histogram
hist_p <- ggplot(Golf_csv, aes(x = Driving_distance, fill = Ball_types)) + geom_histogram(alpha = 0.6, position = "identity", bin = 10) + labs( title = "Distribution of driving distances",
  x = "Driving distance",
  y = "Frequency"
)
theme_minimal()
ggsave("plots/histogram_distance.png", hist_p, width = 6, height = 4)

#The t-test
result_t <- t.test(Driving_distance ~ Ball_types, data = Golf_csv)
result_t

if(!dir.exists("appendix")){
  dir.create("appendix")
}
sink("appendix/Rscript.log")
print(result_t)
sink()

