setwd("C:/Users/BuchananLab/Desktop")
setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/IRT/data")

library(readxl)
paper = read_excel("IRT_Paper_surveys_FINAL.xlsx")
rn15 = read_excel("MSRN.xlsx")
rn16 = read_excel("MSRN2016.xlsx")
rr15 = read_excel("MSRR.xlsx")
rr16 = read_excel("MSRR2016.xlsx")

paper$source = "paper"
paper$year = "2016"
rn15$source = "not random"
rn15$year = "2015"
rn16$source = "not random"
rn16$year = "2016"
rr15$source = "random"
rr15$year = "2015"
rr16$source = "random"
rr16$year = "2016"

library(data.table)
data4 = rbindlist(list(paper, rn15, rn16, rr15, rr16), fill = TRUE)

write.csv(data4, "all_data_combined.csv", row.names = F)
