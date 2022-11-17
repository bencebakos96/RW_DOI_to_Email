#### Packages ####

library(tidyverse)
library(readxl)

#### Data ####

data <- read_excel("data/RWDBDNLD11102022.xlsx")

reasons <- c("Concerns/Issues About Data", "Concerns/Issues About Results",
             "Duplication of Data", "Error in Analyses", "Error in Data",
             "Error in Results and/or Conclusions", "Original Data not Provided",
             "Plagiarism of Data", "Results Not Reproducible",
             "Unreliable Data", "Unreliable Results")

data <- data %>% 
  filter(
    str_detect(Reason, paste(reasons, collapse = "|"))
  )

#### Export ####

write_excel_csv(data, "data/RWDBDNLD11102022_filtered.csv")
