library(tidyverse)
library(dplyr)
library(ggplot2)

Admissions = read_csv("Admission_Predict.csv")

# Basic Analysis Functions
dataSmall = filter(Admissions, SOP == 5) %>% 
        arrange( `GRE Score`, `Chance of Admit` , CGPA) %>% 
        select( `GRE Score`, `Chance of Admit` , CGPA) %>% 
        rename(CumulativeGPA = CGPA)

# Graphing
ggplot(Admissions, aes(x=`Chance of Admit`, y=`GRE Score`)) + geom_point(aes(size = SOP, color = as.logical(Research))) 

# Linear Modeling
lm(`Chance of Admit` ~ `GRE Score`, Admissions)

# Exporting a CSV
write_csv(Admissions, "New_Admissions.csv")
