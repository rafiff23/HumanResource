library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)
library(hrbrthemes)
library(viridis)
library(ggridges)
library(highcharter)
library(rio)
library(purrr)
library(stringr)
library(scales)
library(stringr)
library(DT)
options(scipen = 99)

df <- read_csv("HRDataset_v14.csv")

df_clean <- df %>% 
  mutate_at(.vars = c("RecruitmentSource", "PerformanceScore", "EmpSatisfaction", "Department", "EmploymentStatus", "TermReason", "Sex","CitizenDesc" ), 
            .funs = as.factor) %>% 
  mutate_at(.vars = c("MarriedID", "FromDiversityJobFairID"), 
            .funs = as.logical)
df_clean_2 <- df_clean %>% mutate_at(.vars = c("EmpSatisfaction"), .funs = as.numeric)

df_clean$DateofHire <- mdy(df$DateofHire) 
df_clean$DOB <- mdy(df$DOB) 
df_clean$DateofTermination <- mdy(df$DateofTermination)
df_clean$LastPerformanceReview_Date <- mdy(df$LastPerformanceReview_Date)
df_clean <- select(df_clean, -Zip, -HispanicLatino, -RaceDesc)
tb <- select(df_clean,Employee_Name ,ManagerName ,Department, Position,SpecialProjectsCount ,EmpSatisfaction, PerformanceScore,Absences ,Salary)
