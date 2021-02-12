library(RODBC)
library(ggplot2)
library(magrittr)
odbcCloseAll()
LOCAL <- odbcConnect("LOCAL")

Q1 <- "Select company,[基本每股盈餘元],[權益總額]  FROM [master].[dbo].[Aggregate_1_New] where date='16'"
Q2 <- "Select company,[基本每股盈餘元],[權益合計] as 權益總額  FROM [master].[dbo].[Aggregate_2_New] where date='16'"
Q3 <- "Select company,[基本每股盈餘元],[權益總額]  FROM [master].[dbo].[Aggregate_3_New] where date='16' "
Q4 <- "Select company,[基本每股盈餘元],[權益總額]  FROM [master].[dbo].[Aggregate_4_New] where date='16'"
Q5 <- "Select company,[基本每股盈餘元],[權益總額]  FROM [master].[dbo].[Aggregate_5_New] where date='16'"
Q6 <- "Select company,[基本每股盈餘元],[權益總額]  FROM [master].[dbo].[Aggregate_6_New] where date='16'"

R1 <- LOCAL %>% sqlQuery(Q1)
R2 <- LOCAL %>% sqlQuery(Q2)
R3 <- LOCAL %>% sqlQuery(Q3)
R4 <- LOCAL %>% sqlQuery(Q4)
R5 <- LOCAL %>% sqlQuery(Q5)
R6 <- LOCAL %>% sqlQuery(Q6)

All <- rbind(R1, R2, R3, R4, R5, R6)
plot(All$`基本每股盈餘元`, All$`權益總額`)
All[All$`基本每股盈餘元` < -10 & All$`權益總額` < 400000000,]