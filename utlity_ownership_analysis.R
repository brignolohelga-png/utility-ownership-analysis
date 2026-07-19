#ASRI project 
#Do utility rates differ across utility ownership type?

library(tidyverse)
library(dplyr)
library(ggplot2)

iou <- read.csv("iou_zipcodes_2020.csv")
non_iou <- read.csv("non_iou_zipcodes_2020.csv")

#explore data
names(iou)
names(non_iou)
head(iou)
head(non_iou)
dim(iou)
dim(non_iou)


#merge datasets
utilities <- bind_rows(iou, non_iou)


#summary statistics
table1 <- utilities %>%
  group_by(ownership) %>%
  summarize(
    mean_res_rate = mean(res_rate, na.rm = TRUE),
    sd_res_rate = sd(res_rate, na.rm = TRUE),
    n = n()
  )

View(table1)


#boxplot
ggplot(utilities,
       aes(x = ownership,
           y = res_rate)) +
  geom_boxplot() +
  labs(
    title = "Residential Electricity Rates by Ownership Type",
    x = "Ownership Type",
    y = "Residential Rate"
  )
ggsave("boxplot.png")


#ANOVA
anova_model <- aov(res_rate ~ ownership,
                   data = utilities)

summary(anova_model)
anova_table <- summary(anova_model)[[1]]
View(anova_table)


#Tukey's post-hoc test
tukey <- TukeyHSD(anova_model)

tukey

tukey_table <- as.data.frame(tukey$ownership)
View(tukey_table)

write.csv(table1, "table1.csv")
write.csv(anova_table, "anova_table.csv")
write.csv(tukey_table, "tukey_table.csv")

