library(dplyr)
library(readxl)
library(writexl)
library(ggplot2)
data <- read_xlsx("response_data.xlsx")
summary(data)
getwd()
selected_data <-  data |> select(4,9,10,3,18,19,20)
selected_data
sum(is.na(data))
selected_data <- selected_data |> rename("First Year or Not?"="Which year of study are you currently in?","Prior_knowledge"="Do you have any prior knowledge about Machine Learning?","Retention_Score"="Score")
selected_data
selected_data$Engaging_Score <- rowMeans(selected_data[, c(5,6,7)], na.rm = TRUE)
View(selected_data)

selected_data <- selected_data %>%
  mutate(`First Year or Not?` = if_else(`First Year or Not?` == "1st year", 1, 0))
selected_data <- selected_data %>%
  mutate(`Prior_knowledge` = if_else(`Prior_knowledge` == "Yes", 1, 0))
selected_data <- selected_data |> select(1:4,8)
View(selected_data)
selected_data <- selected_data |> mutate(`Engaging/Dull`=if_else(row_number()>=1 & row_number()<=132,1,0))
View(selected_data)
final_data <- selected_data |> select(1,6,3,2,4,5)
View(final_data)
write_xlsx(final_data, "Cleaned_Data.xlsx")
final_data <- read_xlsx("Cleaned_Data.xlsx")

# ANOVA Test Overall --------------------------------------------------------------

anova_result <- aov(`Retention_Score` ~ `Engaging/Dull` * `Prior_knowledge` * `First Year or Not?`,data = final_data)
View(anova_result)
anova2_result <- aov(`Engaging_Score` ~ `Engaging/Dull` * `Prior_knowledge` * `First Year or Not?`,data = final_data)
View(anova2_result)
summary(anova_result)

# ANOVA Test for each scenario (8 scenarios) --------------------------------------------------------------

#1 Engaging Teaching Method & Having Prior Knowledge & First year
filtered_data_1 <- subset(final_data, `Engaging/Dull` == 1 & Prior_knowledge == 1 & `First Year or Not?` == 1 )
View(filtered_data_1)
anova_retention_01 <- aov(Retention_Score ~ `Engaging/Dull` + Prior_knowledge + `First Year or Not?`, data = filtered_data_1)
summary(anova_retention_01)
anova_engagement_01 <- aov(Engaging_Score ~ `Engaging/Dull` + Prior_knowledge + `First Year or Not?`, data = filtered_data_1)
summary(anova_engagement_01)

#2 Engaging Teaching Method & Having Prior Knowledge & Not First year (senior)
filtered_data_2 <- subset(final_data, `Engaging/Dull` == 1 & Prior_knowledge == 1 & `First Year or Not?` == 0 )
View(filtered_data_2)
anova_retention_02 <- aov(Retention_Score ~ `Engaging/Dull` + Prior_knowledge + `First Year or Not?`, data = filtered_data_2)
summary(anova_retention_02)
anova_engagement_02 <- aov(Engaging_Score ~ `Engaging/Dull` + Prior_knowledge + `First Year or Not?`, data = filtered_data_2)
summary(anova_engagement_02)

#3 Engaging Teaching Method & Not Having Prior Knowledge & First year
filtered_data_3 <- subset(final_data, `Engaging/Dull` == 1 & Prior_knowledge == 0 & `First Year or Not?` == 1 )
View(filtered_data_3)
anova_retention_03 <- aov(Retention_Score ~ `Engaging/Dull` + Prior_knowledge + `First Year or Not?`, data = filtered_data_3)
summary(anova_retention_03)
anova_engagement_03 <- aov(Engaging_Score ~ `Engaging/Dull` + Prior_knowledge + `First Year or Not?`, data = filtered_data_3)
summary(anova_engagement_03)

#4 Dull Teaching Method & Having Prior Knowledge & First year
filtered_data_4 <- subset(final_data, `Engaging/Dull` == 0 & Prior_knowledge == 1 & `First Year or Not?` == 1 )
View(filtered_data_4)
anova_retention_04 <- aov(Retention_Score ~ `Engaging/Dull` + Prior_knowledge + `First Year or Not?`, data = filtered_data_4)
summary(anova_retention_04)
anova_engagement_04 <- aov(Engaging_Score ~ `Engaging/Dull` + Prior_knowledge + `First Year or Not?`, data = filtered_data_4)
summary(anova_engagement_04)

#5 Dull Teaching Method & Not Having Prior Knowledge & Not First year (senior)
filtered_data_5 <- subset(final_data, `Engaging/Dull` == 0 & Prior_knowledge == 0 & `First Year or Not?` == 0 )
View(filtered_data_5)
anova_retention_05 <- aov(Retention_Score ~ `Engaging/Dull` + Prior_knowledge + `First Year or Not?`, data = filtered_data_5)
summary(anova_retention_05)
anova_engagement_05 <- aov(Engaging_Score ~ `Engaging/Dull` + Prior_knowledge + `First Year or Not?`, data = filtered_data_5)
summary(anova_engagement_05)

#6 Dull Teaching Method & Not Having Prior Knowledge & First year
filtered_data_6 <- subset(final_data, `Engaging/Dull` == 0 & Prior_knowledge == 0 & `First Year or Not?` == 1 )
View(filtered_data_6)
anova_retention_06 <- aov(Retention_Score ~ `Engaging/Dull` + Prior_knowledge + `First Year or Not?`, data = filtered_data_6)
summary(anova_retention_06)
anova_engagement_06 <- aov(Engaging_Score ~ `Engaging/Dull` + Prior_knowledge + `First Year or Not?`, data = filtered_data_6)
summary(anova_engagement_06)

#7 Dull Teaching Method & Having Prior Knowledge & Not First year(senior)
filtered_data_7 <- subset(final_data, `Engaging/Dull` == 0 & Prior_knowledge == 1 & `First Year or Not?` == 0 )
View(filtered_data_7)
anova_retention_07 <- aov(Retention_Score ~ `Engaging/Dull` + Prior_knowledge + `First Year or Not?`, data = filtered_data_7)
summary(anova_retention_07)
anova_engagement_07 <- aov(Engaging_Score ~ `Engaging/Dull` + Prior_knowledge + `First Year or Not?`, data = filtered_data_7)
summary(anova_engagement_07)

#8 Engaging Teaching Method & Not Having Prior Knowledge & Not First year(senior)
filtered_data_8 <- subset(final_data, `Engaging/Dull` == 1 & Prior_knowledge == 0 & `First Year or Not?` == 0 )
View(filtered_data_8)
anova_retention_08 <- aov(Retention_Score ~ `Engaging/Dull` + Prior_knowledge + `First Year or Not?`, data = filtered_data_8)
summary(anova_retention_08)
anova_engagement_08 <- aov(Engaging_Score ~ `Engaging/Dull` + Prior_knowledge + `First Year or Not?`, data = filtered_data_8)
summary(anova_engagement_08)

# Visualizations ----------------------------------------------------------
# Histogram for Engaging Score
ggplot(final_data, aes(x = Engaging_Score)) +
  geom_histogram(binwidth = 1, fill = "forestgreen", color = "black", alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Distribution of Engaging Score",
    x = "Engaging Score",
    y = "Number of Students"
  )
# Histogram for Retention Score
ggplot(final_data, aes(x = Retention_Score)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Distribution of Retention Score",
    x = "Retention Score",
    y = "Number of Students"
  )

#1) For Student Engagement Score Dependent Variable

# Bar plot of Student_Engagement_Score by Engaging/Dull
ggplot(final_data, aes(x = `Engaging/Dull`, y = Engaging_Score, fill = factor(`Engaging/Dull`))) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Student Engagement Score by Teaching Method", 
       x = "Engaging/Dull", 
       y = "Mean Student Engaging Score",
       fill = "Type of Video") + # Custom legend title
  scale_fill_manual(values = c("0" = "orange", "1" = "black"), # Customize colors
                    labels = c("0" = "Dull", "1" = "Engaging")) +
  theme_minimal()
#This Shows the average student engagement scores for Engaging and Dull teaching types, highlighting the significant effect of engagement.

# Bar plot of Student_Engagement_Score by Prior Knowledge
ggplot(final_data, aes(x = Prior_knowledge, y = Engaging_Score, fill = factor(Prior_knowledge))) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Student Engagement Score by Prior Knowledge", 
       x = "Prior_knowledge", 
       y = "Mean Student Engaging Score",
       fill = "Prior Knowledge") + # Custom legend title
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), # Customize colors
                    labels = c("0" = "No", "1" = "Yes")) +
  theme_minimal()

# Bar plot of Student_Engagement_Score by First year or senior
ggplot(final_data, aes(x = `First Year or Not?`, y = Engaging_Score, fill = factor(`First Year or Not?`))) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Student Engagement Score by First Year or Senior", 
       x = "Senior or First Year", 
       y = "Mean Student Engaging Score",
       fill = "First Year or Senior") + # Custom legend title
  scale_fill_manual(values = c("0" = "blue", "1" = "green"), # Customize colors
                    labels = c("0" = "Senior", "1" = "First year")) +
  theme_minimal()


# Boxplot for Engaging/Dull and Prior_knowledge interaction

ggplot(final_data, aes(x = Prior_knowledge, y = `Engaging_Score`, fill = factor(`Engaging/Dull`))) +
  geom_boxplot() +
  labs(
    title = "Student Engagement Score by Prior Knowledge and Teaching Method", 
    x = "Prior Knowledge", 
    y = "Student Engagement Score",
    fill = "Teaching Method" # Custom legend title
  ) +
  scale_fill_manual(
    values = c("0" = "#FFC0CB", "1" = "#FF1599"), # Customize colors
    labels = c("0" = "Dull", "1" = "Engaging") # Custom labels
  ) +
  facet_wrap(~ `Engaging/Dull`) +
  theme_minimal()
#This Provides a detailed breakdown of student engagement scores for combinations of Engaging/Dull(Teaching Method) and Prior_knowledge, helping to identify trends and variations.


#2) For Retention Test Score Dependent Variable

# Bar plot for Engaging/Dull
ggplot(final_data, aes(x = `Engaging/Dull`, y = Retention_Score, fill = factor(`Engaging/Dull`))) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Retention Test Score by Teaching Method", 
       x = "Engaging/Dull", 
       y = "Mean Retention Test Score",
  fill = "Prior Knowledge") + # Custom legend title
  scale_fill_manual(values = c("0" = "darkred", "1" = "yellow"), # Customize colors
                    labels = c("0" = "Dull", "1" = "Engaging")) +
  theme_minimal()

# Bar plot for Prior Knowledge
ggplot(final_data, aes(x = factor(Prior_knowledge), y = Retention_Score, fill = factor(Prior_knowledge))) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Retention Test Score by Prior Knowledge", 
       x = "Prior Knowledge", 
       y = "Mean Retention Test Score",
       fill = "Prior Knowledge") + # Custom legend title
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), # Customize colors
                    labels = c("0" = "No Prior Knowledge", "1" = "Has Prior Knowledge")) +
  theme_minimal()
# Bar plot for First Year or Not?
  ggplot(final_data, aes(x = factor(`First Year or Not?`), y = Retention_Score, fill = factor(`First Year or Not?`))) +
    geom_bar(stat = "summary", fun = "mean") +
    labs(title = "Retention Test Score by Student's Year of Study", 
         x = "First Year or Senior?", 
         y = "Mean Retention Test Score",
         fill = "Year of Study") + # Update legend title
    scale_fill_manual(values = c("0" = "blue", "1" = "green"), # Customize colors if needed
                      labels = c("0" = "Not First Year", "1" = "First Year")) +
    theme_minimal()
  #heat map
  # Ensure variables are treated as factors with proper levels
  final_data$Prior_knowledge <- factor(final_data$Prior_knowledge, levels = c(0, 1))
  final_data$`First Year or Not?` <- factor(final_data$`First Year or Not?`, levels = c(0, 1))
  
  # Generate Heatmap for Engaging Score
  ggplot(heatmap_data, aes(x = Prior_knowledge, y = `First Year or Not?`, fill = Mean_Engaging_Score)) +
    geom_tile(color = "white") +
    facet_wrap(~ `Engaging/Dull`, labeller = labeller(`Engaging/Dull` = c(`0` = "Dull", `1` = "Engaging"))) +
    scale_x_discrete(name = "Prior Knowledge", limits = c("No prior Knowledge", "has prior Knowledge")) +  # Ensure only 0 and 1 appear on x-axis
    scale_y_discrete(name = "First Year or Not?", limits = c("not first year", "first year")) +  # Ensure only 0 and 1 appear on y-axis
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    labs(
      title = "Heatmap of Engaging Score",
      fill = "Mean Engaging Score"
    ) +
    theme_minimal()+
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), # Center and format title
      axis.title.x = element_text(size = 12, face = "bold"),           # Format x-axis title
      axis.title.y = element_text(size = 12, face = "bold"),           # Format y-axis title
      axis.text.x = element_text(size = 10, angle = 20, hjust = 1),    # Adjust x-axis text angle for better fit
      axis.text.y = element_text(size = 10, hjust = 0.5),              # Center-align y-axis text vertically
      strip.text = element_text(size = 12, face = "bold"),             # Format facet labels
      legend.title = element_text(size = 10, face = "bold"),           # Format legend title
      legend.text = element_text(size = 9),                            # Format legend text
      panel.spacing = unit(0, "lines"),                                # Remove spacing between facets
      panel.grid = element_blank(),                                    # Remove gridlines
      plot.margin = margin(t = 5, r = 5, b = 5, l = 5)                 # Reduce plot margins
    )
  # Generate Heatmap for Retention Score with Adjusted Y-Axis Label Alignment
  ggplot(heatmap_data, aes(x = Prior_knowledge, y = `First Year or Not?`, fill = Mean_Retention_Score)) +
    geom_tile(color = "white") +
    facet_wrap(
      ~ `Engaging/Dull`,
      labeller = labeller(`Engaging/Dull` = c(`0` = "Dull", `1` = "Engaging"))
    ) +
    scale_x_discrete(
      name = "Prior Knowledge",
      limits = c("no prior knowledge", "has prior knowledge")
    ) +
    scale_y_discrete(
      name = "First Year or Not?",
      limits = c("Not First Year", "First Year")
    ) +
    scale_fill_gradient(
      low = "lightyellow",
      high = "darkred",
      name = "Mean Retention Score"
    ) +
    labs(
      title = "Heatmap of Retention Score"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), # Center and format title
      axis.title.x = element_text(size = 12, face = "bold"),           # Format x-axis title
      axis.title.y = element_text(size = 12, face = "bold"),           # Format y-axis title
      axis.text.x = element_text(size = 10, angle = 20, hjust = 1),    # Adjust x-axis text angle for better fit
      axis.text.y = element_text(size = 10, hjust = 0.5),              # Center-align y-axis text vertically
      strip.text = element_text(size = 12, face = "bold"),             # Format facet labels
      legend.title = element_text(size = 10, face = "bold"),           # Format legend title
      legend.text = element_text(size = 9),                            # Format legend text
      panel.spacing = unit(0, "lines"),                                # Remove spacing between facets
      panel.grid = element_blank(),                                    # Remove gridlines
      plot.margin = margin(t = 5, r = 5, b = 5, l = 5)                 # Reduce plot margins
    )
  
  
  
 