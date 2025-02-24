
library(ggplot2)
library(dplyr)
library(tidyverse)


EducationData$Value <- as.numeric(EducationData$Value)


#Sketch 1
EducationData_summary <- EducationData %>%
  group_by(State) %>%
  summarise(Total_Absenteeism = sum(Value, na.rm = TRUE)) %>%
  ungroup() 

print(EducationData_summary)

Plot1 <- ggplot(EducationData_summary, aes(x = reorder(State, -Total_Absenteeism), y = Total_Absenteeism)) +
  geom_bar(stat = "identity", fill = "#900C3F") +
  labs(title = "Total Chronic Absenteeism by State (2022-2023)",
       x = "State",
       y = "Total Absenteeism") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "Sketch 1.png", plot = Plot1, width = 10, height = 6, dpi = 300)

#Sketch 2

EducationData$Value <- as.numeric(EducationData$Value)

yearly_absenteeism <- EducationData %>%
  group_by(`School Year`) %>%
  summarize(Total_Absentees = sum(Value, na.rm = TRUE)) # Sum values, removing NA

yearly_absenteeism$Total_Absentees_Thousands <- yearly_absenteeism$Total_Absentees / 1000

Plot2 <- ggplot(yearly_absenteeism, aes(x = Total_Absentees_Thousands, y = `School Year`)) +
  geom_col() + 
  labs(title = "Total Chronic Absenteeism by School Year",
       x = "Total Number of Chronically Absent Students (in thousands)",
       y = "School Year") +
  theme_minimal()

ggsave(filename = "Sketch 2.png", plot = Plot2, width = 10, height = 6, dpi = 300)

#Sketch 3:

library(scales)

EducationData$Value <- as.numeric(EducationData$Value)

subgroup_schoolyear_absenteeism <- EducationData %>%
  group_by(Subgroup, `School Year`) %>%
  summarize(Total_Absentees = sum(Value, na.rm = TRUE))

Plot3 <- ggplot(subgroup_schoolyear_absenteeism, aes(x = `School Year`, y = Subgroup, fill = Total_Absentees)) +
  geom_tile() +
  scale_fill_gradient(low = "#8f95b2", high = "#06155d", labels = number_format(accuracy = 1, big.mark = ",")) +  
  labs(title = "Chronic Absenteeism by Subgroup and School Year",
       x = "School Year",
       y = "Subgroup",
       fill = "Total Absentees") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

ggsave(filename = "Sketch 3.png", plot = Plot3, width = 10, height = 6, dpi = 300)

#Final Plot

install.packages('viridis')
install.packages("maps")
library(plotly)
library(maps)
library(tidyverse)
library(scales)

EducationData$Value <- as.numeric(EducationData$Value)
EducationData <- EducationData %>%
  rename(region = State)


state_gender_absenteeism <- EducationData %>%
  group_by(region, Characteristics) %>%  #doing this because I will convert it to plotly
  summarize(Total_Absentees = sum(Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Characteristics, values_from = Total_Absentees, values_fill = 0) 

#Adding the map for better visualization
us_states <- map_data("state")

#my data and the data in the maps had different types of letters for states so I changed it first and then made all lowercase
us_states$region <- tolower(us_states$region)
state_gender_absenteeism$region <- tolower(state_gender_absenteeism$region)

map_data <- merge(us_states, state_gender_absenteeism, by = "region", all.x = TRUE)


map_data$Female[is.na(map_data$Female)] <- 0
map_data$Male[is.na(map_data$Male)] <- 0

map_data <-  map_data %>%
  mutate(Total_Absentees = Female + Male) %>%
  mutate(Total_Absentees_TenThousands = Total_Absentees / 10000)


FinalPlot1 <- ggplot(map_data, aes(x = long, y = lat, group = group, fill = Total_Absentees_TenThousands,
                          text = paste("State:", region, "Total Female Absentees:", Female, "Total Male Absentees:", Male))) +
  geom_polygon(color = "black", linewidth = 0.1) +
  scale_fill_gradient(low = "#94a1d0", high = "#152976", labels = comma) +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  labs(title = "Chronic Absenteeism by State (in 10,000s)",
       fill = "Total Absentees (10,000s)") +
  theme_minimal() + 
  theme(panel.background = element_blank(), 
        axis.line = element_blank(),
        axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        axis.ticks.x = element_blank(),  
        axis.ticks.y = element_blank())   


FinalPlot1 <- ggplotly(g, tooltip = "text")

#for some reason my plot was not showing up.
print(FinalPlot1)

htmlwidgets::saveWidget(FinalPlot1, "Final_plot.html")



