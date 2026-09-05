#_____________________ Data analysis by R __________________________#
##########################################################
########## First read the data & data preperation ########
##########################################################
library(readr)
df <- read.csv("world_population.csv")

head(df) #print the head of your data
str(df) #info of yur data
summary(df) #statistics of your data_set
write.csv("write the output") #used to export your file

###############################################################
########## Second filter and select from your data set ########
###############################################################
library(dplyr)

#Select function used for slect specific columns
new_data <- select(df,Country,Capital,Continent)
delet_example <- select(df,-CCA3)

#Filter function used to older specific rows
filtered_data <- filter(df,Country == "Belize")
second_filter <- filter(df,Growth.Rate > 1.00)

#Arrange function work as order by
arrage_fun <- arrange(df,desc(Country))

# %>% operation make the process more faster 
df_new <- df %>% select(Rank:X1970.Population) %>% 
     filter(X1970.Population>6000000) %>% 
      arrange(Rank)
###################################################
###############Group_by function###################
###################################################
Select_data=df %>% group_by(Country) %>% 
                     summarize(count=n(),mean_populution_2022=mean(X2022.Population))

##################################################
#############Handling missing values##############
##################################################
library(tidyverse)

#Find all missing value for a columns
colSums(is.na(df))

#Handl the missing data

#Drop rows with nan
clean_df <- df %>% drop_na("Growth.Rate")

#Fell na by 
clean_df$X2022.Population[is.na(clean_df$X2022.Population)]<- 
  mean(clean_df$X2022.Population,na.rm = T)

#For loop for cleaning missing data
clean_df <- clean_df %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

head(clean_df) #first 5 rows
str(clean_df) #info of your data
summary(clean_df) #Statistics analysis

colSums(is.na(clean_df))

##############################################
#################Remove Duplicate#############
##############################################
no_duplicate_df<- clean_df %>%
                     distinct() #That mean there no duplicates

##############################################
################Data Visualization############
##############################################
library(ggplot2)

#Bar chart
clean_df %>% 
  group_by(Continent) %>% 
   summarise(mean_Population_at_2022=mean(X2022.Population)) %>% 
  ggplot(aes(x = reorder(Continent,-mean_Population_at_2022),
             y = mean_Population_at_2022,fill = Continent))+
  geom_bar(stat="identity")+
  ggtitle("Mean population at 2022")+
  theme(axis.text.x = element_text(angle = 45.,hjust = 1))

#bar plot
clean_df %>%
  slice_max(order_by = X2022.Population, n = 10) %>%
  ggplot(aes(x = reorder(Country, X2022.Population), y = X2022.Population / 1e6, fill = Continent)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Top 10 Countries by 2022 Population",
       x = "Country",
       y = "Population (Millions)") +
  theme_minimal()

#Stander your data

# 2. Convert all text columns to clean UTF-8
 clean_df %>%
  mutate(across(where(is.character), ~ iconv(.x, from = "", to = "UTF-8", sub = "")))

df_long <- clean_df %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Year",
    values_to = "Population"
  ) %>%
  filter(grepl("Population", Year)) %>%
  mutate(Year = as.numeric(gsub("[^0-9]", "", Year)))

#PLot line chart
df_long %>%
  group_by(Continent, Year) %>%
  summarise(Total_Pop = sum(Population, na.rm = TRUE) / 1e6) %>%
  ggplot(aes(x = Year, y = Total_Pop, color = Continent, group = Continent)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Population Growth by Continent (1970 - 2022)",
       x = "Year",
       y = "Total Population (Millions)") +
  theme_minimal()

#tree map chart
library(treemapify)
ggplot(clean_df, aes(area = World.Population.Percentage, fill = Continent, label = Country)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "center", grow = TRUE, reflow = TRUE) +
  labs(title = "Share of World Population by Country and Continent") +
  theme(legend.position = "bottom")