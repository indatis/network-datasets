library(tidyverse)
library(tidygraph)
library(ggraph)
library(dplyr)
library(writexl)
library(readr)
library(readxl)
df <- read_excel("network/international migration/original/undesa_pd_2024_ims_stock_by_sex_destination_and_origin.xlsx", 
                 sheet = "Table 1", range = "A11:O24081")
View(df)

df<- df%>%select(all_of(c(2,6,8,10,11,12,13,14,15)))
names(df)
# Create the new dataset 'df' by renaming and then selecting the columns
df_<-df%>%
  rename_with(~case_when(
    .x== "Region, development group, country or area of destination" ~ "destination",
    .x== "Region, development group, country or area of origin" ~ "origin",
    .x== "1990"~ "in 1990",
    .x== "2000"~ "in 2000",
    .x==  "2005"~ "in 2005",
    .x== "2010"~ "in 2010",
    .x== "2015"~ "in 2015",
    .x=="2020"~ "in 2020",
    .x=="2024"~ "in 2024",
    TRUE ~ .x
  ))
names(df_)
 # 2. Define the comprehensive list of aggregates to be removed
 aggregates_to_remove <- c(
     "World", 
       "Sub-Saharan Africa", 
       "Northern Africa and Western Asia", 
       "Central and Southern Asia", 
      "Eastern and South-Eastern Asia", 
       "Latin America and the Caribbean", 
      "Oceania (excluding Australia and New Zealand)", 
      "Australia/New Zealand", 
       "Europe and Northern America", 
       "More developed regions", 
       "Less developed regions", 
       "Least developed countries", 
      "Less developed regions, excluding least developed countries", 
       "Less developed regions, excluding China", 
       "Land-locked Developing Countries (LLDC)", 
       "Small Island Developing States (SIDS)", 
       "High-and-upper-middle-income countries", 
       "Low-and-Lower-middle-income countries", 
     "High-income countries", 
       "Low-and-middle-income countries",
       "Low-income countries",
      "Middle-income countries",
       "Upper-middle-income countries",
       "Lower-middle-income countries",
       "AFRICA", "ASIA", "EUROPE", "LATIN AMERICA AND THE CARIBBEAN", 
      "NORTHERN AMERICA", "OCEANIA",
       "Eastern Africa", "Middle Africa", "Southern Africa", "Western Africa",
       "Caribbean", "Central America", "South America",
       "Central Asia", "Southern Asia", "South-Eastern Asia", "Eastern Asia", "Western Asia",
       "Eastern Europe", "Northern Europe", "Southern Europe", "Western Europe",
       "Melanesia", "Polynesia","No income group available","Others","Northern Africa" ,
     " Falkland Islands (Malvinas)*" )
# 3. Filter the data to remove these names from both columns
 df_c <- df_%>%
    filter(!origin %in% aggregates_to_remove) %>%
     filter(!destination %in% aggregates_to_remove)
View(df_c)
df_c = df_c[,c(2,1,3,4,5,6,7,8,9)]
df_c
write.csv(df_c, "selected_countries_dataset.csv", row.names = FALSE)

######ploting the graph
graph <- as_tbl_graph(df_c, directed = TRUE)
print(graph)
# Visualize the network


ggraph(graph, layout = 'kk') + 
  geom_edge_link(aes(width = `in 2020`), 
                 arrow = arrow(length = unit(3, 'mm')), 
                 end_cap = circle(3, 'mm'), 
                 color = "steelblue",
                 alpha = 0.5) + 
  geom_node_point(size = 2, color = "red") + 
  geom_node_text(aes(label = name), repel = TRUE, size = 2) +
  scale_edge_width(range = c(0.2, 4)) + 
  theme_void() +
  labs(title = "Major Migration Flows in ",
       edge_width = "Number of Migrants")
