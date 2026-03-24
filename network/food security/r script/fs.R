library(tidyverse)
library(tidygraph)
library(ggraph)
library(readr)
library(dplyr)
fs <- read_csv("Desktop/Master /intern/dataset/food security/food security in eu.csv")
View(fs)

library(dplyr)
names(fs)

fs_<-fs%>% select(all_of(c(4,6,7,8,10,11,12)))
# Create the new dataset 'fs' by renaming and then selecting the columns
fs_un <- fs_ %>%
  rename_with(~ case_when(
    .x == "ReporterName"                    ~ "origin",
    .x == "PartnerName"            ~ "destination",
    .x == "TradeValue in 1000 USD" ~ "value",
    .x == "Year"                   ~ "year",
    .x == "ProductDescription"     ~ "prod_desc",
    .x == "TradeFlowName"          ~ "flow_type",
    .x == "Quantity"               ~ "qty",
    TRUE ~ .x
  ))
  
# View the result
head(fs_un)

############ separate by flow type
fs_e<- fs_un%>% filter(flow_type== "Export")
fs_i<- fs_un%>% filter(flow_type== "Import")


#######################EXPORT ######################
### starting by adding three food type and having total 

# 1. Define the items you want to add
target_foods <- c("Meat and edible offal, of the poult", "Rice.", "Wheat and meslin.")

# 2. Filter for those foods and sum their values by country and year
fs_ex<- fs_e %>%
  filter(prod_desc %in% target_foods) %>%
  group_by(origin,destination) %>%
  summarise(total_food_value = sum(value, na.rm = TRUE))



# View the result
head(fs_ex)
View(fs_ex)

#### extract the final data
write.csv(fs_ex, "food securty export.csv", row.names = FALSE)


#######################IMPORT ######################
### starting by adding three food type and having total 

# 1. Define the items you want to add
target_foods <- c("Meat and edible offal, of the poult", "Rice.", "Wheat and meslin.")

# 2. Filter for those foods and sum their values by country and year
fs_im<- fs_i %>%
  filter(prod_desc %in% target_foods) %>%
  group_by(origin,destination) %>%
  summarise(total_food_value = sum(value, na.rm = TRUE))

table(fs_im$destination)
View(fs_i)
# View the result
head(fs_im)
View(fs_im)

#### extract the final data
write.csv(fs_im, "food securty import.csv", row.names = FALSE)


############################### visualization for both########################@
# Create the graph object
# It uses 'origin' as the start, 'destination' as the end, and 'total_food_value' as the weight
graph <- as_tbl_graph(fs_im, directed = TRUE)
print(graph)
# Visualize the network
ggraph(graph, layout = 'kk') + 
  geom_edge_link(aes(width = total_food_value, alpha = total_food_value), 
                 arrow = arrow(length = unit(3, 'mm')), 
                 end_cap = circle(3, 'mm'), 
                 color = "steelblue") + 
  geom_node_point(size = 4, color = "red") +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_edge_width(range = c(0.2, 4)) + # Controls how thick the lines get
  theme_void() +
  labs(title = "Combined import Food Trade Network",
       subtitle = "Visualizing total trade value between countries"
       #       edge_width = "Total Value"
       )
