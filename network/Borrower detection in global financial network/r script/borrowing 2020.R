library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(tidygraph)
library(ggraph)
df<- read_csv("network/Borrower detection in global financial network/original/borrowing 2020.csv")
head(df)
View(df)
names(df)

df<- df%>%select(all_of(c(1, 2,3,4,5,6)))


# 1. Fix the headers (Move row 2 to the header and remove metadata rows)
colnames(df) <- df[2, ]         # Set column names to the values in row 2
df_clean <- df[-c(1,2,462),]      # Remove the first two rows (metadata and headers)
head(df_clean)
names(df_clean)
# 2 . Rename using exact column names (use backticks for names with spaces)
bf <- df_clean %>%
  rename(
    origin = `Reporting country`,
    destination = `Counterparty country`,
    `2020_Q3` = `2020-09-30`,
    `2020_Q2` = `2020-06-30`,
    `2020_Q1` = `2020-03-31`,
    `2020_Q4` = `2020-12-31`
  )
View(bf)


# 2. Fix the NA error by converting value columns to numeric first
bf <- bf %>%
  mutate(
    # Keep origin and destination as text
    origin = replace_na(origin, "0"),
    destination = replace_na(destination, "0"),
    
    # Convert date columns to numeric AND then replace NA with 0
    # Note: R will give a "NAs introduced by coercion" warning—this is fine!
    # it just means it's turning text like ".." into NA so we can make them 0.
    `2020_Q3` = replace_na(as.numeric(`2020_Q3`), 0),
    `2020_Q2` = replace_na(as.numeric(`2020_Q2`), 0),
    `2020_Q1` = replace_na(as.numeric(`2020_Q1`), 0),
    `2020_Q4` = replace_na(as.numeric(`2020_Q4`), 0)
  )

# Verify the result
head(bf)



write.csv(bf,"network/Borrower detection in global financial network/csv/borrowing data 2020.csv", row.names = FALSE)

############################### visualization for both########################@
# Create the graph object
# It uses 'origin' as the start, 'destination' as the end, and 'total_food_value' as the weight
graph <- as_tbl_graph(bf, directed = TRUE)
print(graph)
# Visualize the network
ggraph(graph, layout = 'kk') + 
  geom_edge_link(aes(width = `2020_Q4`, alpha = `2020_Q4`), 
                 arrow = arrow(length = unit(3, 'mm')), 
                 end_cap = circle(3, 'mm'), 
                 color = "steelblue") + 
  geom_node_point(size = 4, color = "red") +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_edge_width(range = c(0.2, 4)) + # Controls how thick the lines get
  theme_void() +
  labs(title = "Borrowing Detetion in the Global Finance Network",
       subtitle = "Visualizing Borrowing network in forth quater"
       #       edge_width = "amount"
  )

