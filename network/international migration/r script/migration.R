library(tidyverse)
library(tidygraph)
library(ggraph)
library(dplyr)
library(writexl)
library(readr)
library(readxl)
df <- read_excel("/Users/mac/Desktop/Master /intern/dataset/network/international migration/original/undesa_pd_2024_ims_stock_by_sex_destination_and_origin.xlsx", 
                 sheet = "Table 1", range = "A11:O24081")
View(df)

df<- df%>%select(all_of(c(2,6,8,9,10,11,12,13,14,15)))
names(df)
# Create the new dataset 'df' by renaming and then selecting the columns
df_<-df%>%
  rename_with(~case_when(
    .x== "Region, development group, country or area of destination" ~ "destination",
    .x== "Region, development group, country or area of origin" ~ "origin",
    .x== "1990"~ "in 1990",
    .x== "1995"~ "in 1995",
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
     
     " Falkland Islands (Malvinas)*","Réunion", "Western Sahara","State of Palestine","China, Taiwan Province of China","Martinique*",
      "Polynesia*","Wallis and Futuna Islands*","Micronesia" ,"Guadeloupe*",
     "China, Macao SAR","Holy See*", "Anguilla*", "Tokelau*", "Falkland Islands (Malvinas)*",    
     "Saint Helena","French Guiana*","Cook Islands*","Mayotte","Bonaire, Sint Eustatius and Saba*",
     "Montserrat*","Saint Pierre and Miquelon*"    )
# 3. Filter the data to remove these names from both columns
 df_c <- df_%>%
    filter(!origin %in% aggregates_to_remove) %>%
     filter(!destination %in% aggregates_to_remove)
View(df_c)

df_c = df_c[,c(2,1,3,4,5,6,7,8,9,10)]
df_c




# Check the entire dataframe
any(is.na(df_c))

# Remove rows where 'column_name' has NaN
df_c$`in 1990`[is.na(df_c$`in 1990`)] <- 0

##########################change names 


## Define the mapping (Source Name = "Target Name")
# This matches the names in your CSV to the names in the Population file
country_mapping <- c(
  "United Republic of Tanzania"        = "Tanzania",
  "Democratic Republic of the Congo"   = "Congo, Dem. Rep.",
  "France*"                            = "France",
  "Somalia"                            = "Somalia, Fed. Rep.",
  "Yemen"                              = "Yemen, Rep.",
  "United Kingdom*"                    = "United Kingdom",
  "Congo"                              = "Congo, Rep.",
  "China, Hong Kong SAR"               = "Hong Kong SAR, China",
  "Spain*"                             = "Spain",
  "United States of America*"          = "United States",
  "Australia*"                         = "Australia",
  "Egypt"                              = "Egypt, Arab Rep.",
  "Côte d'Ivoire"                      = "Cote d'Ivoire",
  "Netherlands*"                       = "Netherlands",
  "Türkiye"                            = "Turkiye",
  "Denmark*"                           = "Denmark",
  "Dem. People's Republic of Korea"    = "Korea, Dem. People's Rep.",
  "Republic of Korea"                  = "Korea, Rep.",
  "Lao People's Democratic Republic"   = "Lao PDR",
  "Ukraine*"                           = "Ukraine",
  "Finland*"                           = "Finland",
  "Norway*"                            = "Norway",
  "Iran (Islamic Republic of)"         = "Iran, Islamic Rep.",
  "New Zealand*"                       = "New Zealand",
  "Gambia"                             = "Gambia, The",
  "Bahamas"                            = "Bahamas, The",
  "Slovakia"                           = "Slovak Republic",
  "Serbia*"                            = "Serbia",
  "British Virgin Islands*"            = "British Virgin Islands",
  "Venezuela (Bolivarian Republic of)" = "Venezuela, RB",
  "American Samoa*"                    = "American Samoa",
  "United States Virgin Islands*"      = "Virgin Islands (U.S.)",
  "Republic of Moldova*"               = "Moldova",
  "Saint Vincent and the Grenadines"   = "St. Vincent and the Grenadines",
  "Bolivia (Plurinational State of)"   = "Bolivia",
  "Micronesia (Fed. States of)"        = "Micronesia, Fed. Sts.",
  "Turks and Caicos Islands*"          = "Turks and Caicos Islands",
  "French Polynesia*"                  = "French Polynesia",
  "Kyrgyzstan"                         = "Kyrgyz Republic",
  "Gibraltar*"                         = "Gibraltar",
  "Puerto Rico*"                       = "Puerto Rico (US)",
  "Isle of Man*"                       = "Isle of Man",
  "Aruba*"                             = "Aruba",
  "Cayman Islands*"                    = "Cayman Islands",
  "Bermuda*"                           = "Bermuda",
  "Greenland*"                         = "Greenland",
  "Faroe Islands*"                     = "Faroe Islands",
  "Saint Kitts and Nevis"              = "St. Kitts and Nevis",
  "Saint Lucia"                        = "St. Lucia",
  "New Caledonia*"                     = "New Caledonia",
  "Curaçao*"                           = "Curacao",
  "Channel Islands*"                   = "Channel Islands",
  "Guam*"                              = "Guam",
  "Northern Mariana Islands*"          = "Northern Mariana Islands",
  "Sint Maarten (Dutch part)*"         = "Sint Maarten (Dutch part)"
)

# 3. Apply the mapping to both 'origin' and 'destination' columns
# Note: recode() will leave any names NOT in the mapping list unchanged.
df_s <- df_c %>%
  mutate(
    origin = recode(origin, !!!country_mapping),
    destination = recode(destination, !!!country_mapping)
  )

write.csv(df_s, "/Users/mac/Desktop/Master /intern/dataset/network/international migration/csv/selected_countries_dataset.csv", row.names = FALSE)

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
