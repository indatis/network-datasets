setwd("/Users/mac/Desktop/Master /intern/dataset/network/international migration/")
library(readxl)
library(dplyr)

df<-read_excel("original/total population.xls", 
               sheet = "Data", range = "A4:BQ270")

##from the data remove unwanted column
df<-df%>% select(-(2:34))

### then remove rows that does not contain countries
df$`Country Name`
not_contry<- c("Africa Eastern and Southern" ,"Africa Western and Central",
               "Central Europe and the Baltics","East Asia & Pacific (excluding high income)" ,
               "Early-demographic dividend","East Asia & Pacific","Europe & Central Asia (excluding high income)",                            
                "Europe & Central Asia", "Euro area","European Union" ,"Fragile and conflict affected situations",
               "High income","Heavily indebted poor countries (HIPC)","IBRD only","IDA & IBRD total","IDA total",
               "IDA blend","IDA only","Not classified" ,"Latin America & Caribbean (excluding high income)" ,
               "Latin America & Caribbean","Least developed countries: UN classification" ,                            
               "Low income", "Lower middle income","Low & middle income","Late-demographic dividend",
               "Macao SAR, China","Middle East, North Africa, Afghanistan & Pakistan","Middle income",
               "Middle East, North Africa, Afghanistan & Pakistan (excluding high income)","North America",
               "OECD members","Other small states","Pre-demographic dividend","West Bank and Gaza","Pacific island small states",
               "Post-demographic dividend","South Asia","Sub-Saharan Africa (excluding high income)","Sub-Saharan Africa",
               "East Asia & Pacific (IDA & IBRD countries)","Europe & Central Asia (IDA & IBRD countries)",
               "Latin America & the Caribbean (IDA & IBRD countries)","Middle East, North Africa, Afghanistan & Pakistan (IDA & IBRD)",
               "South Asia (IDA & IBRD)" ,"Sub-Saharan Africa (IDA & IBRD countries)","Upper middle income",
               "World","Arab World","Caribbean small states","St. Martin (French part)","Small states","Kosovo"  )

##remove
df <- df %>% 
  filter(!`Country Name` %in% not_contry)

##check for missing values
any(is.na(df))

write.csv(df,"csv/total population.csv", row.names = FALSE)


################## compare countries names with selected country csv
# Load the datasets
selected_countries <-  read_csv("~/Desktop/Master /intern/dataset/network/international migration/csv/selected_countries_dataset.csv")
total_population <- read_csv("~/Desktop/Master /intern/dataset/network/international migration/csv/total population.csv")
head(total_population)

# 1. Extract the unique sets of names
# Use backticks ` ` for "Country Name" because of the space
origin_set <- unique(selected_countries$origin)
population_set <- unique(total_population$`Country Name`)

# 2. Find countries in ORIGIN that are missing from POPULATION
# (These are names you need to fix in your origin list)
missing_from_population <- setdiff(origin_set, population_set)

# 3. Find countries in POPULATION that are missing from ORIGIN
# (These might be names with different spelling in the population file)
missing_from_origin <- setdiff(population_set, origin_set)

# --- Print Results ---

print("Countries in your data but NOT in the population file:")
print(missing_from_population)

print("Countries in the population file but NOT in your data:")
print(missing_from_origin)

###### then replace countries names from total population 
##to the migration dataset this process done in the migration r script
