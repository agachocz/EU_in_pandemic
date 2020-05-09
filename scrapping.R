install.packages("rvest")
library(rvest)

url <- "https://www.worldometers.info/coronavirus/"
source <- read_html(url)

table <- html_nodes(source, "#main_table_countries_today") %>% html_table()
table <- table[[1]]
names(table)[1] <- "Country"
names(table)[12] <- "Tests"

wm_data <- table %>% filter(Country %in% country_name) %>% 
            select(Country, Tests)

write.table(wm_data, "03_05_tests.csv", sep = ";") # Tests per 1M
