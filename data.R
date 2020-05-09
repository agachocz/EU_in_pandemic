library(dplyr)

# DATA

# cases

country_name <- c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland",
                  "France", "Germany", "Greece", "Hungary", "Italy", "Latvia", "Lithuania", "Luxembourg",
                  "Ireland", "Netherlands", "Poland", "Portugal", "Slovakia", "Slovenia", "Spain",
                  "Switzerland", "Great Britain", "Malta", "Romania")


# gov. policies

# wskaźnik surowości zarządzeń

stringency_index <- read.csv("policies/stringencyindex.csv", na.strings = ".") %>% 
  filter(X %in% country_name)
col_names <- names(stringency_index)[-c(1,2)]

lct <- Sys.getlocale("LC_TIME"); 
Sys.setlocale("LC_TIME", "C")

library(stringr)

stringency_data <- stringency_index %>% 
  pivot_longer(col_names, names_to = "date", values_to = "stringency") %>%
  select(country = X, date, stringency) %>%
  mutate(date = str_replace(date, "jan", "01")) %>% mutate(date = str_replace(date, "feb", "02")) %>%
  mutate(date = str_replace(date, "mar", "03")) %>% mutate(date = str_replace(date, "apr", "04")) %>%
  mutate(date = str_replace(date, "may", "05")) %>%
  mutate(date = paste0(substr(date,6,9),"-",substr(date,4,5),"-",substr(date,2,3))) %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  filter(date < "2020-05-01")

write.table(stringency_data, "stringency_data.csv")

stringency_data <- read.table("stringency_data.csv")

policies_all <- read.csv("policies/all.csv") %>% 
  filter(CountryName %in% country_name) %>% mutate(Date = paste0(substr(Date,1,4),"-",
    substr(Date,5,6),"-",substr(Date,7,8))) %>%
  mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
  filter(Date < "2020-05-01")
   # dalej są puste kolumny
# brakuje: Lithuania, Latvia, Malta, Slovakia

Sys.setlocale("LC_TIME", lct)

# tworzenie wskaźników

# izolacja państwa - S7
# restrykcje wewnętrzne: szkoły, praca, transport, wydarzenia, ograniczenia ruchu
# troska o bezpieczeństwo: testy, kampania informacyjna i utrzymywanie kontaktu
# finansowe kwestie rozpatrzone osobno

policies_names <- c("country", "code", "date","c1_school", "c1_general", "c2_workplace",
                    "c2_general", "c3_events", "c3_general", "c4_gatherings", "c4_general",
                    "c5_transport", "c5_general", "c6_homestaying", "c6_general",
                    "c7_internal_mov", "c7_general", "c8_international_mov", "e1_incomesupport",
                    "e1_general", "e2_debtrelief", "e3_fiscal", "e4_intrsupport", "h1_publicinfo", "h1_general",
                    "h2_testing", "h3_tracing", "h4_healthcare_support", "h5_vaccines", "widlcard", 
                    "cases", "deaths", "stringency", "str_display", "legacy_stringency", "legacy_str_display")
names(policies_all) <- policies_names

policies <- policies_all %>% mutate_at(c(4:27), ~replace(., is.na(.), 0)) %>%
  mutate(restricts = ((c1_school + c1_general)/4 + (c2_workplace + c2_general)/4 + 
  (c3_events + c3_general)/3 + (c4_gatherings + c4_general)/5 + (c5_transport + c5_general)/3 +
  (c6_homestaying + c6_general)/4 + (c7_internal_mov + c7_general)/4 + c8_international_mov/4)/8,
  care = (h2_testing/3 + h3_tracing/2 + (h1_publicinfo + h1_general)/3 + 
            (e1_incomesupport + e1_general)/3 + e2_debtrelief/2)/5) 

write.table(policies, "policies_data.csv")

policies <- read.table("policies_data.csv")

max_policies_used <- policies %>% group_by(country) %>% 
  select(country, restricts, care) %>% 
  summarise(max_restrict = max(restricts), max_care = max(care), index = max_restrict/max_care)

countries_list <- max_policies_used$country


# Stan epidemii

cases <- read.csv("tests.csv")
cases_data <- cases %>% filter(location %in% countries_list) %>%
  select(location, date, cases = total_cases_per_million,
         deaths = total_deaths_per_million, tests = total_tests_per_thousand) %>%
  mutate(date = as.Date(date)) %>%
  filter(date < "2020-05-01" & date > "2019-12-31")

write.table(cases_data, "cases_data.csv")
# manual completing

cases_completed <- read.table("cases_data_complete.csv")
cases_completed$date <- as.Date(cases_completed$date)



# gdp (ostatni kwartał 2019, w milionach Euro)

gdp <- read.csv("gdp.tsv") %>% filter(country %in% countries_list) %>%
  mutate(gdp = gdp*1000000) # w Euro

economic_measures <- policies %>%
  select(country, date, e1_incomesupport, e2_debtrelief, e3_fiscal, e4_intrsupport, 
         h4_healthcare_support, h5_vaccines) %>%
  group_by(country) %>% summarise(fiscal = sum(e3_fiscal, na.rm = T), 
                                  intr_support = sum(e4_intrsupport, na.rm = T),
                                  healthcare = sum(h4_healthcare_support, na.rm = T), 
                                  vaccines = sum(h5_vaccines, na.rm = T))

econ_measures_scaled <- economic_measures %>% left_join(gdp) %>%
  mutate(fiscal = fiscal/gdp*100, healthcare = healthcare/gdp*1000, 
         intr_support = intr_support/gdp*1000, vaccines = vaccines/gdp*1000)


# % starszych w społeczeństwie (2019)

elders <- read.csv("elders.tsv") %>% filter(country %in% countries_list)


# giełda

stock <- read.csv("stock.txt") %>% mutate(change = (last_apr - last_dec)/last_dec, 
                                          max_change = (min - last_dec)/last_dec)

# działania dla Polski

poland_table <- policies_all %>% filter(country == "Poland") %>% select(1, 3:29) %>%
  distinct(c1_school, c1_general, c2_workplace, c2_general, c3_events, c3_general,
           c4_gatherings, c4_general, c5_transport, c5_general, c6_homestaying, c6_general,
           c7_internal_mov, c7_general, c8_international_mov, e1_incomesupport, e1_general,
           e2_debtrelief, e3_fiscal, e4_intrsupport, h1_publicinfo, h1_general, h2_testing,
           h3_tracing, h4_healthcare_support, h5_vaccines, .keep_all = T)
  
