# ANALYSIS

library(ggplot2)
library(tidyr)
install.packages("imputeTS")
library(imputeTS)
library(knitr)

# liczba przypadków
cases_completed <- cases_completed %>%
  group_by(location) %>% mutate(cases = na_interpolation(cases))

# kiedy pojawił się pierwszy przypadek?
first_cases <- cases_completed %>% select(location, date, cases) %>% group_by(location) %>%
  filter( ((lag(cases) == 0 | is.na(lag(cases))) & cases > 0)) %>% arrange(date)  

first_countries <- first_cases %>% filter(date < "2020-02-10")
middle_countries <- first_cases %>% filter(date > "2020-02-10" & date < "2020-03-01")
last_countries <- first_cases %>% filter(date >= "2020-03-01")

cases_completed %>% filter(location %in% first_countries$location, date > "2020-01-20") %>%
  ggplot() + geom_line(aes(x = date, y = cases, col = location)) +
  labs(title = "Potwierdzone przypadki (grupa I)", x = "Czas", 
       y = "Potwierdzone przypadki")

cases_completed %>% filter(location %in% middle_countries$location, date > "2020-02-20") %>%
  ggplot() + geom_line(aes(x = date, y = cases, col = location)) +
  labs(title = "Potwierdzone przypadki (grupa II)", x = "Czas", 
       y = "Potwierdzone przypadki")

cases_completed %>% filter(location %in% last_countries$location, date > "2020-02-29") %>%
  ggplot() + geom_line(aes(x = date, y = cases, col = location)) +
  labs(title = "Potwierdzone przypadki (grupa III)", x = "Czas", 
       y = "Potwierdzone przypadki")



# porównanie liczby przypadków i liczby przeprowadzonych testów
# jakiś wykres, gdzie górną liczbą jest l. testów, a dolną - przypadków (geom_ribbon)
# uzupełnić brakujące dane

cases_modified <- cases_completed %>% group_by(location) %>% mutate(tests_mod = if_else( 
  ((lag(cases) == 0 & cases > 0 & is.na(tests)) | ( is.na(lag(cases)) & is.na(tests) & cases > 0)),
   cases, if_else(cases == 0 & is.na(tests), 0, tests*1000))) %>%
  mutate(tests_mod = na_interpolation(tests_mod))
                    

# obliczenie śrdniego % pozytywnych testów 

positive_ratio <- cases_modified %>% select(location, cases, tests_mod) %>%
  group_by(location) %>% filter(tests_mod > 0) %>% mutate(positive_ratio = cases/tests_mod) %>%
  summarise(avg_positive_ratio = mean(positive_ratio), max_cases = max(cases), max_tests = max(tests_mod))



complete_cases_ratio <- cases_completed %>% group_by(location) %>% filter(cases > 0) %>%
  summarise(complete_cases_ratio = sum(!is.na(tests))/n())

positive_ratio %>% left_join(complete_cases_ratio, by = "location") %>% 
  mutate(avg_positive_ratio = if_else(avg_positive_ratio > 0.2, 0.2, avg_positive_ratio)) %>%
  pivot_longer(c(avg_positive_ratio, complete_cases_ratio), 
               names_to = "type", values_to = "value") %>%
  ggplot(aes(x = location, y = value, 
             fill = factor(if_else(location == "France", "Highlighted", "Normal")))) +
  geom_col() + facet_grid(rows = "type", scales = "free") +
  geom_text(aes(label=round(value, 2)), vjust=-0.3, size=3.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
  guides(fill=FALSE) +
  labs(title = "Stosunek pozytywnych testów", x = "Państwo", 
       y = "Stosunek pozytywnych testów")

cases_tests_cor <- cases_modified %>% select(location, cases, tests_mod) %>%
  group_by(location) %>% summarise(max_cases = max(cases), max_tests =  max(tests_mod)) %>%
  summarise(cor = cor(max_cases, max_tests))

# "czyszczenie" liczby przypadków z efektu testowania

cases_modified_elders <- cases_modified %>% left_join(elders, by = c("location" = "country"))

# Elders
tests_cases_model <- lm(cases ~ tests_mod + elders, cases_modified_elders)
summary(tests_cases_model)
# Wiek nie jest istotny w modelu

tests_cases_model <- lm(cases ~ tests_mod, cases_modified_elders)
summary(tests_cases_model)

cases_modified_elders %>% filter(tests_mod > 0) %>% mutate(positive_ratio = cases/tests_mod) %>%
  ungroup() %>% summarise(cor_elders = cor(cases, elders), cor_tests = cor(cases, tests_mod))
# korelacja między positive_ratio a cases nie jest duża, podobnie między ratio a tests_mod

cases_clean <- tests_cases_model$coefficients[1] + tests_cases_model$residuals


# rangowanie państw pod względem długości trwania epidemii i liczby przypadków

cases_queue <- data.frame(location = cases_modified$location, 
                          date = cases_modified$date, cases_clean) %>%
  group_by(location) %>% summarise(max_cases = max(cases_clean)) %>%
  arrange(desc(max_cases)) %>% mutate(cases_nr = 1:n())

time_queue <- cases_modified %>% select(location, date, cases) %>%
  filter(cases > 0) %>% group_by(location) %>% 
  summarise(days = n(), first_day_cases = min(cases)) %>%
  arrange(desc(days), desc(first_day_cases)) %>% mutate(time_nr = 1:n())

epidemic_develop_index <- cases_queue %>% left_join(time_queue, by = "location") %>%
  mutate(index = time_nr - cases_nr)

epidemic_develop_index %>% arrange(time_nr) %>% 
  mutate(lower = if_else(time_nr > cases_nr, cases_nr, time_nr), 
         upper = if_else(time_nr > cases_nr, time_nr, cases_nr), 
         outcome = if_else(index > 0, "pos", "neg"),
         location_nr = 1:n()) %>%
  ggplot(aes(x = location, y = index)) + 
  geom_bar(aes(fill = outcome), stat = "identity") +
  geom_text(aes(label=index), vjust=0, size=3.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
  guides(fill=FALSE) +
  labs(title = "Wskaźnik względnego rozwoju epidemii", x = "Państwo", 
       y = "Wskaźnik")

# porównanie śmiertelności (liczba śmierci/liczba przypadków) - wykres liniowy

mortality <- cases_modified_elders %>% select(location, cases, deaths, elders) %>%
  group_by(location) %>% filter(cases > 0 & !is.na(deaths)) %>% mutate(death_rate = deaths/cases)
  
# śmiertelność a osoby starsze

mort_elders_model <- lm(death_rate ~ elders, mortality)
summary(mort_elders_model)

mortality$death_rate_mod <- mort_elders_model$coefficients[1] + mort_elders_model$residuals

final_mortality <- mortality %>% group_by(location) %>% arrange(desc(cases)) %>% slice(1) %>%
  summarise(mortality = death_rate_mod)

avg_mortality_2 <- mortality %>% summarise(avg_mortality = mean(death_rate))

total_cases <- cases_modified %>% 
  group_by(location) %>% arrange(desc(cases)) %>% slice(1) %>%
  summarise(nr_cases = cases)

q_nr_cases <- quantile(total_cases$nr_cases, probs=c(0.25, 0.5, 0.75))
q_mortality <- quantile(final_mortality$mortality, probs=c(0.25, 0.5, 0.75))


total_cases %>% left_join(final_mortality, by = "location") %>% 
  ggplot(aes(x = nr_cases, y = mortality)) +
  geom_vline(xintercept = q_nr_cases[2], color = "red") +
  geom_vline(xintercept = c(q_nr_cases[1], q_nr_cases[3]), color = "yellow") +
  geom_hline(yintercept = q_mortality[2], color = "red") +
  geom_hline(yintercept = c(q_mortality[1], q_mortality[3]), color = "yellow") +
  geom_point() + geom_text_repel(aes(label=location), vjust=-0.7, size=3) +
  labs(title = "Stosunek śmiertelności do liczby przypadków (na mln mieszkańców)",
        x = 'Liczba przypadków', y = '"Oczyszczona" śmiertelność')

healthcare_index <- total_cases %>% left_join(final_mortality) %>% 
  mutate(mortality = mortality - min(mortality)) %>%
  mutate(healthcare_index = mortality/nr_cases*10000)

healthcare_index_gdp <- healthcare_index %>% left_join(gdp, by = c("location" = "country"))
 # summarise(cor = cor(healthcare_index, gdp))

health_gdp_model <- lm(healthcare_index ~ gdp, healthcare_index_gdp)
summary(health_gdp_model) # GDP jest nieistotne

# śmiertelność w porównaniu z profilem wiekowym

mort_and_elders <- mortality %>% rename(country = location) %>% left_join(elders)
cases_and_elders <- total_cases %>% rename(country = location) %>% left_join(elders)

cor(mort_and_elders$avg_mortality, mort_and_elders$elders) # bardzo słabo
cor(cases_and_elders$nr_cases, cases_and_elders$elders) # słabo i ujemnie

ggplot(mort_and_elders, aes(x = elders, y = avg_mortality)) +
  geom_point() + geom_text(aes(label=country), vjust=-0.7, size=3) +
  geom_smooth(method = "lm") +
  labs(title = "Stosunek śmiertelności do odsetka osób starszych",
       x = "Odsetek osób po 60 roku życia", y = "Przeciętna śmiertelność")

ggplot(cases_and_elders, aes(x = elders, y = nr_cases)) +
  geom_point() + geom_text(aes(label=country), vjust=-0.7, size=3) +
  geom_smooth(method = "lm") +
  labs(title = "Stosunek liczby przypadków do odsetka osób starszych",
       x = "Odsetek osób po 60 roku życia", y = "Liczba przypadków")

# Wnioski: nie trzeba tego brać pod uwagę

# surowość

stringency_data %>% 
  filter(country %in% first_countries$location) %>%
  ggplot() + geom_line(aes(x = date, y = stringency, col = country)) +
  labs(title = "Stringency Index (Grupa I)",
       x = "Czas", y = "Stringency Index")

stringency_data %>% 
  filter(country %in% middle_countries$location) %>%
  ggplot() + geom_line(aes(x = date, y = stringency, col = country)) +
  labs(title = "Stringency Index (Grupa II)",
       x = "Czas", y = "Stringency Index")

stringency_data %>% 
  filter(country %in% last_countries$location) %>%
  ggplot() + geom_line(aes(x = date, y = stringency, col = country)) +
  labs(title = "Stringency Index (Grupa III)",
       x = "Czas", y = "Stringency Index")

# jak ma się surowość restrykcji do liczby przypadków

stringency_data %>% left_join(cases_modified, by = c("country" = "location")) %>% 
  filter(!is.na(stringency)) %>%
  group_by(country) %>% summarise(max_str = max(stringency), max_cases = max(cases)) %>%
  summarise(cor = cor(max_str, max_cases)) # bardzo, bardzo mała




# wydatki

econ_measures_scaled %>% mutate(fiscal_adj = if_else(fiscal > 200, 200, fiscal)) %>%
  ggplot(aes(x = country, y = fiscal_adj)) + geom_col() +
  geom_text(aes(label=round(fiscal, 2)), vjust=-0.3, size=3.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
  labs(title = "Wydatki fiskalne i ulgi podatkowe",
       x = "Państwo", y = "Procent PKB")

econ_measures_scaled %>% mutate(healthcare_adj = if_else(healthcare > 100, 100, healthcare)) %>%
  ggplot(aes(x = country, y = healthcare_adj)) + geom_col() +
  geom_text(aes(label=round(healthcare, 2)), vjust=-0.3, size=3.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
  labs(title = "Dodatkowe nakłady na służbę zdrowia",
       x = "Państwo", y = "Promil PKB")

econ_measures_scaled %>% filter(vaccines > 0) %>%
  ggplot(aes(x = country, y = vaccines)) + geom_col() +
  geom_text(aes(label=round(vaccines, 2)), vjust=-0.3, size=3.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
  labs(title = "Inwestycje w tworzenie szczepionki",
       x = "Państwo", y = "Promil PKB")

econ_measures_scaled %>% filter(intr_support > 0) %>%
  ggplot(aes(x = country, y = intr_support)) + geom_col() +
  geom_text(aes(label=round(intr_support, 4)), vjust=-0.3, size=3.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
  labs(title = "Inwestycje w tworzenie szczepionki",
       x = "Państwo", y = "Promil PKB")


policies %>% select(country, date, e2_debtrelief) %>% 
  filter(!is.na(e2_debtrelief) & e2_debtrelief > 0) %>%
  mutate(e2_debtrelief = if_else(e2_debtrelief == 1, "Wąski", "Szeroki")) %>%
  group_by(country) %>% distinct(e2_debtrelief, .keep_all = T) %>% arrange(country) %>% 
  kable(col.names = c("Państwo", "Data wprowadzenia", "Zakres"))

policies %>% select(country, date, e1_incomesupport, e1_general) %>% 
  filter(!is.na(e1_incomesupport) & e1_incomesupport > 0) %>%
  mutate(e1_incomesupport = if_else(e1_incomesupport == 1, "<50%", ">=50%")) %>%
  mutate(e1_general = if_else(e1_general == 1, "Zarejestrowani", "Wszyscy")) %>%
  group_by(country) %>% distinct(e1_incomesupport, e1_general, .keep_all = T) %>% 
  arrange(country) %>% kable(col.names = c("Państwo", "Data wprowadzenia", "Wielkość", "Zakres"))
  

# Efektywność wydatków na służbę zdrowia
econ_measures_scaled %>% left_join(total_cases, by = c("country" = "location")) %>% 
  left_join(avg_mortality, by = c("country" = "location")) %>%
  summarise(cor_cases = cor(healthcare, nr_cases), cor_mort = cor(healthcare, avg_mortality))

install.packages("ggrepel")
library(ggrepel)

max_policies_used %>% ggplot(aes(x = max_care, y = max_restrict)) +
  geom_abline(slope = 1, intercept = 0, color = "green") +
  geom_abline(slope = 2, intercept = 0, color = "red") +
  geom_abline(slope = 1.5, intercept = 0, color = "yellow") +
  geom_point() + geom_text_repel(aes(label=country)) +
  labs(title = "Strategie walki z epidemią", x = "Strategia wspomagania", 
       y = "Strategia restrykcji")

# giełda

stock %>% mutate(label = paste(country, index)) %>%
  pivot_longer(c(change, max_change), 
               names_to = "type", values_to = "value") %>%
  ggplot(aes(x = label, y = value)) + geom_col(fill = "dark red") +
  facet_grid(rows = "type") +
  geom_text(aes(label=round(value, 2)), vjust=-0.3, size=3.5, color = "white") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
  labs(title = "Procentowa zmiana wartości indeksów (30.12.19 - 30.04.20)",
       x = "Państwo", y = "Procentowa zmiana")
