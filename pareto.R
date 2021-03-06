# optymalizacja

# zebranie indeksów

index_table <- epidemic_develop_index %>% select(location, epid_index = index) %>%
  left_join(healthcare_index) %>% 
  left_join(max_policies_used, by = c("location" = "country")) %>% 
  select(location, epid_index, healthcare_index, style_index = index) %>%
  left_join(stock, by = c("location" = "country")) %>% 
  select(location, epid_index, healthcare_index, style_index, econ_index = max_change)

# epid_index -> min
# healthcare_index -> min
# style_index -> min
# econ_index -> max

search_pareto <- function(base, nr) {
  epid_best <- base %>% arrange(epid_index) %>% slice(nr)

epid_pareto <- base %>% filter((epid_index <= epid_best$epid_index) | 
  (healthcare_index <= epid_best$healthcare_index) |
  (style_index <= epid_best$style_index) | (econ_index >= epid_best$econ_index))

health_best <- epid_pareto %>% arrange(healthcare_index) %>% slice(nr)

health_pareto <- epid_pareto %>% filter((epid_index <= health_best$epid_index) |
   (style_index <= health_best$style_index) | (healthcare_index <= health_best$healthcare_index) |
     (econ_index >= health_best$econ_index))

style_best <- health_pareto %>% arrange(style_index) %>% slice(nr)

style_pareto <- health_pareto %>% filter((epid_index <= style_best$epid_index) |
   (healthcare_index <= style_best$healthcare_index) | (style_index <= style_best$style_index) |
     (econ_index >= style_best$econ_index))

econ_best <- style_pareto %>% arrange(desc(econ_index)) %>% slice(nr)

econ_pareto <- style_pareto %>% filter((epid_index <= econ_best$epid_index) |
   (healthcare_index <= econ_best$healthcare_index) | (style_index <= econ_best$style_index)
   | (econ_index >= econ_best$econ_index))

return(econ_pareto)
}

base <- index_table
nr <- 1
repeat {
  base <- search_pareto(base, nr)
  nr <- nr+1
  if(nr == dim(base)[1]){
    break
  }
}


# klasyfikacja "medalowa"

ranks_table <- index_table %>%
  mutate(min_epid = min(epid_index), range_epid = max(epid_index)-min(epid_index),
         min_health = min(healthcare_index), range_health = max(healthcare_index)-min(healthcare_index),
         min_style = min(style_index), range_style = max(style_index)-min(style_index),
         min_econ = min(econ_index), range_econ = max(econ_index)-min(econ_index)) %>%
  mutate(epid_ranks = (epid_index - min_epid)/range_epid) %>%
  mutate(health_ranks = (healthcare_index - min_health)/range_health) %>%
  mutate(style_ranks = (style_index - min_style)/range_style) %>%
  mutate(econ_ranks = 1-(econ_index - min_econ)/range_econ) %>%
  mutate(index = (epid_ranks + health_ranks + style_ranks + econ_ranks)/4)
