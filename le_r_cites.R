library(fulltext)
library(rcrossref)
library(dplyr)
library(purrr)
le_works <- cr_works(filter = c(issn = "1572-9761", from_pub_date = "1993-08-01"), sample = 10)
le_clct <- ft_get("10.1371/journal.pone.0102437") %>% ft_collect()

# PLoS example
le_plos <- ft_search("landscape ecology", limit = 100)
for(i in le_plos$plos$data$id){
  ft_get(i) %>% ft_collect()
}

le_plos$plos$data$id %>%
  map( ~ ft_get(.) %>% ft_collect())

le_plos_table <- ft_table()

le_plot_r_cite <- le_plos_table %>%
  filter(grepl("R: A Language and Environment for Statistical Computing", .$text))

# Need to figure out how to get year published easier for cr_

le_plos_table %>%
  filter(grepl("SAS", .$text))

