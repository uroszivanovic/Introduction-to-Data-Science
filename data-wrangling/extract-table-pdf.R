library(tidyverse)
library(dslabs)
data("research_funding_rates")
research_funding_rates %>% 
  select("discipline", "success_rates_men", "success_rates_women")

library("pdftools")
#temp_file <- tempfile()
#url <- paste0("https://www.pnas.org/content/suppl/2015/09/16/",
#              "1510159112.DCSupplemental/pnas.201510159SI.pdf")
#download.file(url, temp_file)
#txt <- pdf_text(temp_file)
#file.remove(temp_file)

data("raw_data_research_funding_rates")

tab <- str_split(raw_data_research_funding_rates, "\n")

tab <- tab[[1]]

the_names_1 <- tab[3]
the_names_2 <- tab[4]

the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>%
  str_split("\\s{2,}", simplify = TRUE)
the_names_1 

the_names_2 <- the_names_2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE)
the_names_2

tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")
the_names

new_research_funding_rates <- tab[6:14] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)
new_research_funding_rates %>% as_tibble()

identical(research_funding_rates, new_research_funding_rates)
    