# Purpose: Analyze USUDC results to see if tapered fixed imbalances
# Author: Karthik Tadepalli
# Created: Mon Oct 12 00:42:09 2020

#---------------SETUP------------------

# load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(devtools, tidyverse, janitor, data.table, ggthemes,
               lubridate, rvest, pdftools, stringi, AER, sandwich, here, tabulizer, glue)

#------------SCRAPE-------------------

breaks <- extract_tables("raw_data/break.pdf", method = 'stream')
br <- breaks[[1]] %>%
  as_tibble() %>% 
  select(team = V4, points = V5, speaks = V6, ones = V7, twos = V8) %>% 
  mutate(points = as.numeric(points), speaks = as.numeric(speaks),
         ones = as.numeric(ones), twos = as.numeric(sub(" Total speaker score", "", twos)),
         team = gsub("[^\x01-\x7F]", "", team)) %>%
  filter(!is.na(points))
write_csv(br, "breaks.csv")

judges <- read_csv("raw_data/judges.csv")

full <- data.frame(team = character(), rank = numeric(), position = character(),
                   chair = character(), judge2 = character(), judge3 = character(),
                   judge4 = character(), judge5 = character(), chair_broke = numeric(),
                   points = numeric(), round = numeric())

for (r in 1:8) {
  results <- extract_tables(glue("raw_data/draws/round{r}.pdf"), method = "stream")
  if (r != 3) {
    df <- results[[1]] %>% as_tibble() %>% 
      select(team = V1, rank_pos = V2, panel = V3) %>%
      separate(rank_pos, into = c("rank", "position"), sep = 4) 
  } else {
    df <- results[[1]] %>% as_tibble() %>%
      select(team = V1, rank = V2, position = V3, panel = V4)
  }
  df <- df %>%
    separate(panel, into = c("chair", "judge2", "judge3", "judge4", "judge5"), sep = ", ") %>%
    mutate(rank = as.numeric(substr(rank, 1, 1)),
           team = gsub("[^\x01-\x7F]", "", team),
           chair = sub("C$", "", chair)) %>%
    filter(!is.na(rank)) 
  
  for (i in 2:length(results)) {
    df_i <- results[[i]] %>% as_tibble() %>% 
      select(team = V1, rank = V2, position = V3, panel = V4) %>%
      separate(panel, into = c("chair", "judge2", "judge3", "judge4", "judge5"), sep = ", ") %>%
      mutate(rank = as.numeric(substr(rank, 1, 1)),
             team = gsub("[^\x01-\x7F]", "", team),
             chair = sub("C$", "", chair))
    df <- bind_rows(df, df_i)
  }
  multiplier <- ifelse(r < 2, 3, ifelse(r < 4, 2, 1))
  df <- df %>%
    mutate(points = multiplier * (4 - rank), 
           chair_broke = as.numeric(chair %in% judges$name),
           round = r)
  full <- bind_rows(full, df)
}

# load the tabula data to get the full data 
for (r in 3:8) {
  multiplier <- ifelse(r < 2, 3, ifelse(r < 4, 2, 1))
  tabula <- read_csv(glue("raw_data/tabula/tabula-round{r}.csv")) %>% clean_names() %>%
    rename(team = x1, rank = x2) %>%
    filter(!is.na(team)) %>%
    separate(adjudicators, 
             into = c("chair", "judge2", "judge3", "judge4", "judge5"), sep = ", ") %>%
    mutate(rank = as.numeric(substr(rank, 1, 1)),
           team = gsub("[^\x01-\x7F]", "", team),
           chair = sub("C$", "", chair),
           points = multiplier * (4 - rank), 
           chair_broke = as.numeric(chair %in% judges$name),
           round = r)
  full <- bind_rows(full, tabula)
}

# augment the data with other information
full <- full %>%
  mutate(broke = team %in% br$team) %>%
  left_join(full %>% group_by(round, position) %>% summarise(pos_points_round = mean(points))) %>%
  mutate(pos_points = sum(pos_points_round))

write_csv(full, "results.csv")
