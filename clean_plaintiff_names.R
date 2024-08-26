library(here)
library(janitor)
library(tidyverse)
library(stringdist)

generate_random_sample <- function(.df, .number) {
  set.seed(42)  
  sample_size <- .number  
  random_indices <- sample(nrow(.df), sample_size)
  random_sample <- .df[random_indices, ]
  return(random_sample)
}

preprocess_text <- function(text) {
  text <- tolower(text)
  text <- trimws(text)
  text <- trimws(gsub("^[[:punct:]]+|[[:punct:]]+$", "", text))
  text <- trimws(gsub("plaintiff", "", text))
  text <- trimws(gsub(" -", "", text))
  return(text)
}

#This assumes the data adheres to the debt data standard, as seen here:
#https://docs.google.com/spreadsheets/d/18AyuiEUJUhQn6fyf-HavUAlBoTLE-3PaUKlQy-zj4Rk/edit#gid=0
#This function takes an argument called cluster size. Cluster size must be a number and determines how conservative the matching is.
#If you're unsure of where to start, I recommend a cluster size of .15.

clean_up_plaintiff_names = function(data, cluster_size) {

  data = data %>% 
    mutate(plaintiff_name = preprocess_text(plaintiff_name))
  
  # Take all the names
  distinct_names = data %>% distinct(plaintiff_name) %>% pull(plaintiff_name)
  
  distinct_names = distinct_names[!is.na(distinct_names)]
  
  # Find stringdist between names
  dist_matrix = stringdistmatrix(distinct_names, distinct_names, method = "jw")
  
  # Cluster the names together
  clusters = hclust(as.dist(dist_matrix), method = "average") %>% 
    cutree(h = cluster_size)
  
  name_cluster_df = data.frame(name = distinct_names, cluster = clusters)
  
  # Find the unique names and return a "representative name" for each cluster
  cluster_representatives = name_cluster_df %>%
    group_by(cluster) %>%
    summarize(representative = name[which.max(table(name))])
  
  # Create a named vector for cluster representative names
  name_to_representative = name_cluster_df %>%
    left_join(cluster_representatives, by = "cluster") %>% 
    select(name, representative)
  
  # Join representatie name to the plaintiff name from raw data 
  data_clean = data %>%
    left_join(name_to_representative, by = c("plaintiff_name" = "name")) %>%
    mutate(plaintiff_name_clean = ifelse(is.na(representative), plaintiff_name, representative)) %>%
    select(-representative)  
  
  #Finally I want to do some common names by hand 
  data_clean_manual=  data_clean %>%
    mutate(plaintiff_name_clean = case_when(
      grepl("midland", plaintiff_name_clean) & grepl("funding", plaintiff_name_clean) ~ "midland funding llc",
      grepl("midland", plaintiff_name_clean) & grepl("credit", plaintiff_name_clean) ~ "midland credit management",
      grepl("portfolio recovery", plaintiff_name_clean) ~ "portfolio recovery associates llc",
      grepl("jefferson capital", plaintiff_name_clean) | grepl("jefferson capitol", plaintiff_name_clean) ~ "jefferson capital systems llc",
      grepl("cavalry", plaintiff_name_clean) | grepl("calvary", plaintiff_name_clean) ~ "cavalry spv",
      grepl("lvnv", plaintiff_name_clean) ~ "lvnv funding llc",
      #large banks without assignee?
      grepl("citibank", plaintiff_name_clean) & !grepl("assignee", plaintiff_name) ~ "citibank",
      grepl("capital one", plaintiff_name_clean) & !grepl("assignee", plaintiff_name) ~ "capital one",
      grepl("jp morgan", plaintiff_name_clean) | grepl("jpmorgan", plaintiff_name_clean) & !grepl("assignee", plaintiff_name) ~ "jp morgan chase",
      #if there's an assignee in the plaintiff name, then we want to preserve the full name for complexity.
      grepl("assignee", plaintiff_name) ~ plaintiff_name,
      TRUE ~ plaintiff_name_clean))
  
  return(data_clean_manual)
  
}

## Here's an example of how this cleaning works. I clean on some random samples to verify the accuracy of the work.
# test = generate_random_sample(df1, 10000)
# test_clean = clean_up_plaintiff_names(test, .15)
# tabyl(test_clean, plaintiff_name_clean) %>% arrange(desc(n)) %>% view()
