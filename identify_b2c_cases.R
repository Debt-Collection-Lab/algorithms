library(tidyverse)
library(here)
library(janitor)
library(googlesheets4)
library(data.table)
library(fst)
library(sf)

generate_random_sample <- function(.df, .number) {
  set.seed(42)  
  sample_size <- .number  
  random_indices <- sample(nrow(.df), sample_size)
  random_sample <- .df[random_indices, ]
  return(random_sample)
}

universal_words_to_filter = c(
  "\\bllc\\b|\\bl\\.l\\.c\\b|\\bl\\.l\\.c\\s",
  "\\binc\\b|\\binc\\.|\\binc\\s",
  "company",
  "\\bbank\\b",
  "association",
  "corporation",
  "corp ",
  "corp",
  "credit union",
  "\\bco\\bb",
  "service",
  "services",
  "investments",
  "capital one",
  "wells fargo",
  "credit",
  "insurance",
  "properties",
  "association",
  "clinic",
  "management",
  "landscape",
  "assoc",
  "limited",
  "\\bltd.\\b",
  "\\bltd\\b",
  "\\s*inc\\s*\\.",
  "\\s*na\\s*\\.\\s*",
  "(pc|p\\.c\\.|pc\\.|p\\.c)",
  "dept.",
  "department",
  "dept",
  "trust",
  "group",
  "realty",
  "partners",
  "consulting",
  "advisors",
  "holdings",
  "ventures",
  "enterprise",
  "partnership",
  "solutions",
  "international",
  "enterprises",
  "global",
  "investment",
  "assets",
  "development",
  "technologies",
  "strategies",
  "marketing",
  "advisory",
  "research",
  "consultants",
  "capital",
  "investors",
  "corporate",
  "associates",
  "network",
  "systems",
  "innovations",
  "communications",
  "logistics",
  "resources",
  "industries",
  "collaborative",
  "analytics",
  "solutions",
  "properties",
  "holdings",
  "consulting",
  "logistics",
  "properties",
  "dba",
  "society",
  "savings",
  "fund",
  "\\band\\bb",
  "\\bthe\bb",
  "\\d",
  "equipment",
  "city",
  "state",
  "department",
  "authority",
  "business",
  "\\!",
  "\\@",
  "\\#",
  "\\$",
  "\\%",
  "\\^",
  "\\&",
  "\\*",
  "\\(",
  "\\)",
  "\\_",
  "\\+",
  "\\=",
  "\\ball\\bb",
  "church",
  "persons",
  "parties",
  "legal",
  "store",
  "housing",
  "stores",
  "\\bl\\.p\\.|\\blp\\b|\\bl\\.p\\b",
  "apartments",
  "apartment",
  " apt",
  " apts",
  "vacation",
  "heirs",
  "\\bl\\.c\\.c\\b|\\blcc\\b|\\bl\\.c\\.c\\b",
  "\\bp\\.l\\.l\\.c\\b|\\bpllc\\b|\\bp\\.l\\.l\\.c\\b",
  "auto",
  "n/a",
  "dentist",
  "agency",
  "finance",
  "collection",
  "collections",
  "office",
  "offices",
  "university",
  "quick",
  "garage",
  "pacific",
  "construction",
  "consumer",
  "ambulance",
  "urgent",
  "dental",
  "foundation",
  "jewelers",
  "debt",
  "debtor",
  "community",
  "medical",
  "united",
  "oregon",
  "northwest",
  "wireless",
  "financial",
  "property",
  "condominium",
  "school",
  "academy",
  "private",
  "townhomes",
  "gardens",
  " homes",
  "trucking",
  "market",
  "\\bshop\\b",
  "\\bcustom\\b"
)

#'*This takes in a vector of plaintiff names and identifies them as likely businesses*
#'*Plaintiff names should be called plaintiff_name*
isolate_business_plaintiffs = function(.data, .state, .city) { 
  
  words_to_filter = c(universal_words_to_filter, .city, .state)

  if(!("plaintiff_name" %in% names(.data))) { 
    print("Error! No column called plaintiff_name present in the data")
  } else {
    return_data = .data %>% 
      mutate(plaintiff_is_business=case_when(
        grepl(paste(words_to_filter, collapse = "|"), plaintiff_name, ignore.case = TRUE) ~ 1, 
        T~0
      )) %>% 
      filter(plaintiff_is_business==1)
    
    return(return_data)
  }
}

isolate_person_defendants = function(.data, .state, .city, .has_comma_boolean) {
  
  words_to_filter = c(universal_words_to_filter, .city, .state)

  if(!("defendant_name" %in% names(.data))) {
    print("Error! No column called defendant_name present in the data")
  } else {
    return_data = .data %>%
      mutate(defendant_is_person=case_when(grepl("dba", defendant_name)~0, T~1)) %>%
      mutate(defendant_is_person=case_when(
        grepl(paste(words_to_filter, collapse = "|"), defendant_name, ignore.case = TRUE) ~ 0,
        T~defendant_is_person
      )) %>% 
      filter(defendant_is_person==1)
    
    if(.has_comma_boolean==T) { 
      return_data_2 = return_data %>%
        mutate(has_comma = case_when(grepl(",", defendant_name)~1, T~0)) %>%
        mutate(defendant_is_person=case_when(
          has_comma==0 ~ 0,
          T~defendant_is_person
        )) %>% 
        filter(defendant_is_person==1)
      return(return_data_2)
      
    } else { 
      return(return_data)
    }
    
  }
}



