library(stringdist)

crm_input_path <- 'data/crm_leads.csv'
scraped_input_path <- 'data/crm_leads.csv'

#Extracts the file name and adds a timestamp (e.g., "ma-state-search_20260706_1045")
scraped_file_name <- sub("\\.[^.]+$", "", basename(scraped_input_path)) 
timestamp <- format(Sys.time(), "%Y%m%d_%H%M")

green_output_path <- paste0("data/GREEN_Safe_", scraped_file_name, "_", timestamp, ".csv")
yellow_output_path <- paste0("data/YELLOW_Review_", scraped_file_name, "_", timestamp, ".csv")

crm_df <- read.csv(crm_input_path, stringsAsFactors = FALSE)

scraped_df <- read.csv(scraped_input_path, 
                         colClasses = c("Postal.code" = "character"), 
                         stringsAsFactors = FALSE)

# scrubbing functions
clean_name_fn <- function(names) {
  n <- tolower(names)
  n <- gsub("&", "and", n)             
  n <- gsub("\\bthe\\b", "", n)        
  n <- gsub("[[:punct:]]", "", n)      
  n <- gsub("\\b(inc|llc|co|corp|company)\\b", "", n) 
  n <- gsub("^na$", "", n) 
  return(trimws(n))
}

clean_street_fn <- function(streets) {
  s <- tolower(streets)
  s <- gsub("[[:punct:]]", "", s)
  s <- gsub("\\bnorth\\b", "n", s)
  s <- gsub("\\bsouth\\b", "s", s)
  s <- gsub("\\beast\\b", "e", s)
  s <- gsub("\\bwest\\b", "w", s)
  s <- gsub("\\b(st|street|ave|avenue|rd|road|blvd|boulevard|hwy|highway|dr|drive|ln|lane|ct|court|pl|place|sq|square|ste|suite|apt|unit|rm|room)\\b", "", s)
  s <- gsub("\\s+", "", s) 
  s <- gsub("^na$", "", s) 
  return(s)
}

# standardizing crm 
crm_clean_names <- clean_name_fn(crm_df$Company)
crm_clean_phones <- gsub("[^0-9]", "", crm_df$Phone)
crm_clean_emails <- tolower(trimws(crm_df$Email))
crm_clean_emails <- gsub("^na$", "", crm_clean_emails) 
crm_clean_zips <- substr(trimws(crm_df$Zip.Code), 1, 5)
crm_clean_streets <- clean_street_fn(crm_df$Street)

# --- 5. INITIALIZE THE BUCKETS ---
green_new_leads <- data.frame()
yellow_review_dashboard <- data.frame()

# --- 6. THE TRIAGE LOOP (Processing Input B) ---
for (i in 1:nrow(scraped_df)) {
  
  t_name <- scraped_df$Name[i]
  t_phone <- scraped_df$Phone[i]
  t_email <- scraped_df$Email[i]
  t_website <- scraped_df$Website[i]
  t_zip <- substr(trimws(scraped_df$Postal.code[i]), 1, 5)
  t_street <- scraped_df$Street.2[i]
  
  clean_t_name <- clean_name_fn(t_name)
  clean_t_phone <- sub("^1", "", gsub("[^0-9]", "", t_phone)) 
  clean_t_email <- tolower(trimws(t_email))
  clean_t_email <- gsub("^na$", "", clean_t_email)
  clean_t_street <- clean_street_fn(t_street)
  
  # alarms
  name_sims <- stringsim(clean_t_name, crm_clean_names, method = "jw")
  name_matches <- which(!is.na(name_sims) & name_sims >= 0.85) 
  
  phone_matches <- which(crm_clean_phones == clean_t_phone & crm_clean_phones != "")
  email_matches <- which(crm_clean_emails == clean_t_email & crm_clean_emails != "")
  
  # 1: Strict Match 
  exact_street_matches <- which(crm_clean_streets == clean_t_street & crm_clean_streets != "")
  
  # 2: Fuzzy match
  street_sims <- stringsim(clean_t_street, crm_clean_streets, method = "jw")
  fuzzy_street_matches <- which(!is.na(street_sims) & street_sims >= 0.85)
  
  # Notice how we use the 'fuzzy' matches to trigger the initial alarm basket
  all_hits <- unique(c(name_matches, phone_matches, email_matches, fuzzy_street_matches))
  
  # -- SORT INTO BUCKETS --
  if (length(all_hits) == 0) {
    # GREEN: passed fuzzy net, new lead
    green_new_leads <- rbind(green_new_leads, scraped_df[i, ])
  } else {
    
    is_true_duplicate <- FALSE
    if (length(name_matches) > 0) {
      matched_zips <- crm_clean_zips[name_matches]
      
      # RED BUCKET RULE: We demand the EXACT street match before we delete it
      matched_streets_exact <- name_matches %in% exact_street_matches 
      
      if (any(t_zip == matched_zips & t_zip != "") || any(matched_streets_exact)) {
        is_true_duplicate = TRUE
      }
    }
    
    if (is_true_duplicate) {
      # RED: Trash (Safely deleted based on exact math)
    } else {
      #YELLOW: Review (Caught the dirty data before it hit Green)
      for (hit in all_hits) {
        review_row <- data.frame(
          Action = "REVIEW",
          Scraped_Company = t_name,
          CRM_Company = crm_df$Company[hit],
          Scraped_Zip = t_zip,
          CRM_Zip = crm_df$Zip.Code[hit],
          Scraped_Street = t_street,
          CRM_Street = crm_df$Street[hit],
          Scraped_Email = t_email,
          CRM_Email = crm_df$Email[hit],
          Scraped_Phone = t_phone,
          CRM_Phone = crm_df$Phone[hit],
          stringsAsFactors = FALSE
        )
        
        # This binds your review columns side-by-side with the completely intact raw row from your scraped file
        complete_row <- cbind(review_row, scraped_df[i, , drop = FALSE])
        yellow_review_dashboard <- rbind(yellow_review_dashboard, complete_row)
      }
    }
  }
}
write.csv(green_new_leads, green_output_path, row.names = FALSE)
write.csv(yellow_review_dashboard, yellow_output_path, row.names = FALSE)

print("complete")