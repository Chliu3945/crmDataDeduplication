This R script automates the cleaning and deduplication of unstructured, web scraped lead data against already existing CRM records. It takes in raw CSV files and applies standardizing functions to strip punctuation, standardize street abbreviations, and format contact info.  
To accurately identify hidden duplicates (e.g. company rebrands or typos) the script utilizes the stringdist library to apply a Jaro-Winkler fuzzy string-matching algorithm alongside strict Boolean logic 
The pipeline sorts the data into three distinct buckets:  
- Green (Safe): Unique, net-new leads that are safe to import.
- Red (Trash): Exact matches identified via fuzzy name logic and strict street/zip code matching, which are safely discarded.
- Yellow (Review): Edge cases that trigger the fuzzy-match threshold, which are routed to a formatted output file for manual human review alongside the original CRM data.  
