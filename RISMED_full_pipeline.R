# In order not to overload the E-utility servers, NCBI recommends that users post no more
# than three URL requests per second and limit large jobs to either weekends or between
# 9:00 PM and 5:00 AM Eastern time during weekdays. Failure to comply with this policy may
# result in an IP address being blocked from accessing NCBI.

# ----- Packages -----

# install_github("skoval/RISmed")
library(RISmed)
library(dplyr)
library(ggplot2)
library(rjson)
library(httr)

# ----- Get data from PubMed -----

# Define the terms of the query
terms_query <- '("Neoplasms"[Mesh])
  AND (("N ENGL J MED"[JOURNAL])
      OR ("JAMA"[JOURNAL])
      OR ("Lancet"[JOURNAL])
      OR ("BMJ"[JOURNAL])
      OR ("J Clin Oncol"[JOURNAL])
      OR ("JAMA Oncol"[JOURNAL])
      OR ("Lancet Oncol"[JOURNAL])
      OR ("J Natl Cancer Inst"[JOURNAL])
      OR ("Cancer"[JOURNAL])
      OR  ("ANN Oncol"[JOURNAL])
      OR ("Int J Radiat Oncol Biol Phys"[JOURNAL])
      OR ("Radiother Oncol"[JOURNAL]))
  AND humans[MeSH Terms]
  AND (2005/01/01:2005/12/31[Date - Publication] OR 2005[Date - Publication])
  NOT (Letter[ptyp] OR Case Reports[ptyp] OR Comment[sb] OR Editorial[ptyp] OR Review[ptyp] OR News[ptyp] OR Congress[ptyp])
  NOT (1800/01/01:2004/12/31[Date - Publication])
  NOT (2006/01/01:3000[Date - Publication])'

# Get information
query <- EUtilsSummary(terms_query, mindate = 2005, maxdate = 2005, retmax = 10000)

# Number of results
QueryCount(query)

# Get the data from PubMed
records <- EUtilsGet(query, type = "efetch", db = "pubmed")

# ----- Data preprocessing  -----

# Convert authors from list to dataframe
authors_list <- Author(records)

# Initialise data.frame empty
authors <- data.frame(first_forename = rep("NONE", length(authors_list)),
                      first_lastname = rep("NONE", length(authors_list)),
                      last_forename = rep("NONE", length(authors_list)),
                      last_lastname = rep("NONE", length(authors_list)))

# Convert to character
class(authors[, 1]) <- "character"
class(authors[, 2]) <- "character"
class(authors[, 3]) <- "character"
class(authors[, 4]) <- "character"

# Extract names
for(i in 1:length(authors_list)){
  # Counter
  if(i %% 100 == 0) print(i)
  # Extract forenames from first and last authors
  authors$first_forename[i] <- authors_list[i][[1]][1,]$ForeName
  authors$last_forename[i] <- authors_list[i][[1]][nrow(authors_list[i][[1]]),]$ForeName
}

# Create data.frame
pubmed_data <- data.frame(
  "Title" = ArticleTitle(records),
  "first_forename" = as.character(authors$first_forename),
  "last_forename" = as.character(authors$last_forename),
  "PMID" = PMID(records),
  "Year" = YearPubmed(records),
  "Journal" = ISOAbbreviation(records)
)

# See the first rows
head(pubmed_data)

# To character
pubmed_data$first_forename <- as.character(pubmed_data$first_forename)
pubmed_data$last_forename <- as.character(pubmed_data$last_forename)

# Results by year
table(pubmed_data$Year)

# Results by journal
table(pubmed_data$Journal)

# Barplot by journal
table(pubmed_data$Journal) %>% as.data.frame() %>% 
  ggplot() + 
  geom_col(aes(x = Var1, y = Freq), fill = "darkblue") +
  ylab("Frequency of articles") +
  xlab("Journal") +
  theme_classic() +
  theme(axis.text = element_text(size = 7))

# ----- Text mining -----

# Names more frequent
table(pubmed_data$first_forename) %>% sort(decreasing = TRUE) %>% head(20)
table(pubmed_data$last_forename) %>% sort(decreasing = TRUE) %>% head(20)

# keep names if they are not NA
nrow(pubmed_data)
pubmed_data <- pubmed_data[is.na(pubmed_data$first_forename) == FALSE, ]
pubmed_data <- pubmed_data[is.na(pubmed_data$last_forename) == FALSE, ]
nrow(pubmed_data)

# Lisa M -> Lisa
# Michael M -> Michael
for(i in 1:nrow(pubmed_data)){
  pubmed_data$first_forename[i] <- strsplit(pubmed_data$first_forename[i], split = " ", fixed = TRUE)[[1]][1]
  pubmed_data$last_forename[i] <- strsplit(pubmed_data$last_forename[i], split = " ", fixed = TRUE)[[1]][1]
}

# keep only names with length > 1
nrow(pubmed_data)
pubmed_data <- pubmed_data %>% filter(nchar(first_forename)>1)
pubmed_data <- pubmed_data %>% filter(nchar(last_forename)>1)
nrow(pubmed_data)

# Names more frequent
table(pubmed_data$first_forename) %>% sort(decreasing = TRUE) %>% head(20)
table(pubmed_data$last_forename) %>% sort(decreasing = TRUE) %>% head(20)

# ----- Export file first author -----

# To reduce the number of querys, the names are aggregated if they're the same
pubmed_data %>%
  group_by(first_forename) %>%
  summarise(n()) -> export_first_author

# All the names
write.csv(export_first_author, file = "export_first_author.csv", row.names = FALSE)

# First 100 to test GenderAPI
write.csv(export_first_author[1:100, ], file = "export_first_author.csv", row.names = FALSE)

# ----- Export file last author -----

# To reduce the number of querys, the names are aggregated if they're the same
# IF WE WANT RESULTS BY JOURNAL, THE AGGREGATION SHOULD INCLUDE JOURNAL
pubmed_data %>%
  group_by(last_forename) %>%
  summarise(n()) -> export_last_author

# All the names
write.csv(export_last_author, file = "export_last_author.csv", row.names = FALSE)

# First 100 to test GenderAPI
write.csv(export_last_author[1:100, ], file = "export_last_author.csv", row.names = FALSE)

# ----- GenderAPI -----
# Upload CSV to:
# https://gender-api.com/en/csv
# Save as: export_first_author_with_genderAPI.csv
# This process can also be done with the API provided at GenderAPI

first_author_gender <- read.csv("export_first_author_with_genderAPI.csv", header = TRUE)

# Establish a threshold for accuracy (in percentage)
nrow(first_author_gender)
threshold <- 85 # For example
# See cases that are going to be removed
first_author_gender %>% filter(ga_accuracy < threshold)
first_author_gender <- first_author_gender %>% filter(ga_accuracy >= threshold)
nrow(first_author_gender)

# Number of authors per gender
first_author_gender %>%
  group_by(ga_gender) %>%
  summarise(sum(n..))
