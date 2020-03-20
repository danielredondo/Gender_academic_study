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
library(reshape)

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
      OR ("ANN Oncol"[JOURNAL])
      OR ("Int J Radiat Oncol Biol Phys"[JOURNAL])
      OR ("Radiother Oncol"[JOURNAL]))
  AND humans[MeSH Terms]
  AND (2002/01/01:2017/12/31[Date - Publication]
       OR 2002[Date - Publication] OR 2003[Date - Publication]
       OR 2004[Date - Publication] OR 2005[Date - Publication]
       OR 2006[Date - Publication] OR 2007[Date - Publication]
       OR 2008[Date - Publication] OR 2009[Date - Publication]
       OR 2010[Date - Publication] OR 2011[Date - Publication]
       OR 2012[Date - Publication] OR 2013[Date - Publication]
       OR 2014[Date - Publication] OR 2015[Date - Publication]
       OR 2016[Date - Publication] OR 2017[Date - Publication]
       )
  NOT (Letter[ptyp] OR Case Reports[ptyp] OR Comment[sb] OR Editorial[ptyp] OR Review[ptyp] OR News[ptyp] OR Congress[ptyp])
  NOT (1800/01/01:2001/12/31[Date - Publication])
  NOT (2018/01/01:3000[Date - Publication])'

# Get information
query <- EUtilsSummary(terms_query, mindate = 2002, maxdate = 2017, retmax = 50000)

# Number of results
QueryCount(query)

# Get the data from PubMed (+1.5 hour to get the data)
# Downloaded 19/03/2020
#records <- EUtilsGet(query, type = "efetch", db = "pubmed")
#save(records, file = "records.RData")
load("records.RData")

# ----- Data preprocessing  -----

# Convert authors from list to dataframe
authors_list <- Author(records)

# Initialise data.frame empty
authors <- data.frame(first_forename = rep("NONE", length(authors_list)),
                      last_forename = rep("NONE", length(authors_list)))

# Convert to character
class(authors[, 1]) <- "character"
class(authors[, 2]) <- "character"

# Extract names
for(i in 1:length(authors_list)){
  # Counter
  if(i %% 500 == 0) print(i)
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
nrow(pubmed_data)
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
# 1999-2001 only have initials as names
table(pubmed_data$Year)
nrow(pubmed_data)
pubmed_data <- pubmed_data %>% filter(nchar(first_forename)>1)
pubmed_data <- pubmed_data %>% filter(nchar(last_forename)>1)
nrow(pubmed_data)
table(pubmed_data$Year)

# Names more frequent
table(pubmed_data$first_forename) %>% sort(decreasing = TRUE) %>% head(20)
table(pubmed_data$last_forename) %>% sort(decreasing = TRUE) %>% head(20)

# ----- Table to get genders -----

# This code is to do the minimum number of querys to GenderAPI:
names <- levels(factor(c(pubmed_data$first_forename, pubmed_data$last_forename)))
print(paste0("Number of articles: ", nrow(pubmed_data), ". Number of names: ", length(names), "."))

# Subset only 400 to get the gender for free
write.csv(names[1:400], file = "names.csv", row.names = FALSE)


# ----- GenderAPI -----

# Upload CSV to:
# https://gender-api.com/en/csv
# Save as: names_with_gender.csv
# This process can also be done with the API provided at GenderAPI

names_with_genders <- read.csv("names_with_genders.csv", header = TRUE, sep = ";")[, c(1, 3, 4)]
names(names_with_genders) <- c("names", "genders", "accuracy")

# Convert to character 
names_with_genders$names <- as.character(names_with_genders$names)

head(names_with_genders)

# Removing unknowns and empty gender:
nrow(names_with_genders)
names_with_genders <- names_with_genders %>% filter(genders != "unknown", genders != "")
names_with_genders$genders <- factor(names_with_genders$genders)
nrow(names_with_genders)

# Establish a threshold for accuracy (in percentage)
nrow(names_with_genders)
threshold <- 85 # For example
# See cases that are going to be removed
names_with_genders %>% filter(accuracy < threshold)
# Remove
names_with_genders <- names_with_genders %>% filter(accuracy >= threshold)
nrow(names_with_genders)

# ----- Updating gender in pubmed_data -----

# Create columns
pubmed_data$first_gender <- "none yet"
pubmed_data$last_gender <- "none yet"

# Merge to get the gender
pubmed_data$first_gender <- left_join(x = pubmed_data, y = names_with_genders, by = c("first_forename" = "names"))$genders
pubmed_data$last_gender <- left_join(x = pubmed_data, y = names_with_genders, by = c("last_forename" = "names"))$genders

# Table of genders found - only 400 names!
table(pubmed_data$first_gender, useNA = "always")
table(pubmed_data$last_gender, useNA = "always")

# Table of genders found - only 400 names!
table(pubmed_data$first_gender, pubmed_data$Year, useNA = "always")
table(pubmed_data$last_gender, pubmed_data$Year, useNA = "always")


# ----- Analysis by year - FIRST -----
df <- table(pubmed_data$first_gender, pubmed_data$Year)
df <- rbind(df, df[2,]/df[1,])
rownames(df)[3] <- "ratio_male_female"
df <- melt(df) %>% filter(X2 != 2018, X1 == "ratio_male_female")
df

ggplot(df) + 
  geom_line(aes(x = X2, y = value)) +
  geom_hline(yintercept = 1, lty = 2, col = "red") +
  ggtitle("Evolution of ratio male-female (First author) - Only 400 names with genders") +
  ylim(c(1, 2.5)) +
  ylab("Ratio male-female") + 
  xlab("Year") +
  theme_classic()


# ----- Analysis by year - LAST -----
df <- table(pubmed_data$last_gender, pubmed_data$Year)
df <- rbind(df, df[2,]/df[1,])
rownames(df)[3] <- "ratio_male_female"
df <- melt(df) %>% filter(X2 != 2018, X1 == "ratio_male_female")
df

ggplot(df) + 
  geom_line(aes(x = X2, y = value)) +
  geom_hline(yintercept = 1, lty = 2, col = "red") +
  ggtitle("Evolution of ratio male-female (Last author) - Only 400 names with genders") +
  ylim(c(1, 5)) +
  ylab("Ratio male-female") + 
  xlab("Year") +
  theme_classic()

# ----- Analysis by journal -----




