# In order not to overload the E-utility servers, NCBI recommends that users post no more
# than three URL requests per second and limit large jobs to either weekends or between
# 9:00 PM and 5:00 AM Eastern time during weekdays. Failure to comply with this policy may
# result in an IP address being blocked from accessing NCBI.

# ----- Packages -----

# devtools::install_github("skoval/RISmed")
library(RISmed)
library(dplyr)
library(ggplot2)
library(rjson)
library(httr)
library(reshape)

# ----- Get data from PubMed -----

# Define the terms of the query
terms_query <- '("Neoplasms"[Mesh])
  AND humans[MeSH Terms]
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
  AND Journal Article[ptyp]
  NOT (Letter[ptyp] OR Case Reports[ptyp] OR Comment[ptyp] OR Editorial[ptyp]
       OR Review[ptyp] OR News[ptyp] OR Congress[ptyp] OR Retracted Publication[ptyp]
       OR Published Erratum[ptyp] OR Biography[ptyp] OR Personal Narrative[ptyp]
       OR Book Illustrations[ptyp] OR Introductory Journal Article[ptyp]
       OR Guideline [ptyp] OR Practice guideline[ptyp]
       OR consensus development conferences[ptyp] OR Clinical Conference[ptyp]
       OR Address[ptyp] OR Duplicate Publication[ptyp] OR Interview[ptyp] OR Legal case[ptyp]) 
  AND (2002/01/01:2019/12/31[Date - Publication]
       OR 2002[Date - Publication] OR 2003[Date - Publication]
       OR 2004[Date - Publication] OR 2005[Date - Publication]
       OR 2006[Date - Publication] OR 2007[Date - Publication]
       OR 2008[Date - Publication] OR 2009[Date - Publication]
       OR 2010[Date - Publication] OR 2011[Date - Publication]
       OR 2012[Date - Publication] OR 2013[Date - Publication]
       OR 2014[Date - Publication] OR 2015[Date - Publication]
       OR 2016[Date - Publication] OR 2017[Date - Publication]
       OR 2018[Date - Publication] OR 2019[Date - Publication]
       )
  NOT (1800/01/01:2001/12/31[Date - Publication])
  NOT (2020/01/01:3000[Date - Publication])'

# Get information
query <- EUtilsSummary(terms_query, mindate = 2002, maxdate = 2019, retmax = 50000)

# Number of results (23/03/2020 - 42066 articles)
QueryCount(query)

# Get the data from PubMed
# Downloaded 23/03/2020 - 42066 articles
#records <- EUtilsGet(query, type = "efetch", db = "pubmed")
#save(records, file = "records.RData")
load("records.RData")

# ----- Data preprocessing  -----

# Convert authors from list to dataframe
authors_list <- Author(records)

# Initialise data.frame empty
authors <- data.frame(first_forename = rep("", length(authors_list)),
                      last_forename = rep("", length(authors_list)))

# Convert to character
class(authors[, 1]) <- "character"
class(authors[, 2]) <- "character"

# Extract names
for(i in 1:length(authors_list)){
  # Counter
  if(i %% 500 == 0) cat(".")
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

# keep articles if first and last names are not NA
nrow(pubmed_data)
pubmed_data <- pubmed_data %>% filter(is.na(first_forename) == FALSE)
pubmed_data <- pubmed_data %>% filter(is.na(last_forename) == FALSE)
nrow(pubmed_data)

# Lisa M -> Lisa
# Michael M -> Michael
for(i in 1:nrow(pubmed_data)){
  if(i %% 500 == 0) cat(".")
  pubmed_data$first_forename[i] <- strsplit(pubmed_data$first_forename[i], split = " ", fixed = TRUE)[[1]][1]
  pubmed_data$last_forename[i] <- strsplit(pubmed_data$last_forename[i], split = " ", fixed = TRUE)[[1]][1]
}

# keep only names with length > 1
table(pubmed_data$Year)
nrow(pubmed_data)
pubmed_data <- pubmed_data %>% filter(nchar(first_forename)>1)
pubmed_data <- pubmed_data %>% filter(nchar(last_forename)>1)
nrow(pubmed_data)
table(pubmed_data$Year)

# 20 names more frequent
table(pubmed_data$first_forename) %>% sort(decreasing = TRUE) %>% head(20)
table(pubmed_data$last_forename) %>% sort(decreasing = TRUE) %>% head(20)

# ----- Table to get genders -----

# This code is to do the minimum number of querys to GenderAPI:
names <- unique(c(pubmed_data$first_forename, pubmed_data$last_forename))
print(paste0("Number of articles: ", nrow(pubmed_data), ". Number of names: ", length(names), "."))

head(names)

# ----- GenderAPI - Creating files -----

# This code uses an API from GenderAPI to obtain a dataframe containing:
# - name
# - gender
# - samples (Number of samples used to obtain the gender)
# - accuracy (Percentage of accuracy of the gender)

## # API of GenderAPI
## api = "<INSERT API>"

## # Get the genders
## for(i in 1:length(names)){
##   # Small sleep between names
##   Sys.sleep(0.1)
##   # Counter
##   if(i %% 50 == 0) print(i)
##   # Build URL
##   url <- paste0("https://gender-api.com/get?name=", names[i], "&key=", api)
##   # Get the gender from GenderAPI
##   content <- url %>% GET %>% content %>% data.frame
##   name_i_with_gender <- content %>% select(names = name, gender, accuracy)
##   if(i == 1){
##     # Create table
##     names_with_genders <- name_i_with_gender
##   } else {
##     # Append to the table
##     names_with_genders <- rbind(names_with_genders, name_i_with_gender)
##   }
##   # Save raw result in case it's needed
##   write.csv(content, file = paste0("genderapi/", names[i], ".csv"))
## }

# ----- GenderAPI - Joining files -----

## # Slight modifications of characters, only in the name of the CSV:
## # Çağlar.csv -> Çaglar.csv
## # Doğangün.csv -> Dogangün.csv
## # Grażyna.csv -> Grazyna.csv
## # Jiří.csv -> Jirí.csv
## # Łukasz.csv -> Lukasz.csv
## 
## # Changes reflected also in pubmed_data (using UNICODE characters)
## names_with_issues <- c("Ça\U011Flar", paste0("Do", "\U011F", "ang\U00FCn"), "Gra\U017Cyna", "Ji\U0159í", "\U0141ukasz")
## print(names_with_issues) # See the names with issues
## names_corrected <- c("Çaglar", "Dogangün", "Grazyna", "Jirí", "Lukasz")
## for(i in 1:length(names_with_issues)){
##   pubmed_data$first_forename[pubmed_data$first_forename == names_with_issues[i]] <- names_corrected[i]
##   pubmed_data$last_forename[pubmed_data$last_forename == names_with_issues[i]] <- names_corrected[i]
## }
## 
## # Join all files produced with GenderAPI
## setwd("genderapi")
## files <- list.files()
## 
## for(i in 1:length(files)){
##   # Counter
##   if(i %% 500 == 0) cat(".")
##   file_i <- read.csv(files[i])
##   if(i == 1) names_with_genders <- file_i
##   else names_with_genders <- rbind(names_with_genders, file_i)
## }
## 
## setwd("..")

## # Some names are duplicated: (e.g. Ayşe is processed through GenderAPI like "Ayse", that was present already)
## # We remove duplicates
## names_with_genders <- unique(names_with_genders)
## 
## head(names_with_genders)
## save(names_with_genders, file = "names_with_genders.RData")
load("names_with_genders.RData")

# ----- Processing genders -----

# Convert name to character 
names_with_genders$name <- as.character(names_with_genders$name)
head(names_with_genders)

# Removing unknowns and empty gender:
table(names_with_genders$gender, useNA = "always")
nrow(names_with_genders)
names_with_genders <- names_with_genders %>% filter(gender != "unknown", gender != "")
names_with_genders$gender <- factor(names_with_genders$gender)
nrow(names_with_genders)

# Establish a threshold for accuracy (in percentage)
nrow(names_with_genders)
threshold <- 75
# See names + genders that are going to be removed
names_with_genders %>% filter(accuracy < threshold) %>% head(10)
# Remove
names_with_genders <- names_with_genders %>% filter(accuracy >= threshold)
nrow(names_with_genders)

# Establish a threshold for samples
nrow(names_with_genders)
threshold <- 10 # For example
# See names + genders that are going to be removed
names_with_genders %>% filter(samples < threshold) %>% head(10)
# Remove
names_with_genders <- names_with_genders %>% filter(accuracy >= threshold)
nrow(names_with_genders)


# ----- Updating gender in pubmed_data -----

# Remove capitals from names
pubmed_data$first_forename <- tolower(pubmed_data$first_forename)
pubmed_data$last_forename <- tolower(pubmed_data$last_forename)

# Create columns
pubmed_data$first_gender <- NULL
pubmed_data$last_gender <- NULL

# Merge to get the gender
pubmed_data$first_gender <- left_join(x = pubmed_data, y = names_with_genders,
                                      by = c("first_forename" = "name"))$gender %>% as.character
pubmed_data$last_gender <- left_join(x = pubmed_data, y = names_with_genders,
                                     by = c("last_forename" = "name"))$gender %>% as.character

# ----- Master data file -----

# Create
master <- pubmed_data %>%
  group_by(Year, Journal) %>%
  summarise() %>% 
  mutate(first_male = 0,
         first_female = 0,
         last_male = 0,
         last_female = 0) %>%
  as.data.frame

# Auxiliar tables
pubmed_data_F <- pubmed_data %>%
  group_by(Year, Journal, first_gender) %>%
  summarise(n = n()) %>%
  as.data.frame
head(pubmed_data_F)

# Auxiliar tables
pubmed_data_L <- pubmed_data %>%
  group_by(Year, Journal, last_gender) %>%
  summarise(n = n()) %>%
  as.data.frame
head(pubmed_data_L)

# Update - first
for(i in 1:nrow(pubmed_data_F)){
  if(i %% 50 == 0) cat(".")
  for(j in 1:nrow(master)){
    if(pubmed_data_F[i, "Year"] == master[j, "Year"] & pubmed_data_F[i, "Journal"] == master[j, "Journal"]){
      if(is.na(pubmed_data_F[i, "first_gender"]) == FALSE){
        if(pubmed_data_F[i, "first_gender"] == "male") master[j, "first_male"] = pubmed_data_F[i, "n"]
        if(pubmed_data_F[i, "first_gender"] == "female") master[j, "first_female"] = pubmed_data_F[i, "n"]
      }
      break()
    }
  }
}

# Update - last
for(i in 1:nrow(pubmed_data_L)){
  if(i %% 50 == 0) cat(".")
  for(j in 1:nrow(master)){
    if(pubmed_data_L[i, "Year"] == master[j, "Year"] & pubmed_data_L[i, "Journal"] == master[j, "Journal"]){
      if(is.na(pubmed_data_L[i, "last_gender"]) == FALSE){
        if(pubmed_data_L[i, "last_gender"] == "male") master[j, "last_male"] = pubmed_data_L[i, "n"]
        if(pubmed_data_L[i, "last_gender"] == "female") master[j, "last_female"] = pubmed_data_L[i, "n"]
      }
      break()
    }
  }
}

head(master)

# Export
save(master, file = "master.RData")
write.csv(master, file = "master.csv", row.names = FALSE)

# ----- Results - Overall male-female ratio -----

table(pubmed_data$first_gender, useNA = "always") # Ratio: 21197/11490 = 1.84
table(pubmed_data$last_gender, useNA = "always")  # Ratio: 25477/7728  = 3.30

# ----- Figures - Ratio male/female -----

# First author
df <- table(pubmed_data$first_gender, pubmed_data$Year)
df <- rbind(df, df["male",]/df["female",])
rownames(df)[3] <- "ratio_male_female"
df <- melt(df) %>% filter(X1 == "ratio_male_female")
names(df) <- c("ratio", "year", "value")
df

ggplot(df) + 
  geom_line(aes(x = year, y = value), size = 0.75) +
  geom_hline(yintercept = 1, lty = 2, col = "red", size = 0.5) +
  ggtitle("Evolution of ratio male-female (First author)") +
  ylab("Ratio male-female") + 
  ylim(c(0, 5)) +
  scale_x_continuous("Year", breaks = seq(2002, 2020, 2)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(filename = "plots/r_first.png", width = 200, height = 200, units = "mm")

# Last author
df <- table(pubmed_data$last_gender, pubmed_data$Year)
df <- rbind(df, df["male",]/df["female",])
rownames(df)[3] <- "ratio_male_female"
df <- melt(df) %>% filter(X1 == "ratio_male_female")
names(df) <- c("ratio", "year", "value")
df

ggplot(df) + 
  geom_line(aes(x = year, y = value), size = 0.75) +
  geom_hline(yintercept = 1, lty = 2, col = "red", size = 0.5) +
  ggtitle("Evolution of ratio male-female (Last author)") +
  ylab("Ratio male-female") + 
  ylim(c(0, 5)) +
  scale_x_continuous("Year", breaks = seq(2002, 2020, 2)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "plots/r_last.png", width = 200, height = 200, units = "mm")

# ----- Figures - Ratio male/female by journal -----

# Only 4 journals with more articles
table(pubmed_data$Journal) %>% sort(decreasing = TRUE)
journals <- table(pubmed_data$Journal) %>% sort(decreasing = TRUE) %>% head(4) %>% names()

# First author
df <- pubmed_data %>% 
  filter(is.na(first_gender) == FALSE) %>%
  filter(is.na(Journal) == FALSE) %>%
  filter(Journal %in% journals) %>%
  mutate(first_gender = as.character(first_gender)) %>%
  mutate(Journal = as.character(Journal)) %>%
  group_by(Year, Journal, first_gender) %>%
  summarise(n = n()) %>% 
  mutate(n = as.double(n)) %>%
  as.data.frame()

# Calculate ratio
for(i in 2002:2019){
  for(j in 1:length(journals)){
    ratio <- df[df$Year == i & df$Journal == journals[j] & df$first_gender == "male", "n"] / 
      df[df$Year == i & df$Journal == journals[j] & df$first_gender == "female", "n"]
    aux <- data.frame(i, journals[j], "ratio", ratio)
    names(aux) <- names(df)
    df <- rbind(df, aux)
  }
}

df <- df %>% filter(first_gender == "ratio")
names(df)[4] <- "ratio"
head(df)

ggplot(df) + 
  geom_line(aes(x = Year, y = ratio), size = 0.75) +
  geom_hline(yintercept = 1, lty = 2, col = "red", size = 0.5) +
  ggtitle("Evolution of ratio male-female (First author)") +
  ylab("Ratio male-female") + 
  scale_x_continuous("Year", breaks = seq(2002, 2020, 2)) +
  facet_wrap(vars(Journal)) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "plots/r_first_by_journal.png", width = 200, height = 200, units = "mm")

# Last author
df <- pubmed_data %>% 
  filter(is.na(last_gender) == FALSE) %>%
  filter(is.na(Journal) == FALSE) %>%
  filter(Journal %in% journals) %>%
  mutate(last_gender = as.character(last_gender)) %>%
  mutate(Journal = as.character(Journal)) %>%
  group_by(Year, Journal, last_gender) %>%
  summarise(n = n()) %>% 
  mutate(n = as.double(n)) %>%
  as.data.frame()

# Calculate ratio
for(i in 2002:2019){
  for(j in 1:length(journals)){
    ratio <- df[df$Year == i & df$Journal == journals[j] & df$last_gender == "male", "n"] / 
      df[df$Year == i & df$Journal == journals[j] & df$last_gender == "female", "n"]
    aux <- data.frame(i, journals[j], "ratio", ratio)
    names(aux) <- names(df)
    df <- rbind(df, aux)
  }
}

df <- df %>% filter(last_gender == "ratio")
names(df)[4] <- "ratio"
head(df)

ggplot(df) + 
  geom_line(aes(x = Year, y = ratio), size = 0.75) +
  geom_hline(yintercept = 1, lty = 2, col = "red", size = 0.5) +
  ggtitle("Evolution of ratio male-female (Last author)") +
  ylab("Ratio male-female") + 
  scale_x_continuous("Year", breaks = seq(2002, 2020, 2)) +
  facet_wrap(vars(Journal)) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "plots/r_last_by_journal.png", width = 200, height = 200, units = "mm")

# ----- Figures - Number -----

# First author
df <- table(pubmed_data$first_gender, pubmed_data$Year) %>% melt
names(df) <- c("gender", "year", "value")
df

ggplot(df) + 
  geom_line(aes(x = year, y = value, color = gender), size = 0.75) +
  geom_hline(yintercept = 1, lty = 2, col = "red", size = 0.5) +
  ggtitle("Evolution of number of articles by gender (First author)") +
  ylab("Number of articles") + 
  scale_x_continuous("Year", breaks = seq(2002, 2020, 2)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(filename = "plots/n_first.png", width = 200, height = 200, units = "mm")


# Last author
df <- table(pubmed_data$last_gender, pubmed_data$Year) %>% melt
names(df) <- c("gender", "year", "value")
df

ggplot(df) + 
  geom_line(aes(x = year, y = value, color = gender), size = 0.75) +
  geom_hline(yintercept = 1, lty = 2, col = "red", size = 0.5) +
  ggtitle("Evolution of number of articles by gender (Last author)") +
  ylab("Number of articles") + 
  scale_x_continuous("Year", breaks = seq(2002, 2020, 2)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(filename = "plots/n_last.png", width = 200, height = 200, units = "mm")

# Trends Ratio male/female over time (accouting for overdispersion)
library(sandwich)

# First author
trendF <- glm(n ~ I(first_gender) + Year + first_gender:Year, family=poisson(link="log"), data=pubmed_data_F)
summary(trendF)
confint(trendF)
# Robust Standard Errors (accouting for overdispersion)
cov.F <- vcovHC(trendF, type="HC0")
stdf.err <- sqrt(diag(cov.F))
rf.est <- cbind(Estimate= coef(trendF), "Robust SE" = stdf.err,
                "Pr(>|z|)" = 2 * pnorm(abs(coef(trendF)/stdf.err), lower.tail=FALSE),
                LL = coef(trendF) - 1.96 * stdf.err,
                UL = coef(trendF) + 1.96 * stdf.err)
rf.est

#Second author
trendL <- glm(n ~ I(last_gender) + Year + last_gender:Year, family=poisson(link="log"), data=pubmed_data_L)
summary(trendL)
confint(trendL)

# Robust Standard Errors (accouting for overdispersion)
cov.L <- vcovHC(trendF, type="HC0")
stdl.err <- sqrt(diag(cov.L))
rl.est <- cbind(Estimate= coef(trendL), "Robust SE" = stdl.err,
                "Pr(>|z|)" = 2 * pnorm(abs(coef(trendL)/stdl.err), lower.tail=FALSE),
                LL = coef(trendL) - 1.96 * stdl.err,
                UL = coef(trendL) + 1.96 * stdl.err)
rl.est
