install.packages("easyPubMed")

library("easyPubMed")
nejm_on_pubmed <- get_pubmed_ids(nejm_query)
nejm_abstracts_xml <- fetch_pubmed_data(nejm_on_pubmed)
nejm_abstracts_list <- articles_to_list(nejm_abstracts_xml)
article_to_df(pubmedArticle = nejm_abstracts_list, autofill = TRUE, max_chars = 0, getAuthors = TRUE)
out.A <- batch_pubmed_download(pubmed_query_string = nejm_query, format = "xml", batch_size = 150, dest_file_prefix = "nejm")
custom_grep(curr_PM_record, tag = "LastName", format = "char")



my_query <- "(NEOPLASM[MESH]) AND (N ENGL J MED[JOURNAL]) NOT(LETTER[PTYP] OR CASE REPORTS[PTYP] OR COMMENT[SB] OR EDITORIAL[PTYP] OR REVIEW[PTYP]) AND (HUMANS[MESH TERMS]) AND (2000:2019[DP])"
my_entrez_id <- get_pubmed_ids(my_query)
my_abstracts_txt <- fetch_pubmed_data(my_entrez_id, format = "xml")

my_abstracts_xml <- fetch_pubmed_data(my_entrez_id)
class(my_abstracts_xml) 





##important for extract is from here (by journal) - NEJM 2000 to 2019
nejm_query <- "(NEOPLASM[MESH]) AND (N ENGL J MED[JOURNAL]) NOT(LETTER[PTYP] OR CASE REPORTS[PTYP] OR COMMENT[SB] OR EDITORIAL[PTYP] OR REVIEW[PTYP]) AND (HUMANS[MESH TERMS]) AND (2000:2009[DP]) NOT (1800/01/01:1999/12/31[Date - Publication]) NOT (2010/01/01:3000[Date - Publication])"
out.A <- batch_pubmed_download(pubmed_query_string = nejm_query, 
                               format = "xml", 
                               batch_size = 5000,
                               dest_file_prefix = "nejm_batch")

new_PM_file <- out.A[1]
new_PM_df <- table_articles_byAuth(pubmed_data = new_PM_file, included_authors = "all", max_chars = 0)
new_PM_df$address <- substr(new_PM_df$address, 1, 28)
new_PM_df$jabbrv <- substr(new_PM_df$jabbrv, 1, 9)
new_PM_df[1:5000, c("pmid", "year", "jabbrv", "lastname", "firstname")] 
write.csv(new_PM_df, file="nejm.csv", row.names=F)


##important for extract is from here (by journal) - Red Journal 2000 to 2009
red_query <- "(NEOPLASM[MESH]) AND (Int J Radiat Oncol Biol Phys[JOURNAL]) NOT(LETTER[PTYP] OR CASE REPORTS[PTYP] OR COMMENT[SB] OR EDITORIAL[PTYP] OR REVIEW[PTYP]) AND (HUMANS[MESH TERMS]) AND (2000:2009[DP]) NOT (1800/01/01:1999/12/31[Date - Publication]) NOT (2010/01/01:3000[Date - Publication])"
out.A <- batch_pubmed_download(pubmed_query_string = red_query, 
                               format = "xml", 
                               batch_size = 5000,
                               dest_file_prefix = "red_batch")

new_PM_file <- out.A[1]
new_PM_df <- table_articles_byAuth(pubmed_data = new_PM_file, included_authors = "all", max_chars = 0, autofill = TRUE, getKeywords = TRUE, encoding = "UTF8")
new_PM_df$address <- substr(new_PM_df$address, 1, 28)
new_PM_df$jabbrv <- substr(new_PM_df$jabbrv, 1, 9)
new_PM_df[1:5000, c("pmid", "year", "jabbrv", "lastname", "firstname")] 
write.csv(new_PM_df, file="red.csv", row.names=F)


######################




##important for extract is from here (by year)- e.g. all journals in year 2005
first_query <- '("Neoplasms"[Mesh]) AND (("N ENGL J MED"[JOURNAL]) OR ("JAMA"[JOURNAL]) OR ("Lancet"[JOURNAL]) OR ("BMJ"[JOURNAL]) OR ("J Clin Oncol"[JOURNAL]) OR ("JAMA Oncol"[JOURNAL]) OR ("Lancet Oncol"[JOURNAL]) OR ("J Natl Cancer Inst"[JOURNAL]) OR ("Cancer"[JOURNAL]) OR  ("ANN Oncol"[JOURNAL]) OR ("Int J Radiat Oncol Biol Phys"[JOURNAL]) OR ("Radiother Oncol"[JOURNAL])) NOT (Letter[ptyp] OR Case Reports[ptyp] OR Comment[sb] OR Editorial[ptyp] OR Review[ptyp] OR News[ptyp] OR Congress[ptyp]) AND humans[MeSH Terms] AND (2005/01/01:2005/12/31[Date - Publication]) NOT (1800/01/01:2004/12/31[Date - Publication]) NOT (2006/01/01:3000[Date - Publication])'
out.A <- batch_pubmed_download(pubmed_query_string = first_query, 
                               format = "xml", 
                               batch_size = 5000,
                               dest_file_prefix = "first_batch")

new_PM_file <- out.A[1]
new_PM_df <- table_articles_byAuth(pubmed_data = new_PM_file, 
                                   included_authors = "all", 
                                   max_chars = 0, 
                                   autofill = TRUE, 
                                   getKeywords = TRUE, 
                                   encoding = "UTF8")
new_PM_df$address <- substr(new_PM_df$address, 1, 28)
new_PM_df$jabbrv <- substr(new_PM_df$jabbrv, 1, 9)
new_PM_df[1:5000, c("pmid", "year", "jabbrv", "lastname", "firstname")] 
write.csv(new_PM_df, file="first.csv", row.names=F)