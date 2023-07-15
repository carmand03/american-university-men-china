#  American University Men's postgraduate career and collaborations

## A Study based on Who's Who of China 1936 (same year as AUM) 
# -> limitation: contain only Chinese 
# -> to supplement with Men of Shanghai/North China 1935

# Experiment with various methods

  # 1. Extract full text of Who's Who 1936 and search names in titles 
  # 2. Search concordance "American University" to find members of AUC (limitations: their affiliation to the club may not be mentioned)
  # 3. Search list of universities 
  # 4. Search universities one by one (case study of most important universities, based on the number of curricula)

save.image("auc_whos.RData")
load(file = "auc_whos.RData")

# Load packages 

library(readr)
library(histtext)
library(tidyverse)
library(quanteda)

# load data

aucv2 <- read_csv("aucv2.csv") # last version of dataset (with corrections on names)


# 1. Retrieve all biographies 

# retrieve names of corpus and fields 
histtext::list_filter_fields("imh-en")
histtext::list_possible_filters("imh-en", "book")
book_imh <- histtext::list_possible_filters("imh-en", "bookno") # Who's Who China 1936 is bookno 14 

# retrieve all entries in Who's Who series

search_who_en <- histtext::search_documents_ex('*', corpus = "imh-en", filter_query = list(bookno = "14"))
# search_who_en <- histtext::search_documents_ex('*', corpus = "imh-en", filter_query = list(book = "Who's Who in China ")) # does not work

# retrieve full text 

search_who_en_ftext <- get_documents(search_who_en, corpus = "imh-en", batch_size = 10, verbose = TRUE) # 1477 biographies

# count length of biographies 

search_who_en_ftext <- search_who_en_ftext %>% mutate(length = ntoken(Text))

# identify American University men based on 1936 roster

# create list of names (Chinese) 

name_list <- aucv2 %>% select(Name_zh, Name_full) %>% unique() %>% drop_na()
name_vec <- paste(name_list$Name_zh, sep = "", collapse = "|")

# search list of names 
who_aum <- search_who_en_ftext %>% mutate(aum = str_extract(Text, name_vec)) %>% drop_na(aum) # 44 results

# limitation: 7 individuals have no Chinese names in the titles of their biographies, and 5 not in the text of their biographies

# extract year and place of birth 

# YEAR (first 4 digits)

who_aum_year <- who_aum %>% mutate(birthyear = str_extract(Text, "\\d{4}")) # check manually 

# Extract place of birth (using NER - LOCATION)

who_aum_id <- who_aum %>% select(DocId) 
who_aum_ner <- ner_on_corpus(who_aum_id, "imh-en")
who_aum_location <- who_aum_ner %>% filter(Type %in% c("LOC", "GPE"))

# alternative : concordance on "born at" or "native of" 

who_aum_born <- search_concordance_on_df(
  who_aum,
  "born at",
  context_size = 47,
  search_column = "Text",
  id_column = "DocId")

# extract everything before  ","

who_aum_born <- who_aum_born %>% mutate(town=str_extract(After,"[^,]+,"))

# use "town" to retrieve "province"

who_aum_born <- who_aum_born %>% mutate(Province = str_remove_all(After, town)) %>% 
  mutate(Province = str_trim(Province)) %>% 
  mutate(Province = str_remove_all(Province, "[:digit:]")) %>% 
  mutate(Province_clean=str_extract(Province,"[^,]+,")) %>% 
  mutate(Province_clean = str_remove_all(Province_clean,","))

# clean "town" 

who_aum_born <- who_aum_born %>% mutate(town = str_remove_all(town,",")) %>% 
  mutate(town = str_remove_all(town,"：")) %>% 
  mutate(town = str_remove_all(town,"\\d{4}")) %>% 
  mutate(town = str_squish(town)) %>% 
  mutate(town = str_replace(town,"- ", "-"))

# NATIVE OF 

who_aum_native <- search_concordance_on_df(
  who_aum,
  "native of",
  context_size = 47,
  search_column = "Text",
  id_column = "DocId")


# save results

write.csv(search_who_en_ftext, "who_1936_ftext.csv")
write.csv(who_aum_year, "who_aum_1936.csv")
write.csv(who_aum_born, "born.csv")

# reimport clean data (date and place of birth)

library(readr)

birth_clean <- read_delim("birth_clean.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
birthplace_clean <- read_delim("birthplace_clean.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE)


# join date and place

birth_data <- left_join(birth_clean, birthplace_clean)
who_length <- who_aum %>% select(DocId, length) 
birth_data <- left_join(birth_data, who_length)
birth_data <- birth_data %>% mutate(age = (1936-birthyear))

# load cleaned data

library(readr)
who_birthdata <- read_delim("auc_whos_who/who_birthdata.csv",
delim = ";", escape_double = FALSE, trim_ws = TRUE)


# plot age and length of biographies
library(ggplot2)
library(hrbrthemes)

p <- ggplot(who_birthdata, aes(x=age, y=length)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum() + 
  labs(title = "American University Men in  Who's Who of China",
       subtitle = "Length of biography in relation to age",
       x = "Age (years)", 
       y = "Length (words)", 
       caption = "Based on Who's Who of China, 1936")

ggplot(who_birthdata, aes(x=age, y=length)) +
  geom_jitter() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum() + 
  labs(title = "American University Men in  Who's Who of China",
       subtitle = "Length of biography in relation to age",
       x = "Age (years)", 
       y = "Length (words)", 
       caption = "Based on Who's Who of China, 1936")

library(ggExtra)

# The mtcars dataset is proposed in R
head(mtcars)

# classic plot :
p <- ggplot(birth_data, aes(x=age, y=length, color=age, size=age)) +
  geom_point() +
  geom_jitter() +
  theme(legend.position="none")

p1 <- ggMarginal(p, type="histogram") # DOES NOT WORK

# births over time

who_birthdata %>%
  ggplot( aes(x=birthyear)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("American University Men: Year of birth") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )

who_birthdata %>%
  ggplot( aes(x=birthyear)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("American University Men: Year of birth") +
  theme_ipsum() +
  labs(title = "American University Men of Shanghai",
       subtitle = "Year of birth",
       x = "Year", 
       y = "Number of births", 
       caption = "Based on Who's Who of China, 1936")


who_birthdata %>%
  ggplot( aes(x=age)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  theme_ipsum() +
  labs(title = "American University Men of Shanghai",
       subtitle = "Generational effect",
       x = "Age in 1936", 
       y = "Number of individuals", 
       caption = "Based on Who's Who of China, 1936")

# export data 

write.csv(birth_data, "who_birthdata.csv")

####################################################################   

# find members of AUC in Who's Who 

auc_conc_1936 <- histtext::search_concordance_ex('"american university"', 
                                                          corpus = "imh-en", context_size = 200, 
                                                          filter_query = list(bookno = "14")) 

# remove non members 

auc_filtered <- auc_conc_1936 %>% filter(!DocId %in% c("imh-34-885", "imh-34-1286", "imh-34-883"))

duplicates <- intersect(auc_filtered$DocId, who_aum$DocId)

duplicates <- as.data.frame(duplicates) %>% rename(DocId = duplicates) # 4 duplicates, 5 new names

# add newly found names to the who_aum dataset 

auc_new <- auc_filtered %>% filter(!DocId %in% duplicates$DocId) # remove duplicates
auc_new_ft <- get_documents(auc_new, corpus = "imh-en", batch_size = 10, verbose = TRUE) # retrieve full text 
auc_new_ft <- auc_new_ft %>% mutate(length = ntoken(Text)) # compute length 
# add aum variable (extract Chinese characters)
auc_new_ft <- auc_new_ft %>%  tidyr::extract(Title, c("aum"), "(\\p{Han}+)", remove=FALSE) %>% relocate(aum, .after = length)

# bind with previous data

auc_who_1936 <- bind_rows(auc_new_ft, who_aum) # 49 
auc_who_1936 <- auc_who_1936 %>% unique() # 48

# save results

write.csv(auc_who_1936, "auc_who_1936_add.csv")

####################################################################  

# 3. Search list of universities 

# create list of universities 

univ_list <- aucv2 %>% group_by(University) %>% count()
univ_vec <- paste(univ_list$University, sep = "", collapse = "|") # convert into vector

# search list of universities 
who_aum_univ <- search_who_en_ftext %>% mutate(univ = str_extract_all(Text, univ_vec)) %>% filter(!univ == "character(0)") # 527 results = 527 liumei in Who's Who

# separate universities in case of multiple colleges

who_aum_univ <- with(who_aum_univ, who_aum_univ[!(univ[,1] == "" | is.na(univ[,1])), ]) # 527 liumei = 35.7%

# save results 

write.csv(who_aum_univ, "who_aum_univ.csv")


####################################################################################### Extract NEs

# Extract named entities (organizations) from their biographies

# from AUM (48 individuals)

auc_who_1936_id <- auc_who_1936 %>% select(DocId)

ner_auc_who_1936 <- ner_on_corpus(auc_who_1936_id, corpus = "imh-en") # 2872
org_auc_who_1936 <- ner_auc_who_1936 %>% filter(Type == "ORG") # 1016 organizations

org_auc_who_1936 <- org_auc_who_1936 %>% group_by(DocId) %>% add_tally()

# from all liumei (527 individuals)

who_aum_univ_id <- who_aum_univ %>% select(DocId)

ner_who_aum_univ <- ner_on_corpus(who_aum_univ_id, corpus = "imh-en") # 24754
org_who_aum_univ <- ner_who_aum_univ %>% filter(Type == "ORG") # 8178 organizations

org_who_aum_univ <- org_who_aum_univ %>% group_by(DocId) %>% add_tally()

## To clean/standardize # see script tsinghua17_ner_2

# Join with names 

id_names <- auc_who_1936 %>% select(DocId, aum) %>% rename(Name = aum)
org_auc_who_1936 <- inner_join(id_names, org_auc_who_1936)
name_list <- name_list  %>% rename(Name = Name_zh)
org_auc_who_1936 <- left_join(org_auc_who_1936, name_list)

# save results 

write.csv(org_auc_who_1936, "org_auc_who_1936.csv")
write.csv(org_who_aum_univ, "org_univ_who_1936.csv")

####################################################################  

# Case studies of top universities

# we start from univ_list with number of curricula -> we select universities with at least 10 

# Columbia n1

columbia_doc <- histtext::search_documents_ex("Columbia", corpus = "imh-en", filter_query = list(bookno = "14")) # 145 graduates 

columbia_conc <- histtext::search_concordance_ex('"Columbia"', 
                                                corpus = "imh-en", context_size = 200, 
                                                filter_query = list(bookno = "14")) 


# extract full text

columbia_ft <- get_documents(columbia_doc, corpus = "imh-en", batch_size = 10, verbose = TRUE)
columbia_ft <- columbia_ft %>% mutate(length = ntoken(Text)) 
columbia_ft <- columbia_ft %>%  tidyr::extract(Text, c("Name"), "(\\p{Han}+)", remove=FALSE)

# identify aum 1936 (10)
columbia_ft <- columbia_ft %>%  mutate(aum = str_extract(Text, name_vec))  
sum(!is.na(columbia_ft$aum)) # 10 AUM 

columbia_id <- columbia_ft %>% select(DocId, Name)

# extract entities 

columbia_ner <- ner_on_corpus(columbia_doc, corpus = "imh-en") # 7090
columbia_org <- columbia_ner %>% filter(Type == "ORG") # 2393

columbia_org <- columbia_org %>% group_by(DocId) %>% add_tally()

columbia_org <- inner_join(columbia_id, columbia_org)

# Harvard 2 

harvard_doc <- histtext::search_documents_ex("harvard", corpus = "imh-en", filter_query = list(bookno = "14")) # 61

# Penn 3

penn_doc <- histtext::search_documents_ex("pennsylvania", corpus = "imh-en", filter_query = list(bookno = "14")) # 23 graduates

penn_conc <- histtext::search_concordance_ex('"pennsylvania"', 
                                                 corpus = "imh-en", context_size = 200, 
                                                 filter_query = list(bookno = "14")) 
 
penn_doc_filtered <- penn_doc %>% filter(DocId != "imh-34-544") # 23 graduates


# Michigan 4 

michigan_doc <- histtext::search_documents_ex("michigan", corpus = "imh-en", filter_query = list(bookno = "14")) ## 33 

# others 
california_doc <- histtext::search_documents_ex("california", corpus = "imh-en", filter_query = list(bookno = "14")) ## 23 
california_conc <- histtext::search_concordance_ex('"california"', 
                                             corpus = "imh-en", context_size = 200, 
                                             filter_query = list(bookno = "14")) 

california_doc_filtered <- california_doc %>% filter(!DocId %in% c("imh-34-882", "imh-34-1284", "imh-34-513", "imh-34-541", "imh-34-594", "imh-34-738", "imh-34-744")) # 35 graduates

# to remove (not university): imh-34-882 imh-34-1284 	imh-34-513 imh-34-541 imh-34-594 imh-34-738 imh-34-744


cornell_doc <- histtext::search_documents_ex("cornell", corpus = "imh-en", filter_query = list(bookno = "14")) ## 41 

chicago_doc <- histtext::search_documents_ex("chicago", corpus = "imh-en", filter_query = list(bookno = "14")) ## 33 
chicago_conc <- histtext::search_concordance_ex('"chicago"', 
                                                   corpus = "imh-en", context_size = 200, 
                                                   filter_query = list(bookno = "14")) 

yale_doc <- histtext::search_documents_ex("yale", corpus = "imh-en", filter_query = list(bookno = "14")) ## 53 
princeton_doc <- histtext::search_documents_ex("princeton", corpus = "imh-en", filter_query = list(bookno = "14")) ## 18 



# save results 

write.csv(columbia_ft, "~/Places AUC/auc_whos_who/columbia_ft.csv")
write.csv(columbia_org, "~/Places AUC/auc_whos_who/columbia_org.csv")

####################################################################  
# multiple query directly in corpus 

name_vec <-  name_list %>% mutate(Queries=str_glue('"{Name_zh}"'))

# create function for multiple queries 

multiple_search <- function(queries, corpus) {
  results <- histtext::search_documents_ex(queries[1], corpus) %>%
    mutate(Q=queries[1])
  for(q in queries){
    new_result <- histtext::search_documents_ex(q, corpus) %>%
      mutate(Q=q)
    results <- dplyr::bind_rows(results, new_result)
  }
  distinct(results)

}

#################################################################### 

# find places in cleaned list of org

# load data and typology

library(readr)
aum_org <- read_csv("aum_org.csv")
Typo <- read_csv("Typo.csv")

aum_org <- inner_join(aum_org, Typo) # join data with typo
aum_org <- aum_org %>% unique() # remove duplicates
aum_org <- left_join(aum_org, name_list) 
aum_org <- aum_org %>% relocate(Name_full, .after = Name)
aum_org <- aum_org %>% unique()
aum_org <- aum_org %>% drop_na(Name)

aum_org <- as.data.frame(aum_org)
class(aum_org)

aum_org_filtered <- aum_org %>% filter(N>1) # retain most frequent organizations

# filter by type of organisations 

aum_edu <- zest_filtered %>% filter(Type1 == "Education")
aum_asso <- zest_filtered %>% filter(Type1 == "Association")
aum_non_asso <- zest_filtered %>% filter(!Type1 %in% c("Association", "Education"))

# search places/k places

library(Places)

Result1 <- places(aum_edu, "Name", "Organization") 
result1df <- as.data.frame(Result1$PlacesData)
# 45 elements and 33 sets
# 42 places, 2 involved more than one student (2), including one with 2 with 2 students and 2 colleges, one with 3 students and 3 universities
result1df %>% filter(NbElements>1) 

# ASSOCIATIONS : 24 associative places (based on 27 individuals and 26 associations)
Result2 <- places(aum_asso, "Name", "Organization") 
result2df <- as.data.frame(Result2$PlacesData)
result2df %>% filter(NbElements>1) 

# SALARIED OCCUPATIONS : 46 professional places, based on 47 individuals and 83 sets 
Result3 <- places(aum_non_asso, "Name", "Organization") 
result3df <- as.data.frame(Result3$PlacesData)
result3df %>% filter(NbElements>1) 

# ALL ORGANIZATIONS (NOT FILTERED)

aum_org <- as.data.frame(aum_org)
Result4 <- places(aum_org, "Name", "Organization") 
Result4df <- as.data.frame(Result4$PlacesData)
Result4df %>% filter(NbElements>1) # NONE

# ALL ORGANIZATIONS (FILTERED)

aum_org_filtered <- as.data.frame(aum_org_filtered)
Result5 <- places(aum_org_filtered, "Name", "Organization") 
Result5df <- as.data.frame(Result5$PlacesData)
Result5df %>% filter(NbElements>1) 

# Networks of places

### EDUCATION ONLY 

# create the adjacency matrix
bimod<-table(Result1$Edgelist$Places, Result1$Edgelist$Set) 
PlacesMat<-bimod %*% t(bimod)
diag(PlacesMat)<-0 

# create the adjacency matrix
bimod2<-table(Result1$Edgelist$Set, Result1$Edgelist$Places)
PlacesMat2<-bimod2 %*% t(bimod2)
diag(PlacesMat2)<-0

# build network from adjacency matrix with igraph

library(igraph)
Pla1Net<-graph_from_adjacency_matrix(PlacesMat, mode="undirected", weighted = TRUE)
Pla2Net<-graph_from_adjacency_matrix(PlacesMat2, mode="undirected", weighted = TRUE)

plot(Pla1Net, vertex.size = 8, 
     vertex.color = "orange", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.6, 
     main="Network of places linked by organizations")


plot(Pla2Net, vertex.size = 8, 
     vertex.color = "light blue", 
     vertex.shape = "square", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.6, 
     main="Network of organizations linked by places")

plot(Pla1Net, vertex.size = degree(Pla1Net)*0.5, 
     vertex.color = "orange", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.8, 
     main="Network of places linked by organizations")

plot(Pla1Net, vertex.size = betweenness(Pla1Net)*0.5, 
     vertex.color = "orange", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.8, 
     main="Network of places linked by organizations")


plot(Pla2Net, vertex.size = degree(Pla2Net)*0.5, 
     vertex.color = "light blue", 
     vertex.shape = "square", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.6, 
     main="Network of organizations linked by places")

plot(Pla2Net, vertex.size = betweenness(Pla2Net)*0.5, 
     vertex.color = "light blue", 
     vertex.shape = "square", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.6, 
     main="Network of organizations linked by places")


# alternative:  filter by university

columbia <- aum_org %>% filter(Organization == "Columbia University") %>% unique() # 13 individuals
aum_org_columbia <- aum_org %>% filter(Name %in% columbia$Name) %>% drop_na(Organization) # 225 affiliations
aum_org_columbia %>% group_by(Organization) %>% count(sort = TRUE) # 170 unique organizations
aum_org_columbia <- aum_org_columbia %>% distinct(Name, Organization, Type1) %>% group_by(Organization, Type1) %>% add_tally()

