# Search for Chinese and American graduates in "Men of North China" 1935

# Load packages 

library(readr)
library(histtext)
library(tidyverse)
library(quanteda)
library(hrbrthemes)
library(viridis)
library(ggExtra)

# load data

aucv2 <- read_csv("aucv2.csv") # last version of dataset (with corrections on names)


# 1. Retrieve all biographies 

# retrieve names of corpus and fields 
histtext::list_filter_fields("imh-en")
histtext::list_possible_filters("imh-en", "book")
histtext::list_possible_filters("imh-en", "book")
book_imh <- histtext::list_possible_filters("imh-en", "bookno") # Men of Shanghai and North China 1935 is bookno 25 


# retrieve all entries in Who's Who series

search_men_en <- histtext::search_documents_ex('*', corpus = "imh-en", filter_query = list(bookno = "25"))

# search_who_en <- histtext::search_documents_ex('*', corpus = "imh-en", filter_query = list(book = "Men of Shanghai and North China")) # does not work

# retrieve full text 

search_men_en_ftext <- get_documents(search_men_en, corpus = "imh-en", batch_size = 10, verbose = TRUE) # 527 biographies

# count length of biographies 

search_men_en_ftext <- search_men_en_ftext %>% mutate(length = ntoken(Text))

# identify American University men based on 1936 roster

# create list of names (English) 

name_en_list <- aucv2 %>% select(Name_full, SurName, Name_zh) %>% unique () # 282
name_en <- name_en_list %>% select(SurName) %>% unique() # 155
name_en_vec <- paste(name_en_list$SurName, sep = "", collapse = "|")


firstname_list <- aucv2 %>% select(Name_full, FirstName) %>% unique() 
first_en_vec <- paste(firstname_list$FirstName, sep = "", collapse = "|")

# search list of names 
men_aum <- search_men_en_ftext %>% mutate(Title = str_to_title(Title)) %>% mutate(SurName = str_extract(Title, name_en_vec))  %>% drop_na(SurName) # 240 results
men_aum_list <- inner_join(men_aum, name_en_list)
men_aum_list <- men_aum_list %>% unique()

men_aum_us <- men_aum %>% mutate(US = str_extract(Text, "America")) %>% drop_na(US)
men_aum_us2 <- men_aum %>% mutate(US = str_extract(Text, "United States")) %>% drop_na(US)
men_aum_us3 <- men_aum %>% mutate(US = str_extract(Text, "American")) %>% drop_na(US)
men_aum_us_all <- bind_rows(men_aum_us, men_aum_us2) 
men_aum_us_all <- bind_rows(men_aum_us_all, men_aum_us3) 
men_aum_us_list <- men_aum_us_all %>% distinct(DocId, SurName, Title)
men_aum_us_list_expand <- inner_join(men_aum_us_list, name_en_list)

men_aum_us_list2 <- men_aum_us_list %>% mutate(First = str_extract(Title, first_en_vec))

men_aum_club <- men_aum %>% mutate(AUC = str_extract(Text, "American University Club")) %>% 
men_aum_club2 <- men_aum %>% mutate(AUC = str_extract(Text, "American University Men")) 

# extract year and place of birth 

# YEAR (first 4 digits)

men_aum_year <- men_aum %>% mutate(birthyear = str_extract(Text, "\\d{4}")) # check manually 

# reimport list of disambiguated names (members of AUC) # 33 biographies 

library(readr)
men_list <- read_delim("men_list.csv", delim = ";", 
                       escape_double = FALSE, trim_ws = TRUE) 

# 33 individuals, 19 Chinese and 14 Americans
men_list %>% group_by(Nationality) %>% count(sort = TRUE)

# retrieve full text 

men_list_ft <- get_documents(men_list, "imh-en")

# year of birth 

men_born <- search_concordance_on_df(
  men_list_ft,
  "born",
  context_size = 140,
  search_column = "Text",
  id_column = "DocId")

# place of birth 

men_born2 <- search_concordance_on_df(
  men_born,
  "at ",
  context_size = 50,
  search_column = "After",
  id_column = "DocId")

men_born3 <- search_concordance_on_df(
  men_born,
  " in ",
  context_size = 50,
  search_column = "After",
  id_column = "DocId")

men_born4 <- bind_rows(men_born2, men_born3) 

men_born4 %>% distinct(DocId) # 2 missing : imh-43-224 imh-43-300

setdiff(men_list$DocId, men_born4$DocId)

men_born_all <- men_born4 %>% mutate(birthyear = str_extract(Before, "\\d{4}")) 

# extract birthplace 

men_born_all <- men_born_all %>% mutate(Town=str_extract(After,"[^,]+,"),
           Province=str_extract(After,",.*")) %>% 
  mutate(Town = str_remove_all(Town,",")) %>% 
  mutate(Province = str_remove_all(Province,", "))

men_born_all <- men_born_all %>% mutate(Province=str_replace(After,"[^,]+, ",""),
                                        Town=str_replace(After,",.*",""))


### MEN OF CHINA AUM ONLY (33 individuals)

## Birth data (32 known)

library(readr)
men_birthdata_clean <- read_delim("men_birthdata_clean.csv", 
                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)

men_birthdata_clean %>%
  ggplot( aes(x=birthyear)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("American University Men: Year of birth") +
  theme_ipsum() +
  labs(title = "American University Men of Shanghai",
       subtitle = "Year of birth",
       x = "Year", 
       y = "Number of births", 
       caption = "Based on Men of Shanghai and North China, 1935")

men_birthdata_clean %>%
  ggplot( aes(x=age)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("American University Men: Year of birth") +
  theme_ipsum() +
  labs(title = "American University Men of Shanghai",
       subtitle = "Age in 1935",
       x = "Age", 
       y = "Number of individuals", 
       caption = "Based on Men of Shanghai and North China, 1935")

# length of bios in relation to age 

men_birthdata_clean <- left_join(men_list, men_birthdata_clean)
men_birthdata_clean <- men_birthdata_clean %>% unique()

ggplot(men_birthdata_clean, aes(x=age, y=length)) +
  geom_jitter() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum() + 
  labs(title = "American University Men in Men of Shanghai and North China",
       subtitle = "Length of biography in relation to age",
       x = "Age (years)", 
       y = "Length (words)", 
       caption = "Based on Men of Shanghai and North China, 1935")

# no significant correlation between age and length # p-value: 0.8725

mod1 <- lm(length~age,data=men_birthdata_clean)
summary(mod1)

# between age and nationality? # not significant (p-value: 0.3142)

men_birthdata_clean <- men_birthdata_clean %>% mutate(Nationality = as.factor(Nationality))
mod2 <- glm(Nationality~length, data=men_birthdata_clean, family="binomial")
summary(mod2)
exp(coefficients(mod2))

mod3 <- lm(length ~ Nationality, men_birthdata_clean)
mod3
drop1(mod3, test="F") 

men_birthdata_clean %>%
  ggplot( aes(x=Nationality, y=length, fill=Nationality)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("American University Men in 'Men of Shanghai and North China' (1935)") +
  xlab("Length of biography in relation to nationality")

# Place of birth 

men_birthdata_clean %>% group_by(Nationality, Country) %>% count(sort = TRUE)

men_birthdata_clean %>% filter(Nationality == "Chinese") %>% group_by(Country) %>% count(sort = TRUE)

men_birthdata_clean  %>% filter(Country == "China") %>% group_by(Province) %>% count(sort = TRUE)
men_birthdata_clean  %>% filter(Country == "China") %>% group_by(Town) %>% count(sort = TRUE)

men_birthdata_clean  %>% filter(!Country == "China") %>% group_by(Province) %>% count(sort = TRUE)
men_birthdata_clean  %>% filter(!Country == "China") %>% group_by(Town) %>% count(sort = TRUE)

## EXTRACT AND CLEAN ENTITIES 


# extract named entities 

men_list_ner <- ner_on_corpus(men_list, "imh-en")

# filter organizations

men_list_org <- men_list_ner %>% filter(Type == "ORG") # 757 org 

men_list_org %>% group_by(DocId) %>% count(sort = TRUE)


men_list_org_clean <- men_list_org %>% 
  mutate(Text_clean = str_remove_all(Text, "[\\p{P}\\p{S}&&[^-&'.]]")) %>% # remove non-alpha numeric character 
  mutate(Text_clean = str_replace(Text_clean, "\\.$", "")) %>% # remove final point
  relocate(Text_clean, .after = Text) %>% 
  mutate(Text_clean = str_replace(Text_clean, "^the ", "")) %>% 
  mutate(Text_clean = str_replace(Text_clean, "^The ", "")) %>% 
  mutate(Text_clean = str_squish(Text_clean)) %>% 
  mutate(nchar = nchar(Text_clean)) %>% 
  relocate(nchar, .after = Text_clean) # discard entities > 4 characters


write.csv(men_born_all, "men_birthdata.csv")
write.csv(men_list_org, "men_org.csv")
write.csv(men_list_org_clean, "men_list_org_clean.csv")

# join with names and birth data

men_org <- left_join(men_list_org_clean, men_birthdata_clean)

##############
# USA corpus

men_us_ner <- ner_on_corpus(men_aum_us_all, "imh-en") # 757 ORG 

save.image("menShanghai.RDAta")


### PLACES 

# import cleaned edge list of affiliations and typo

library(readr)
MSNC_simple_edge <- read_delim("MSNC/MSNC_simple_edge.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE)

MSNC_detail <- read_delim("MSNC/MSNC_detail.csv",
delim = ";", escape_double = FALSE, trim_ws = TRUE)


MSNC_typo <- read_delim("MSNC/MSNC_typo.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

# join docid with names and organization with typo 

# first clean names
men_birthdata_clean <- men_birthdata_clean %>% 
  mutate(Title = str_remove_all(Title, ":")) %>% 
  mutate(Title = str_remove_all(Title, "\\(")) %>% 
  mutate(Title = str_squish(Title)) 
# select relevant variables
men_attributes <- men_birthdata_clean %>% rename(Name = Title)
men_id <- men_birthdata_clean %>% select(DocId, Title) %>% rename(Name = Title)
# join with edge list 
MSNC_simple_edge <- left_join(men_id, MSNC_simple_edge)
MSNC_simple_edge <- left_join(MSNC_simple_edge, MSNC_typo)


# apply places on full dataset 

library(Places)

msnc_org <- as.data.frame(MSNC_simple_edge) # total of 580 unique pairs (affiliations) 
class(msnc_org)

Result35 <- places(msnc_org, "Name", "Organization") 
Result35df <- as.data.frame(Result35$PlacesData) ## 33 places
Result35df %>% filter(NbElements>1)


# create the adjacency matrix
bimod<-table(Result35$Edgelist$Places, Result35$Edgelist$Set) 
PlacesMat<-bimod %*% t(bimod)
diag(PlacesMat)<-0 

# create the adjacency matrix
bimod2<-table(Result35$Edgelist$Set, Result35$Edgelist$Places)
PlacesMat2<-bimod2 %*% t(bimod2)
diag(PlacesMat2)<-0

# build network from adjacency matrix with igraph

library(igraph)
Pla1Net<-graph_from_adjacency_matrix(PlacesMat, mode="undirected", weighted = TRUE) # 33 nodes, 369 edges
Pla2Net<-graph_from_adjacency_matrix(PlacesMat2, mode="undirected", weighted = TRUE) # 375 nodes, 5420 edges

# export edge lists 

# convert igraph object into edge list 
edgelist35p_places <- as_edgelist(Pla1Net)
edgelist35p_org <- as_edgelist(Pla2Net)
# export edge lists and node list as csv files
write.csv(edgelist35p_places, "~/Places AUC/MSNC/edgelist35p_places.csv")
write.csv(Result35df, "~/Places AUC/MSNC/nodelist35.csv")
write.csv(edgelist35p_org, "~/Places AUC/MSNC/edgelist35p_org.csv")

# extract place details (names of individuals)

split1 <- Result35df %>% mutate(PlaceDetail,
                               Name=str_extract(PlaceDetail,"[^\\}]+\\}"),
                               Organization=str_extract(PlaceDetail,"\\} - \\{.*")) %>% 
  mutate(Name = str_remove_all(Name,"\\}")) %>% 
  mutate(Name = str_remove_all(Name,"\\{")) %>% 
  mutate(Organization = str_remove_all(Organization,"\\} - \\{")) %>% 
  mutate(Organization = str_remove_all(Organization,"\\}")) 


# split multiple students = one row per students  ";" as separator

split2 <- split1 %>% separate_rows(Name, sep=";")


# split multiple colleges = one row per college ";" as separator

split3 <- split2 %>% separate_rows(Organization, sep=";") 


# join individuals names with place label

place_name <- left_join(split1, men_attributes, by = "Name")
place_attrib <- place_name %>% select(PlaceLabel, PlaceDetail, NbElements, NbSets, Name, Nationality, birthyear, age, Town, Province, Country, length, DocId)
write.csv(place_attrib, "~/Places AUC/MSNC/place_attrib.csv")
write.csv(men_birthdata_clean, "~/Places AUC/MSNC/men_birthdata_clean.csv")


# Visualize 

# visualize

plot(Pla1Net, vertex.size = 8, 
     vertex.color = "orange", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.6, 
     main="Network of places linked by organizations")


plot(Pla2Net, vertex.size = 5, 
     vertex.color = "light blue", 
     vertex.shape = "square", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.25, 
     main="Network of organizations linked by places")

# Note that the two networks are fully connected (no isolated components)


plot(Pla1Net, vertex.size = degree(Pla1Net)*0.5, 
     vertex.color = "orange", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     main="Network of places linked by organizations")

plot(Pla1Net, vertex.size = betweenness(Pla1Net)*0.5, 
     vertex.color = "orange", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     main="Network of places linked by organizations")

# few differences in terms of degree, but strong differences in terms of betweenness
# with two major brokers standing out: Yan Fuqing (prominent medical professional) and Lee Poy Gum (prominent architect)
# next in order are engineering missionary/educator Francis Pott (P026), Walker Mill (P29) and engineer Wen (P23)

plot(Pla2Net, vertex.size = degree(Pla2Net)*0.05, 
     vertex.color = "light blue", 
     vertex.shape = "square", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     main="Network of organizations linked by places")

plot(Pla2Net, vertex.size = betweenness(Pla2Net)*0.005, 
     vertex.color = "light blue", 
     vertex.shape = "square", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.25, 
     main="Network of organizations linked by places")

# extract centrality metrics

# places 

degree1 <- degree((Pla1Net), normalized = TRUE)
eigen1 <- evcent(Pla1Net)$vector
betw1 <- betweenness(Pla1Net)
close1 <- closeness(Pla1Net)

centrality_places <- cbind(degree1, eigen1, betw1, close1) # compile
centrality_places_df <- as.data.frame(centrality_places) # convert into dataframe
centrality_places_df <- tibble::rownames_to_column(centrality_places_df, "PlaceLabel") 
centrality_places_df <- left_join(centrality_places_df, Result35df) # join with place detail

degree2 <- degree((Pla2Net), normalized = TRUE)
eigen2 <- evcent(Pla2Net)$vector
betw2 <- betweenness(Pla2Net)
close2 <- closeness(Pla2Net)

centrality_org <- cbind(degree2, eigen2, betw2, close2) # compile
centrality_org_df <- as.data.frame(centrality_org) # convert into dataframe
centrality_org_df <- tibble::rownames_to_column(centrality_org_df, "Organization") 
centrality_org_df <- left_join(centrality_org_df, MSNC_typo) # join with typo

write.csv(centrality_places_df, "~/Places AUC/MSNC/centrality_places.csv")
write.csv(centrality_org_df, "~/Places AUC/MSNC/centrality_org.csv")

# Strong ties 

# network of places 

table(E(Pla1Net)$weight)

E(Pla1Net)[weight == 8]
t8 <- split3 %>% filter(PlaceNumber %in% c("9", "19"))
t8 %>% group_by(Organization) %>% count(sort = TRUE)

E(Pla1Net)[weight == 7]
split3 %>% filter(PlaceNumber %in% c("9", "17")) %>% group_by(Organization) %>% count() %>% filter(n>1)
split3 %>% filter(PlaceNumber %in% c("17", "19")) %>% group_by(Organization) %>% count() %>% filter(n>1)

E(Pla1Net)[weight == 6]
split3 %>% filter(PlaceNumber %in% c("3", "8")) %>% group_by(Organization) %>% count() %>% filter(n>1)
split3 %>% filter(PlaceNumber %in% c("9", "18")) %>% group_by(Organization) %>% count() %>% filter(n>1)
split3 %>% filter(PlaceNumber %in% c( "10", "27")) %>% group_by(Organization) %>% count() %>% filter(n>1)


E(Pla1Net)[weight == 5]
split3 %>% filter(PlaceNumber %in% c("4", "10")) %>% group_by(Organization) %>% count() %>% filter(n>1)
split3 %>% filter(PlaceNumber %in% c("9", "15")) %>% group_by(Organization) %>% count() %>% filter(n>1)
split3 %>% filter(PlaceNumber %in% c("14", "15")) %>% group_by(Organization) %>% count() %>% filter(n>1)


E(Pla1Net)[weight == 4]
t4a <- split3 %>% filter(PlaceNumber %in% c("1", "12")) %>% group_by(Organization) %>% count() %>% filter(n>1)
t4b <- split3 %>% filter(PlaceNumber %in% c("3", "14")) %>% group_by(Organization) %>% count() %>% filter(n>1)
t4c <- split3 %>% filter(PlaceNumber %in% c("6", "9")) %>% group_by(Organization) %>% count() %>% filter(n>1)
t4d <- split3 %>% filter(PlaceNumber %in% c("6", "17")) %>% group_by(Organization) %>% count() %>% filter(n>1)

# network of organizations

table(E(Pla2Net)$weight)

E(Pla2Net)[weight == 11]
E(Pla2Net)[weight == 8]
E(Pla2Net)[weight == 6]
E(Pla2Net)[weight == 5]
E(Pla2Net)[weight == 4]
E(Pla2Net)[weight == 4][11:17]
E(Pla2Net)[weight == 3]
E(Pla2Net)[weight == 3][11:20]
E(Pla2Net)[weight == 3][21:30]

split3 %>% distinct(Name, Organization) %>% 
  filter(Organization %in% c("American Club", "Columbia Country Club")) %>% 
  group_by(Name) %>% count() %>% filter(n>1)

split3 %>% distinct(Name, Organization) %>% 
  filter(Organization %in% c("American Club", "Shanghai Race Club")) %>% 
  group_by(Name) %>% count() %>% filter(n>1)

split3 %>% distinct(Name, Organization) %>% 
  filter(Organization %in% c("Columbia Country Club", "Shanghai Race Club")) %>% 
  group_by(Name) %>% count() %>% filter(n>1)



split3 %>% distinct(Name, Organization) %>% 
  filter(Organization %in% c("Rotary Club", "St. John's University")) %>% 
  group_by(Name) %>% count() %>% filter(n>1)

split3 %>% distinct(Name, Organization) %>% filter(Organization %in% c("Rotary Club", "Masonic Order")) %>% 
  group_by(Name) %>% count() %>% filter(n>1)
split3 %>% distinct(Name, Organization) %>% filter(Organization %in% c("Shanghai Club", "Columbia Country Club")) %>% 
  group_by(Name) %>% count() %>% filter(n>1)
split3 %>% distinct(Name, Organization) %>% filter(Organization %in% c("Shanghai Club", "American Club")) %>% 
  group_by(Name) %>% count() %>% filter(n>1)
split3 %>% distinct(Name, Organization) %>% filter(Organization %in% c("International Recreation Club", "American Club")) %>% 
  group_by(Name) %>% count() %>% filter(n>1)



split3 %>% distinct(Name, Organization) %>% filter(Organization %in% c("Cercle Sportif Francais", "American Club")) %>% 
  group_by(Name) %>% count() %>% filter(n>1)
split3 %>% distinct(Name, Organization) %>% filter(Organization %in% c("Masonic Order", "American Club")) %>% 
  group_by(Name) %>% count() %>% filter(n>1)
split3 %>% distinct(Name, Organization) %>% filter(Organization %in% c("Shanghai Polo Club", "American Club")) %>% 
  group_by(Name) %>% count() %>% filter(n>1)
split3 %>% distinct(Name, Organization) %>% filter(Organization %in% c("Shanghai Bankers' Club", "American Returned Students' Association")) %>% 
  group_by(Name) %>% count() %>% filter(n>1)
split3 %>% distinct(Name, Organization) %>% filter(Organization %in% c("Cercle Sportif Francais", "Columbia Country Club")) %>% 
  group_by(Name) %>% count() %>% filter(n>1)
split3 %>% distinct(Name, Organization) %>% filter(Organization %in% c("International Recreation Club", "Columbia Country Club")) %>% 
  group_by(Name) %>% count() %>% filter(n>1)
split3 %>% distinct(Name, Organization) %>% filter(Organization %in% c("Masonic Order", "Columbia Country Club")) %>% 
  group_by(Name) %>% count() %>% filter(n>1)
split3 %>% distinct(Name, Organization) %>% filter(Organization %in% c("Shanghai Polo Club", "Columbia Country Club")) %>% 
  group_by(Name) %>% count() %>% filter(n>1)
split3 %>% distinct(Name, Organization) %>% filter(Organization %in% c("Shanghai Club", "Shanghai Race Club"))  %>% 
  group_by(Name) %>% count() %>% filter(n>1)
split3 %>% distinct(Name, Organization) %>% filter(Organization %in% c("Shanghai Bankers' Club", "St. John's University")) %>% 
  group_by(Name) %>% count() %>% filter(n>1)
split3 %>% distinct(Name, Organization) %>% filter(Organization %in% c("University of Pennsylvania", "St. John's University"))  %>% 
  group_by(Name) %>% count() %>% filter(n>1)


split3 %>% distinct(Name, Organization) %>% filter(Organization %in% c("Harvard University", "St. John's University")) %>% 
  group_by(Name) %>% count() %>% filter(n>1)
split3 %>% distinct(Name, Organization) %>% filter(Organization %in% c("National Government (Nanking)", "St. John's University")) %>% 
  group_by(Name) %>% count() %>% filter(n>1)

men_place_id <- place_name %>% select(PlaceLabel, DocId, Name, Nationality, birthyear, age, Town, Province, Country)
centrality_places_df <- left_join(centrality_places_df, men_place_id)
write.csv(centrality_places_df, "~/Places AUC/MSNC/centrality_places_df.csv")

# find communities

## detect communities with Louvain
set.seed(2023)
lvc1 <- cluster_louvain(Pla1Net) # 2 ; mod: 0.23
lvc1b <- cluster_louvain(Pla1Net)
lvc1c <- cluster_louvain(Pla1Net)
lvc2 <- cluster_louvain(Pla2Net) # 8, mod: 0.57
lvc2b <- cluster_louvain(Pla2Net) # 9, mod: 0.56
lvc2c <- cluster_louvain(Pla2Net) # 10, mod: 0.56
lvc2d <- cluster_louvain(Pla2Net) # 8, mod: 0.57


# plot communities 

V(Pla1Net)$group <- lvc1$membership # create a group for each community
V(Pla1Net)$color <- lvc1$membership # node color reflects group membership 

plot(lvc1, Pla1Net, vertex.label=V(Pla1Net)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.8, 
     vertex.size=5,
     vertex.label.family="Helvetica Neue",  
     vertex.label.dist=1,    
     main="Communities of places (Louvain method)")

new_cols <- c("steelblue", "red")[membership(lvc1)]
plot(lvc1, Pla1Net, col=new_cols,
     mark.col=c("lightblue", "pink"),
     mark.border=c("steelblue", "red"),
     vertex.label.color = "black", 
     vertex.label.cex = 0.8, 
     vertex.size=5,
     vertex.label.family="Helvetica Neue",  
     vertex.label.dist=1,    
     main="Communities of places (Louvain method)")


V(Pla1Net)$name <- places_com_detail$Name
lvc1$names <- V(Pla1Net)$name

plot(lvc1, Pla1Net, col=new_cols,
     vertex.label=lvc1$names,
     mark.col=c("lightblue", "pink"),
     mark.border=c("steelblue", "red"),
     vertex.label.color = "black", 
     vertex.label.cex = 0.8, 
     vertex.size=5,
     vertex.label.family="Helvetica Neue",  
     vertex.label.dist=1,    
     main="Communities of places (Louvain method)")


# Organizations 

V(Pla2Net)$group <- lvc2$membership  # create a group for each community
V(Pla2Net)$color <- lvc2$membership # node color reflects group membership 

plot(lvc2, Pla2Net, vertex.label=V(Pla2Net)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     vertex.size=3,
     vertex.label.dist=1, 
     main="Communities of organizations (Louvain method)")


plot(lvc2, Pla2Net,
     vertex.label = NA, 
     vertex.size=3,
     main="Communities of organizations (Louvain method)")


V(Pla2NetMC)$group <- lvc2c$membership  # create a group for each community
V(Pla2NetMC)$color <- lvc2c$membership # node color reflects group membership 

plot(lvc2c, Pla2NetMC, vertex.label=V(Pla2NetMC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     vertex.size=3,
     main="Communities of organizations (Louvain method)")

# join places detail with metrics

place_to_join <- places_com_detail %>% select(-c("PlaceDetail", "NbElements", "NbSets"))
men_centralities <- left_join(place_to_join, centrality_places_df)

write.csv(men_centralities, "~/aum/data/msnc/men_centralities.csv")

# Extract membership data 

# places 

places_com <- data.frame(lvc1$membership,
                         lvc1$names) %>% 
  group_by(lvc1.membership) %>% 
  add_tally() %>% # add size of clusters
  rename(PlaceLabel = lvc1.names, lvcluster = lvc1.membership, size = n)

places_com_detail <- inner_join(places_com, place_attrib, by = "PlaceLabel")  # join place details 

# organizations

org_com <- data.frame(lvc2$membership,
                      lvc2$names)  %>% 
  group_by(lvc2.membership) %>%  
  add_tally() %>% # add size of clusters
  rename(Organization = lvc2.names, lvcluster = lvc2.membership, 
         size = n) 

org_com_detail <- inner_join(org_com, MSNC_typo, by = "Organization")  # join with org typology 

# most linking institutions among communities

place_com_org <- split3 %>% select(PlaceLabel, Organization)
place_com_org <- left_join(places_com, place_com_org)

com1 <- place_com_org %>% filter(lvcluster == "1")
com1_count <-  com1 %>% distinct(PlaceLabel, Organization) %>% group_by(Organization) %>% count(sort = TRUE) %>% mutate(percent = n/50*100)

com2 <- place_com_org %>% filter(lvcluster == "2")
com2_count <- com2 %>% distinct(PlaceLabel, Organization) %>% group_by(Organization) %>% count(sort = TRUE) %>% mutate(percent = n/22*100)

# extract communities 


V(Pla2Net)$group <- lvc2b$membership  # create a group for each community
V(Pla2Net)$color <- lvc2b$membership # node color reflects group membership 

glvc1 <- induced_subgraph(Pla2Net, V(Pla2Net)$group==1) # 35 nodes 
glvc2 <- induced_subgraph(Pla2Net, V(Pla2Net)$group==2) # 25 nodes
glvc3 <- induced_subgraph(Pla2Net, V(Pla2Net)$group==3) # 21 nodes
glvc4 <- induced_subgraph(Pla2Net, V(Pla2Net)$group==4) # 20 nodes 
glvc5 <- induced_subgraph(Pla2Net, V(Pla2Net)$group==5) # 46 nodes
glvc6 <- induced_subgraph(Pla2Net, V(Pla2Net)$group==6) # 21 nodes 
glvc7 <- induced_subgraph(Pla2Net, V(Pla2Net)$group==7)


###########################
# Apply places on education only 




# apply places on career only 

