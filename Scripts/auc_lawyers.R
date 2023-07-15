# Analysis of US-educated legal professionals backgrounds and careers (based on AUM 1936)

save.image("auc_lawyers.RData")
load(file = "auc_lawyers.RData")

# load packages 
library(readr)
library(tidyverse)
library(hrbrthemes)
library(viridis)

install.packages("http://lereps.sciencespo-toulouse.fr/IMG/gz/places_0.2.3.tar.gz", repos = NULL, type = "source")
library(Places)
library(igraph)

# load data

# birth data 
legal_birth_data <- read_delim("data/legal/input/legal_birth_data.csv", 
                               delim = "\t", escape_double = FALSE, 
                               trim_ws = TRUE)

# edu data
legal_edu <- read_delim("data/legal/input/legal_edu.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE) # 173 total curricula

legal_edu_simple <- legal_edu %>% distinct(NameID, Name, Chinese, University, Country) # 27 affiliations

# career
legal_post_simple <- read_delim("data/legal/input/legal_post_simple.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

# all affiliations 

legal_affil_all <- read_delim("data/legal/input/legal_affil_all.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Generation

legal_birth_data %>%
  ggplot( aes(x=Birth_year)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("American-educated lawyers: Year of birth") +
  theme_ipsum() +
  labs(title = "American-educated lawyers",
       subtitle = "Year of birth",
       x = "Year", 
       y = "Number of births", 
       caption = "Based on American University Men of Shanghai, 1936")

legal_birth_data %>%
  ggplot( aes(x=age)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("American-educated lawyers: Age in 1936") +
  theme_ipsum() +
  labs(title = "American-educated lawyers",
       subtitle = "Age in 1936",
       x = "Age", 
       y = "Number of individuals", 
       caption = "Based on American University Men of Shanghai, 1936")

# Native place

legal_birth_data %>% group_by(Province) %>% count(sort = TRUE)
legal_birth_data %>% group_by(Town) %>% count(sort = TRUE)

# Education 

# 173 curricula, 27 unique pairs 

legal_edu_simple %>% group_by(University) %>% count(sort = TRUE)

# Career 

legal_post_count <- legal_post_simple %>% group_by(Organisation) %>% count(sort = TRUE)
legal_post_simple %>% distinct(Name, Organisation) %>% group_by(Organisation) %>% count(sort = TRUE)
legal_post_unique <- legal_post_simple %>% distinct(Name, Organisation)

# Places 

place <- legal_affil_all %>% select(Chinese, Organisation)
place <- as.data.frame(place) # total of 91 unique pairs (affiliations) 
class(place)

Result <- places(place, "Chinese", "Organisation") 
Resultdf <- as.data.frame(Result$PlacesData) ## 16 places

# create the adjacency matrix
bimod<-table(Result$Edgelist$Places, Result$Edgelist$Set) 
PlacesMat<-bimod %*% t(bimod)
diag(PlacesMat)<-0 

# create the adjacency matrix
bimod2<-table(Result$Edgelist$Set, Result$Edgelist$Places)
PlacesMat2<-bimod2 %*% t(bimod2)
diag(PlacesMat2)<-0

# build network from adjacency matrix with igraph

library(igraph)
Pla1Net<-graph_from_adjacency_matrix(PlacesMat, mode="undirected", weighted = TRUE) # 16 nodes, 42 edges
Pla2Net<-graph_from_adjacency_matrix(PlacesMat2, mode="undirected", weighted = TRUE) # 51 nodes, 185 edges

# visualize 

# visualize

# places

plot(Pla1Net, vertex.size = 8, 
     vertex.color = "orange", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.6, 
     main="Network of legal professionals linked by organizations")

plot(Pla1Net, vertex.size = degree(Pla1Net), 
     vertex.color = "orange", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.7, 
     main="Network of legal professionals linked by organizations")

plot(Pla1Net, vertex.size = betweenness(Pla1Net), 
     vertex.color = "orange", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.7, 
     main="Network of organizations linked by legal professionals")


# organizations

plot(Pla2Net, vertex.size = 5, 
     vertex.color = "light blue", 
     vertex.shape = "square", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     main="Network of organizations linked by legal professionals")

plot(Pla2Net, vertex.size = degree(Pla2Net)*0.5, 
     vertex.color = "light blue", 
     vertex.shape = "square", 
     vertex.label.color = "black", 
     vertex.label.cex = degree(Pla2Net)*0.05, 
     main="Network of organizations linked by legal professionals")

plot(Pla2Net, vertex.size = betweenness(Pla2Net)*0.05, 
     vertex.color = "light blue", 
     vertex.shape = "square", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.4, 
     main="Network of organizations linked by legal professionals")

# extract centrality metrics

# places 

degree1 <- degree((Pla1Net), normalized = TRUE)
eigen1 <- evcent(Pla1Net)$vector
betw1 <- betweenness(Pla1Net)
close1 <- closeness(Pla1Net)

centrality_places <- cbind(degree1, eigen1, betw1, close1) # compile
centrality_places_df <- as.data.frame(centrality_places) # convert into dataframe
centrality_places_df <- tibble::rownames_to_column(centrality_places_df, "PlaceLabel") 
centrality_places_df <- left_join(centrality_places_df, Resultdf) # join with place detail

degree2 <- degree((Pla2Net), normalized = TRUE)
eigen2 <- evcent(Pla2Net)$vector
betw2 <- betweenness(Pla2Net)
close2 <- closeness(Pla2Net)

centrality_org <- cbind(degree2, eigen2, betw2, close2) # compile
centrality_org_df <- as.data.frame(centrality_org) # convert into dataframe
centrality_org_df <- tibble::rownames_to_column(centrality_org_df, "Organisation") 


write.csv(centrality_places_df, "centrality_places.csv")
write.csv(centrality_org_df, "centrality_org.csv")

# extract place details (names of individuals)

split1 <- Resultdf %>% mutate(PlaceDetail,
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

# Strong ties 

# network of places 

table(E(Pla1Net)$weight)
table(E(Pla2Net)$weight)

E(Pla1Net)[weight == 3]
E(Pla1Net)[weight == 2]

split3 %>% filter(PlaceNumber %in% c("1", "4")) %>% group_by(Organization) %>% count(sort = TRUE)
split3 %>% filter(PlaceNumber %in% c("2", "3")) %>% group_by(Organization) %>% count(sort = TRUE)
split3 %>% filter(PlaceNumber %in% c("2", "6")) %>% group_by(Organization) %>% count(sort = TRUE)
split3 %>% filter(PlaceNumber %in% c("2", "10")) %>% group_by(Organization) %>% count(sort = TRUE)
split3 %>% filter(PlaceNumber %in% c("3", "10")) %>% group_by(Organization) %>% count(sort = TRUE)
split3 %>% filter(PlaceNumber %in% c("4", "6")) %>% group_by(Organization) %>% count(sort = TRUE)
split3 %>% filter(PlaceNumber %in% c("5", "6")) %>% group_by(Organization) %>% count(sort = TRUE)
split3 %>% filter(PlaceNumber %in% c("7", "9")) %>% group_by(Organization) %>% count(sort = TRUE)

# networks of organizations 

table(E(Pla2Net)$weight)
E(Pla2Net)[weight == 2][11]


# find communities

## detect communities with Louvain
set.seed(2023)
lvc1 <- cluster_louvain(Pla1Net) # 3, mod: 0.33
lvc1b <- cluster_louvain(Pla1Net) # 3, mod: 0.33
lvc1c <- cluster_louvain(Pla1Net) # 3, mod: 0.33
lvc2 <- cluster_louvain(Pla2Net) # 5, mod: 0.5
lvc2b <- cluster_louvain(Pla2Net) # 5, mod: 0.5
lvc2c <- cluster_louvain(Pla2Net) # 5, mod: 0.5

# plot communities 

V(Pla1Net)$group <- lvc1$membership # create a group for each community
V(Pla1Net)$color <- lvc1$membership # node color reflects group membership 

plot(lvc1, Pla1Net, vertex.label=V(Pla1Net)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 1, 
     vertex.size=5,
     main="Communities of legal professionals (Louvain method)")

V(Pla1Net)$name <- node$Chinese
lvc1$names <- V(Pla1Net)$name

plot(lvc1, Pla1Net, vertex.label=V(Pla1Net)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.8, 
     vertex.size=degree(Pla1Net),
     vertex.label.family="Helvetica Neue",  
     vertex.label.dist=1.25,    
     main="Communities of legal professionals (Louvain method)")

plot(lvc1, Pla1Net, vertex.label=V(Pla1Net)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 1, 
     vertex.size=betweenness(Pla1Net)*0.5,
     main="Communities of legal professionals (Louvain method)")


# Organizations 

V(Pla2Net)$group <- lvc2$membership  # create a group for each community
V(Pla2Net)$color <- lvc2$membership # node color reflects group membership 

plot(lvc2, Pla2Net, vertex.label=V(Pla2Net)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     vertex.size=3,
     main="Communities of legal organizations (Louvain method)")

plot(lvc2, Pla2Net, vertex.label=V(Pla2Net)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     vertex.size=degree(Pla2Net)*0.5,
     main="Communities of legal organizations (Louvain method)")

plot(lvc2, Pla2Net, vertex.label=V(Pla2Net)$id,
     vertex.label.color = "black", 
     vertex.label.cex = degree(Pla2Net)*0.025, 
     vertex.size=degree(Pla2Net)*0.5,
     main="Communities of legal organizations (Louvain method)")

# extract membership 

# places 

places_com <- data.frame(lvc1$membership,
                         lvc1$names) %>% 
  group_by(lvc1.membership) %>% 
  add_tally() %>% # add size of clusters
  rename(PlaceLabel = lvc1.names, lvcluster = lvc1.membership, size = n)


org_com <- data.frame(lvc2$membership,
                      lvc2$names)  %>% 
  group_by(lvc2.membership) %>%  
  add_tally() %>% # add size of clusters
  rename(Organisation = lvc2.names, lvcluster = lvc2.membership, 
         size = n) 


### export edge lists 

# export edge lists 

# convert igraph object into edge list 
edge_places <- as_edgelist(Pla1Net)
edge_org <- as_edgelist(Pla2Net)
# export edge lists and node list as csv files
write.csv(edge_places, "edge_places.csv")
write.csv(edge_org, "edge_org.csv")
write.csv(org_com, "org_com.csv")

# create node list for places 

node <- split1 %>% select(PlaceLabel, PlaceDetail, NbSets, Name) %>% rename(Chinese = Name)
node <- left_join(node, places_com)
node <- left_join(node, legal_birth_data)

write.csv(node, "nodelist.csv")

###### communities attribute (age and native place)

# we expect members of community 1 to be older than the average 
# we expect community 3 to be dominated by Jiangsu natives 

com1 <- node %>% filter(lvcluster == "1")
com2 <- node %>% filter(lvcluster == "2")
com3 <- node %>% filter(lvcluster == "3")

# age

mean(node$age,na.rm = TRUE) # 46.6
mean(com1$age,na.rm = TRUE) # 46.1
mean(com2$age,na.rm = TRUE) # 48.6
mean(com3$age,na.rm = TRUE) # 42

com1 %>% group_by(Province) %>% count(sort = TRUE) # Cantonese dominated
com2 %>% group_by(Province) %>% count(sort = TRUE) # Zhejiang (3) (Guangdong 2) 
com3 %>% group_by(Province) %>% count(sort = TRUE) # Zhejiang / Guangdong at par (1/1)
