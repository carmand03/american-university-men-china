#################################################################### 

library(readr)
library(tidyverse)
library(Places)
library(igraph)

save.image("who_networks.RData")

# find places in cleaned list of org

# load data and typology

aum_org <- read_csv("aum_org.csv")
Typo <- read_csv("Typo.csv")

aum_org <- left_join(aum_org, Typo) # join data with typo
aum_org <- aum_org %>% unique() # remove duplicates
aum_org <- left_join(aum_org, name_list) 
aum_org <- aum_org %>% relocate(Name_full, .after = Name)
aum_org <- aum_org %>% unique()
aum_org <- aum_org %>% drop_na(Name)

aum_org <- as.data.frame(aum_org) # total of 764 unique pairs (affiliations) 
class(aum_org)


# distribution of entities per biography

aum_org_count <- aum_org %>% group_by(DocId) %>% count(sort = TRUE)
aum_org_count <- aum_org_count %>% drop_na(DocId)
aum_org_count <- aum_org_count %>% rename(nb_entities = n) 
aum_org_count <- left_join(aum_org_count, who_birthdata)

mean(aum_org_count$nb_entities,na.rm = TRUE)

# plot entity and age 

ggplot(aum_org_count, aes(x=age, y=nb_entities)) +
  geom_jitter() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum() + 
  labs(title = "American University Men in  Who's Who of China",
       subtitle = "Number of affiliations in relation to age",
       x = "Age (years)", 
       y = "Number of organizations mentioned in the biography", 
       caption = "Based on Who's Who of China, 1936")


mod1 <- lm(nb_entities~age,data=aum_org_count) # pvalue >0.5 the correlation is not significant
summary(mod1)
cor.test(aum_org_count$nb_entities,aum_org_count$age)

mod2 <- lm(length~age,data=aum_org_count) # pvalue 0.02 the correlation is hardly significant
summary(mod2)

# Filter by type

# load detailed data (with positions)

library(readr)
aum_org_detail <- read_delim("aum_org_detail.csv",
delim = ";", escape_double = FALSE, trim_ws = TRUE)

# filter relevant data and join with Typo

aum_org_rank <- aum_org_detail %>% select(DocId, Name, Organization, Position) %>% unique() # 760 unique affiliations
aum_org_rank <- left_join(aum_org_rank, Typo, by = "Organization")

# Education only 
aum_edu <- aum_org_rank %>% filter(Type1 == "Education") %>% filter(!Position == "POST") # 160 affiliations
# Work only 
aum_work1 <- aum_org_rank %>% filter(!Type1 == "Education")  %>% filter(!Type1 == "Association") # 368 
aum_work2 <- aum_org_rank %>% filter(Type1 == "Education")  %>% filter(Position == "POST") # retrieve positions in universities (83)
aum_work <- bind_rows(aum_work1, aum_work2) # 451 salaried positions

# Associations 
aum_asso <- aum_org_rank %>% filter(Type1 == "Association") # 147 Affiliations

# search places/k places

# ALL ORGANIZATIONS (NOT FILTERED)

aum_org <- as.data.frame(aum_org)
Result4 <- places(aum_org, "Name", "Organization") 
Result4df <- as.data.frame(Result4$PlacesData)
Result4df %>% filter(NbElements>1) # NONE

# Networks of places/organizations

# create the adjacency matrix
bimod<-table(Result4$Edgelist$Places, Result4$Edgelist$Set) 
PlacesMat<-bimod %*% t(bimod)
diag(PlacesMat)<-0 

# create the adjacency matrix
bimod2<-table(Result4$Edgelist$Set, Result4$Edgelist$Places)
PlacesMat2<-bimod2 %*% t(bimod2)
diag(PlacesMat2)<-0

# build network from adjacency matrix with igraph

library(igraph)
Pla1Net<-graph_from_adjacency_matrix(PlacesMat, mode="undirected", weighted = TRUE)
Pla2Net<-graph_from_adjacency_matrix(PlacesMat2, mode="undirected", weighted = TRUE)

# export edge lists 

# convert igraph object into edge list 
edgelist1 <- as_edgelist(Pla1Net)
edgelist2 <- as_edgelist(Pla2Net)
# export edge lists and node list as csv files
write.csv(edgelist1, "~/Places AUC/auc_whos_who/who_places/edgelist1.csv")
write.csv(Result4df, "~/Places AUC/auc_whos_who/who_places/nodelist1.csv")
write.csv(edgelist2, "~/Places AUC/auc_whos_who/who_places/edgelist2.csv")


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

plot(Pla1Net, vertex.size = degree(Pla1Net)*0.25, 
     vertex.color = "orange", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     main="Network of places linked by organizations")

plot(Pla1Net, vertex.size = betweenness(Pla1Net)*0.25, 
     vertex.color = "orange", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     main="Network of places linked by organizations")


plot(Pla2Net, vertex.size = degree(Pla2Net)*0.5, 
     vertex.color = "light blue", 
     vertex.shape = "square", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.6, 
     main="Network of organizations linked by places")

plot(Pla2Net, vertex.size = betweenness(Pla2Net)*0.005, 
     vertex.color = "light blue", 
     vertex.shape = "square", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.25, 
     main="Network of organizations linked by places")

# centralities 

degree1 <- degree((Pla1Net), normalized = FALSE)
degree1N <- degree((Pla1Net), normalized = TRUE)
eigen1 <- evcent(Pla1Net)$vector
betw1 <- betweenness(Pla1Net)
close1 <- closeness(Pla1Net)

centrality_places <- cbind(degree1, degree1N, eigen1, betw1, close1) # compile
centrality_places_df <- as.data.frame(centrality_places) # convert into dataframe
centrality_places_df <- tibble::rownames_to_column(centrality_places_df, "PlaceLabel") 
centrality_places_df <- left_join(centrality_places_df, Result4df) # join with place detail

degree2 <- degree((Pla2Net), normalized = FALSE)
degree2N <- degree((Pla2Net), normalized = TRUE)
eigen2 <- evcent(Pla2Net)$vector
betw2 <- betweenness(Pla2Net)
close2 <- closeness(Pla2Net)

centrality_org <- cbind(degree2, degree2N, eigen2, betw2, close2) # compile
centrality_org_df <- as.data.frame(centrality_org) # convert into dataframe
centrality_org_df <- tibble::rownames_to_column(centrality_org_df, "Organization") 
centrality_org_df <- left_join(centrality_org_df, Typo) # join with typo

write.csv(centrality_places_df, "~/Places AUC/auc_whos_who/who_places/centrality_places.csv")
write.csv(centrality_org_df, "~/Places AUC/auc_whos_who/who_places/centrality_org.csv")

# Strong ties 

table(E(Pla1Net)$weight)
table(E(Pla2Net)$weight)

E(Pla1Net)[weight > 1]
E(Pla2Net)[weight > 1]


E(Pla2Net)[weight == 6]
E(Pla2Net)[weight == 5]
E(Pla2Net)[weight == 4]
E(Pla2Net)[weight == 4][11:14]
E(Pla2Net)[weight == 3]
E(Pla2Net)[weight == 3][11:20]
E(Pla2Net)[weight == 3][21:30]
E(Pla2Net)[weight == 3][31:40]
E(Pla2Net)[weight == 3][40:41]
E(Pla2Net)[weight == 2]
E(Pla2Net)[weight == 2][11:20]
E(Pla2Net)[weight == 2][21:30]
E(Pla2Net)[weight == 2][31:40]
E(Pla2Net)[weight == 2][41:50]
E(Pla2Net)[weight == 2][51:60]
E(Pla2Net)[weight == 2][61:70]
E(Pla2Net)[weight == 2][71:80]
E(Pla2Net)[weight == 2][81:90]
E(Pla2Net)[weight == 2][91:100]
E(Pla2Net)[weight == 2][101:110]
E(Pla2Net)[weight == 2][111:120]
E(Pla2Net)[weight == 2][121:130]
E(Pla2Net)[weight == 2][131:140]
E(Pla2Net)[weight == 2][141:150]
E(Pla2Net)[weight == 2][151:160]
E(Pla2Net)[weight == 2][161:170]
E(Pla2Net)[weight == 2][171:180]
E(Pla2Net)[weight == 2][181:190]
E(Pla2Net)[weight == 2][161:170]
E(Pla2Net)[weight == 2][246:255]
E(Pla2Net)[weight == 2][256:265]

### 

# Find cliques in networks of organizations 

clique3 <- cliques(Pla2Net, min=3, max = 3)
largest_cliques(g)

################# COMMUNITIES OF PLACES AND ORGANIZATIONS 


# extract main components
Pla1NetMC <- induced.subgraph(Pla1Net,vids=clusters(Pla1Net)$membership==1)
Pla2NetMC <- induced.subgraph(Pla2Net,vids=clusters(Pla2Net)$membership==1)

## detect communities with Louvain
set.seed(2023)
lvc1 <- cluster_louvain(Pla1NetMC)
lvc1b <- cluster_louvain(Pla1NetMC)
lvc1c <- cluster_louvain(Pla1NetMC)
lvc2 <- cluster_louvain(Pla2NetMC) # 9  mod: 0.52
lvc2c <- cluster_louvain(Pla2NetMC) # 10 mod: 0.53

print(lvc1b) 
print(lvc1c) 

# plot communities 

V(Pla1Net)$group <- lvc1$membership # create a group for each community
V(Pla1Net)$color <- lvc1$membership # node color reflects group membership 

vertex.label = V(Pla1Net)$name
lvc1$names <- V(Pla1Net)$name

plot(lvc1, Pla1NetMC, vertex.label=V(Pla1NetMC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     vertex.size=1.8,
     main="Communities of places (Louvain method)")

V(Pla1NetMC)$group <- lvc1$membership # create a group for each community
V(Pla1NetMC)$color <- lvc1$membership # node color reflects group membership 


V(Pla1NetMC)$name <- places_com_detail$Name
lvc1$names <- V(Pla1NetMC)$name

plot(lvc1, Pla1NetMC, vertex.label=V(Pla1NetMC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.8, 
     vertex.size=5,
     main="Communities of places (Louvain method)")


plot(lvc1, Pla1NetMC, 
     vertex.label=lvc1$names,
     vertex.label.color = "black", 
     vertex.label.cex = 0.8, 
     vertex.size=5,
     vertex.label.family="Helvetica Neue",  
     vertex.label.dist=1,    
     main="Communities of places (Louvain method)")


# Organizations 

V(Pla2NetMC)$group <- lvc2$membership  # create a group for each community
V(Pla2NetMC)$color <- lvc2$membership # node color reflects group membership 

plot(lvc2, Pla2NetMC,
     vertex.label = NA, 
     vertex.size=2,
     main="Communities of organizations (Louvain method)")

V(Pla2NetMC)$group <- lvc2c$membership  # create a group for each community
V(Pla2NetMC)$color <- lvc2c$membership # node color reflects group membership 

plot(lvc2c, Pla2NetMC, vertex.label=V(Pla2NetMC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     vertex.size=3,
     main="Communities of organizations (Louvain method)")

# Extract membership data 

places_com <- data.frame(lvc1$membership,
                          lvc1$names) %>% 
  group_by(lvc1.membership) %>% 
  add_tally() %>% # add size of clusters
  rename(PlaceLabel = lvc1.names, lvcluster = lvc1.membership, size = n)

places_com_detail <- inner_join(places_com, Result4df, by = "PlaceLabel")  # join place details 

# join with docId and birth data
label_id <- place_com_typo %>% select(PlaceLabel, Name) %>% unique()
label_id <- left_join(label_id, id_names)
places_com_detail <- left_join(places_com_detail, label_id, by = "PlaceLabel") 

# join with birth data 

# first extract names


split1 <- places_com_detail %>% mutate(PlaceDetail,
                               Name=str_extract(PlaceDetail,"[^\\}]+\\}")) %>% 
  mutate(Name = str_remove_all(Name,"\\}")) %>% 
  mutate(Name = str_remove_all(Name,"\\{")) 

places_com_detail <- left_join(split1, aum_org_count)
places_com_detail <- left_join(places_com_detail, centrality_join)

centrality_join <- centrality_places_df %>% select(-c(PlaceNumber, PlaceDetail, NbSets, NbElements))

# load birth data

who_birthdata <- read_delim("who_birthdata.csv", 
                            delim = ";", escape_double = FALSE, col_types = cols(Name = col_skip()), 
                            trim_ws = TRUE)
# join 
places_com_detail <- left_join(places_com_detail, who_birthdata, by = "DocId") # Join with DocId

org_com <- data.frame(lvc2$membership,
                        lvc2$names)  %>% 
  group_by(lvc2.membership) %>%  
  add_tally() %>% # add size of clusters
  rename(Organization = lvc2.names, lvcluster = lvc2.membership, 
         size = n) 

org_com_detail <- inner_join(org_com, Typo, by = "Organization")  # join with org typology 


org_com2c <- data.frame(lvc2c$membership,
                        lvc2c$names)  %>% 
  group_by(lvc2c.membership) %>%  
  add_tally() %>% # add size of clusters
  rename(Organization = lvc2c.names, lvcluster = lvc2c.membership, 
         size = n) 
org2c_com_detail <- inner_join(org_com2c, Typo, by = "Organization")  # join with org typology 

## extract place details

# split individuals (elements) and universities (sets) - "-" as separator and remove special characters (-, {})

split1 <- Result4df %>% mutate(PlaceDetail,
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


# join with organizations typo and size

place_typo <- left_join(split3, Typo, by = "Organization")

places_com_to_join <- places_com_detail %>% select(lvcluster, PlaceLabel, size)
place_com_typo <- left_join(places_com_to_join, place_typo)


# export communities 

write.csv(places_com_detail, "~/Places AUC/auc_whos_who/who_places/places_com_detail.csv")
write.csv(place_com_typo, "~/Places AUC/auc_whos_who/who_places/places_com.csv")
write.csv(org_com_detail, "~/Places AUC/auc_whos_who/who_places/org11_com.csv")
write.csv(org2c_com_detail, "~/Places AUC/auc_whos_who/who_places/org10_com.csv")


### Explore communities

# most linking institutions among communities

com1 <- place_com_typo %>% filter(lvcluster == "1")
com1_count <-  com1 %>% distinct(PlaceLabel, Name, Organization) %>% group_by(Organization) %>% count(sort = TRUE) %>% mutate(percent = n/10*100)

com2 <- place_com_typo %>% filter(lvcluster == "2")
com2_count <- com2 %>% distinct(PlaceLabel, Name, Organization) %>% group_by(Organization) %>% count(sort = TRUE) %>% mutate(percent = n/11*100)

com3 <- place_com_typo %>% filter(lvcluster == "3")
com3_count <- com3 %>% distinct(PlaceLabel, Name, Organization) %>% group_by(Organization) %>% count(sort = TRUE) %>% mutate(percent = n/15*100)

com4 <- place_com_typo %>% filter(lvcluster == "4")
com4_count <- com4 %>% distinct(PlaceLabel, Name, Organization) %>% group_by(Organization) %>% count() %>% mutate(percent = n/11*100)

# Birth data 

# general

com1 <- places_com_detail %>% filter(lvcluster == "1") 
com2 <- places_com_detail %>% filter(lvcluster == "2") 
com3 <- places_com_detail %>% filter(lvcluster == "3") 
com4 <- places_com_detail %>% filter(lvcluster == "4") 

mean(com1$age,na.rm = TRUE) # 46.1 yo
mean(com2$age,na.rm = TRUE) # 44.3 yo
mean(com3$age,na.rm = TRUE) # 45 yo
mean(com4$age,na.rm = TRUE) # 40.67 yo 


min(com1$age,na.rm = TRUE) # 36
min(com2$age,na.rm = TRUE) # 36
min(com3$age,na.rm = TRUE) # 36
min(com4$age,na.rm = TRUE) # 35

max(com1$age,na.rm = TRUE) # 
max(com2$age,na.rm = TRUE) # 
max(com3$age,na.rm = TRUE) # 
max(com4$age,na.rm = TRUE) # 


com1 %>% group_by(Province) %>% count(sort = TRUE) # equal distribution betwen Jiangnan provinces and USA
com2 %>% group_by(Province) %>% count(sort = TRUE) # Jiangsu predominated (especially Shanghai, 4), before Guangdong
com3 %>% group_by(Province) %>% count(sort = TRUE)  # Guangdong predominated, before Jiangsu
com4 %>% group_by(Province) %>% count(sort = TRUE) # Zhejiang predominated, before Jiangsu

com2 %>% group_by(birthplace) %>% count(sort = TRUE) 

# by cluster

places_com_detail %>% filter(lvcluster == "1") %>% group_by(birthplace) %>% count(sort = TRUE)
places_com_detail %>% filter(lvcluster == "1") %>% group_by(birthyear) %>% count()


places_com_detail %>% filter(lvcluster == "2") %>% group_by(birthplace) %>% count(sort = TRUE)
places_com_detail %>% filter(lvcluster == "2") %>% group_by(Province) %>% count(sort = TRUE)
places_com_detail %>% filter(lvcluster == "2") %>% group_by(birthyear) %>% count()



## Compare communities metrics and structure 

# 3 types: college-centered (1), employer-centered (3), small-world/specialized communities (2, 4)

glvc1 <- induced_subgraph(Pla1NetMC, V(Pla1NetMC)$group==1) # 10 nodes 
glvc2 <- induced_subgraph(Pla1NetMC, V(Pla1NetMC)$group==2) # 11 nodes
glvc3 <- induced_subgraph(Pla1NetMC, V(Pla1NetMC)$group==3) # 15 nodes
glvc4 <- induced_subgraph(Pla1NetMC, V(Pla1NetMC)$group==4) # 11 nodes 

# plot 

plot(glvc1, vertex.label=V(Pla1NetMC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 1, 
     vertex.size= 10,
     vertex.label.family="Helvetica Neue",  
     vertex.label.dist=1.5, 
     main="P1-Columbia-YMCA")



plot(glvc2, vertex.label=V(Pla1NetMC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 1, 
     vertex.size= 10,
     vertex.label.family="Helvetica Neue",  
     vertex.label.dist=1.5, 
     main="P2-St John's Professionals")


plot(glvc3, vertex.label=V(Pla1NetMC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 1, 
     vertex.size= 10,
     vertex.label.family="Helvetica Neue",  
     vertex.label.dist=1.5, 
     main="P3-Economic Bureaucracy")



plot(glvc4, vertex.label=V(Pla1NetMC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 1, 
     vertex.size= 10,
     vertex.label.family="Helvetica Neue",  
     vertex.label.dist=1.5, 
     main="P4-Foreign Affairs")


plot(glvc1, vertex.label=V(Pla1NetMC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.8, 
     vertex.size= betweenness(Pla1NetMC)*0.5,
     vertex.label.family="Helvetica Neue",  
     vertex.label.dist=1.8, 
     main="P1-Columbia-YMCA")

plot(glvc1, vertex.label=V(Pla1NetMC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     vertex.size= 5, 
     vertex.label.family="Helvetica Neue",  
     vertex.label.dist=1.2,    
     main = "Cornell-Beiyang-Construction")

plot(glvc2, vertex.label=V(Pla1NetMC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.7, 
     vertex.size= 8,
     main="AUM Community 2 (Professionals)")

plot(glvc3, vertex.label=V(Pla1NetMC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.7, 
     vertex.size= 8,
     main="AUM Community 3 (Government officials & experts)")

plot(glvc4, vertex.label=V(Pla1NetMC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.7, 
     vertex.size= 8,
     main="AUM Community 4 (Yale-Tsinghua)")

# extract metrics 

order1 <- gorder(glvc1)
order2 <- gorder(glvc2)
order3 <- gorder(glvc3)
order4 <- gorder(glvc4)
order <- c(order1, order2, order3, order4)

size1 <- gsize(glvc1)
size2 <- gsize(glvc2)
size3 <- gsize(glvc3)
size4 <- gsize(glvc4)
size <- c(size1, size2, size3, size4)

d1 <- diameter(glvc1) 
d2 <- diameter(glvc2)  
d3 <- diameter(glvc3) 
d4 <- diameter(glvc4)
diameter <- c(d1, d2, d3, d4)

deg1 <- mean(degree(glvc1))  
deg2 <- mean(degree(glvc2))  
deg3 <- mean(degree(glvc3))   
deg4 <- mean(degree(glvc4)) 

avgdegree <- c(deg1, deg2, deg3, deg4)

avdist1 <- average.path.length(glvc1)  
avdist2 <- average.path.length(glvc2) 
avdist3 <- average.path.length(glvc3) 
avdist4 <- average.path.length(glvc4)
avgpath <- c(avdist1, avdist2, avdist3, avdist4)

# compactness

Compactness <- function(g) {
  gra.geo <- distances(g) ## get geodesics
  gra.rdist <- 1/gra.geo  ## get reciprocal of geodesics
  diag(gra.rdist) <- NA   ## assign NA to diagonal
  gra.rdist[gra.rdist == Inf] <- 0 ## replace infinity with 0
  # Compactness = mean of reciprocal distances
  comp.igph <- mean(gra.rdist, na.rm=TRUE) 
  return(comp.igph)
}

# Apply function to each communities of universities
comp1 <- Compactness(glvc1) 
comp2 <- Compactness(glvc2) 
comp3 <- Compactness(glvc3)
comp4 <- Compactness(glvc4)


compactness <- c(comp1, comp2, comp3, comp4)
compactness

ed1 <- edge_density(glvc1) 
ed2 <- edge_density(glvc2)  
ed3 <- edge_density(glvc3)  
ed4 <- edge_density(glvc4)   
density <- c(ed1, ed2, ed3, ed4)

trans1 <- transitivity(glvc1) 
trans2 <- transitivity(glvc2)
trans3 <- transitivity(glvc3) 
trans4 <- transitivity(glvc4) 

transitivity <- c(trans1, trans2, trans3, trans4)

eigen1 <- centr_eigen(glvc1)$centralization
eigen2 <- centr_eigen(glvc2)$centralization
eigen3 <- centr_eigen(glvc3)$centralization
eigen4 <- centr_eigen(glvc4)$centralization

dominance <- c(eigen1, eigen2, eigen3, eigen4)

places_com_metrics <- cbind(order, size, diameter, avgpath, density, avgdegree, compactness, transitivity, dominance) 

places_com_metrics_df <- as.data.frame(places_com_metrics)
places_com_metrics_df <- tibble::rownames_to_column(places_com_metrics_df, var = "Community")
write_csv(places_com_metrics_df, "~/Places AUC/auc_whos_who/who_places/places_com_metrics.csv")

library(FactoMineR)
library(Factoshiny)
library(factoextra)

Factoshiny(places_com_metrics)


## Communities of institutions


glvc1 <- induced_subgraph(Pla2NetMC, V(Pla2NetMC)$group==1) #  
glvc2 <- induced_subgraph(Pla2NetMC, V(Pla2NetMC)$group==2) #  nodes
glvc3 <- induced_subgraph(Pla2NetMC, V(Pla2NetMC)$group==3) #  nodes
glvc4 <- induced_subgraph(Pla2NetMC, V(Pla2NetMC)$group==4) #  nodes 
glvc5 <- induced_subgraph(Pla2NetMC, V(Pla2NetMC)$group==5) #  nodes 
glvc6 <- induced_subgraph(Pla2NetMC, V(Pla2NetMC)$group==6) #  nodes 
glvc7 <- induced_subgraph(Pla2NetMC, V(Pla2NetMC)$group==7) #  nodes 
glvc8 <- induced_subgraph(Pla2NetMC, V(Pla2NetMC)$group==8) #  nodes 
glvc9 <- induced_subgraph(Pla2NetMC, V(Pla2NetMC)$group==9) #  nodes 


plot(glvc4, vertex.label=V(Pla2NetMC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     vertex.size= 5, 
     vertex.label.family="Helvetica Neue",  
     vertex.label.dist=1.2,    
     main = "Cornell-Beiyang-Construction")

plot(glvc7, vertex.label=V(Pla2NetMC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     vertex.size= 5, 
     vertex.label.family="Helvetica Neue",  
     vertex.label.dist=1.2,    
     main = "O7-Columbia-YMCA")

plot(glvc3, vertex.label=V(Pla2NetMC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     vertex.size= 5, 
     vertex.label.family="Helvetica Neue",  
     vertex.label.dist=1.2,    
     main = "O3-Pennsylvania-Chicago")

plot(glvc5, vertex.label=V(Pla2NetMC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     vertex.size= 5, 
     vertex.label.family="Helvetica Neue",  
     vertex.label.dist=1.2,    
     main = "O5-St John-Harvard-Red Cross")


plot(glvc8, vertex.label=V(Pla2NetMC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     vertex.size= 5, 
     vertex.label.family="Helvetica Neue",  
     vertex.label.dist=1.2,    
     main = "O8-Michigan-Law")


plot(glvc9, vertex.label=V(Pla2NetMC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     vertex.size= 5, 
     vertex.label.family="Helvetica Neue",  
     vertex.label.dist=1.2,    
     main = "O9-Christian-Commissions")

plot(glvc1, vertex.label=V(Pla2NetMC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     vertex.size= 7, 
     vertex.label.family="Helvetica Neue",  
     vertex.label.dist=1.2,    
     main = "O1-Freemason Professionals")

plot(glvc2, vertex.label=V(Pla2NetMC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     vertex.size= 7, 
     vertex.label.family="Helvetica Neue",  
     vertex.label.dist=1.2,    
     main = "O2-Qinghua-Beida-Foreign Affairs")

plot(glvc4, vertex.label=V(Pla2NetMC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     vertex.size= 7, 
     vertex.label.family="Helvetica Neue",  
     vertex.label.dist=1.2,    
     main = "O4-Cornell-Beiyang-Construction")


plot(glvc6, vertex.label=V(Pla2NetMC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     vertex.size= 7, 
     vertex.label.family="Helvetica Neue",  
     vertex.label.dist=1.2,    
     main = "O6-Foreign Press")

plot(glvc2, vertex.label=V(Pla2NetMC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     vertex.size= 5)

plot(glvc2, vertex.label=V(Pla2NetMC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     vertex.size= 5)

order1 <- gorder(glvcp1)
order2 <- gorder(glvcp2)
order3 <- gorder(glvcp3)
order4 <- gorder(glvcp4)
order5 <- gorder(glvcp5)
order6 <- gorder(glvcp6)
order7 <- gorder(glvcp7)
order8 <- gorder(glvcp8)
order9 <- gorder(glvcp9)

order <- c(order1, order2, order3, order4, order5, order6, order7, order8, order9)
order

size1 <- gsize(glvcp1)
size2 <- gsize(glvcp2)
size3 <- gsize(glvcp3)
size4 <- gsize(glvcp4)
size5 <- gsize(glvcp5)
size6 <- gsize(glvcp6)
size7 <- gsize(glvcp7)
size8 <- gsize(glvcp8)
size9 <- gsize(glvcp9)

size <- c(size1, size2, size3, size4, size5, size6, size7, size8, size9)
size

d1 <- diameter(glvcp1) 
d2 <- diameter(glvcp2)  
d3 <- diameter(glvcp3) 
d4 <- diameter(glvcp4)
d5 <- diameter(glvcp5) 
d6 <- diameter(glvcp6) 
d7 <- diameter(glvcp7) 
d8 <- diameter(glvcp8) 
d9 <- diameter(glvcp9) 

diameter <- c(d1, d2, d3, d4, d5, d6, d7, d8, d9)
diameter

avdist1 <- average.path.length(glvcp1)  
avdist2 <- average.path.length(glvcp2) 
avdist3 <- average.path.length(glvcp3) 
avdist4 <- average.path.length(glvcp4)
avdist5 <- average.path.length(glvcp5) 
avdist6 <- average.path.length(glvcp6) 
avdist7 <- average.path.length(glvcp7) 
avdist8 <- average.path.length(glvcp8) 
avdist9 <- average.path.length(glvcp9)

avgpath <- c(avdist1, avdist2, avdist3, avdist4, avdist5, avdist6, avdist7, avdist8, avdist9)

ed1 <- edge_density(glvcp1) 
ed2 <- edge_density(glvcp2)  
ed3 <- edge_density(glvcp3)  
ed4 <- edge_density(glvcp4)  
ed5 <- edge_density(glvcp5)
ed6 <- edge_density(glvcp6) 
ed7 <- edge_density(glvcp7) 
ed8 <- edge_density(glvcp8) 
ed9 <- edge_density(glvcp9) 

density <- c(ed1, ed2, ed3, ed4, ed5, ed6, ed7, ed8, ed9)
density

deg1 <- mean(degree(glvcp1))  
deg2 <- mean(degree(glvcp2))  
deg3 <- mean(degree(glvcp3))   
deg4 <- mean(degree(glvcp4)) 
deg5 <- mean(degree(glvcp5)) 
deg6 <- mean(degree(glvcp6)) 
deg7 <- mean(degree(glvcp7))
deg8 <- mean(degree(glvcp8))
deg9 <- mean(degree(glvcp9))

avgdegree <- c(deg1, deg2, deg3, deg4, deg5, deg6, deg7, deg8, deg9)
avgdegree

# compactness

Compactness <- function(g) {
  gra.geo <- distances(g) ## get geodesics
  gra.rdist <- 1/gra.geo  ## get reciprocal of geodesics
  diag(gra.rdist) <- NA   ## assign NA to diagonal
  gra.rdist[gra.rdist == Inf] <- 0 ## replace infinity with 0
  # Compactness = mean of reciprocal distances
  comp.igph <- mean(gra.rdist, na.rm=TRUE) 
  return(comp.igph)
}

# Apply function to each communities of universities
comp1 <- Compactness(glvcp1) 
comp2 <- Compactness(glvcp2) 
comp3 <- Compactness(glvcp3)
comp4 <- Compactness(glvcp4)
comp5 <- Compactness(glvcp5)
comp6 <- Compactness(glvcp6)
comp7 <- Compactness(glvcp7) 
comp8 <- Compactness(glvcp8)
comp9 <- Compactness(glvcp9)

compactness <- c(comp1, comp2, comp3, comp4, comp5, comp6, comp7, comp8, comp9)
compactness


# transitivity (number of triangles)

trans1 <- transitivity(glvcp1) 
trans2 <- transitivity(glvcp2)
trans3 <- transitivity(glvcp3) 
trans4 <- transitivity(glvcp4) 
trans5 <- transitivity(glvcp5) 
trans6 <- transitivity(glvcp6)
trans7 <- transitivity(glvcp7) 
trans8 <- transitivity(glvcp8) 
trans9 <- transitivity(glvcp9) 

transitivity <- c(trans1, trans2, trans3, trans4, trans5, trans6, trans7, trans8)
transitivity 

eigen1 <- centr_eigen(glvcp1)$centralization
eigen2 <- centr_eigen(glvcp2)$centralization
eigen3 <- centr_eigen(glvcp3)$centralization
eigen4 <- centr_eigen(glvcp4)$centralization
eigen5 <- centr_eigen(glvcp5)$centralization
eigen6 <- centr_eigen(glvcp6)$centralization
eigen7 <- centr_eigen(glvcp7)$centralization
eigen8 <- centr_eigen(glvcp8)$centralization
eigen9 <- centr_eigen(glvcp9)$centralization

dominance <- c(eigen1, eigen2, eigen3, eigen4, eigen5, eigen6, eigen7, eigen8, eigen9)
dominance

louvain_org <- cbind(order, size, diameter, avgpath, density, avgdegree, compactness, transitivity, dominance) 

louvain_org_df <- as.data.frame(louvain_org)
louvain_org_metrics <- tibble::rownames_to_column(louvain_org_df, var = "Communities")



