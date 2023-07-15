# Analysis of US-educated engineers backgrounds and careers (based on AUM 1936)
# Data manually created from S.M. Lee, In Engineering and Architecture, AUM 1936

save.image("auc_engineers.RData")
load(file = "auc_engineers.RData")

# load packages 
library(readr)
library(tidyverse)
library(hrbrthemes)
library(viridis)

install.packages("http://lereps.sciencespo-toulouse.fr/IMG/gz/places_0.2.3.tar.gz", repos = NULL, type = "source")
library(Places)
library(igraph)

# load data 


## Education

engineers_education <- read_delim("data/engineers/input/engineers_education.csv", 
                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)

## Positions

engineers_positions <- read_delim("data/engineers/input/engineers_positions.csv", 
                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)

## For statistics and flow charts, see Places AUC

# statistical description by field

field_count <- engineers_education %>% distinct(NameID, Subfield) %>% group_by(Subfield) %>% drop_na(Subfield) %>% count(sort = TRUE) %>% mutate(percent = n/157*100)

engineers_education %>% group_by(University) %>% count(sort = TRUE)
engineers_positions %>% group_by(Organisation) %>% count(sort = TRUE)

########################## PLACES


# education 

engineers_education <- engineers_education %>% mutate(University = str_replace(University, "Jiatong", "Jiaotong"))

place_edu <- engineers_education %>% select(FullName, University) %>% drop_na(University) # 149 curricula
place_edu <- as.data.frame(place_edu) 
class(place_edu)

Result1 <- places(place_edu, "FullName", "University")
Result1df <- as.data.frame(Result1$PlacesData) 

# 136 elements (engineers) and 26 sets (universities)
# 34 places found

topplaces1 <- Result1df %>% filter(NbElements >1)


# positions

place_pos <- engineers_positions %>% select(FullName, Organisation) %>% drop_na(Organisation) # 241 positions
place_pos <- as.data.frame(place_pos) 
class(place_pos)

Result2 <- places(place_pos, "FullName", "Organisation")
Result2df <- as.data.frame(Result2$PlacesData) 

# 151 elements (engineers) and 113 sets (employers)
# 94 places found

topplaces2 <- Result2df %>% filter(NbElements >1 & NbSets>1)
topplaces2 <- Result2df %>% filter(NbElements >1)

# join education and positions 

edu_to_join <- engineers_education %>% select(Subfield, FullName, Chinese, University) %>% drop_na(University) %>% unique()
edu_positions <- inner_join(edu_to_join, place_pos)
edu_positions <-edu_positions %>% unique()

# both 

place_edu_to_bind <- place_edu %>% rename(Element = FullName, Set = University) %>% mutate(Type = "Education")
place_pos_to_bind <- place_pos %>% rename(Element = FullName, Set = Organisation) %>% mutate(Type = "Career")
place_edu_pos <- bind_rows(place_edu_to_bind, place_pos_to_bind)
place_edu_pos <- as.data.frame(place_edu_pos) 
class(place_edu_pos)

Result3 <- places(place_edu_pos, "Element", "Set")
Result3df <- as.data.frame(Result3$PlacesData) 

# 155 elements (engineers) and 139 sets (affiliations)
# 126 places found

topplaces3 <- Result3df %>% filter(NbElements >1 & NbSets>1)

# kplaces 

ResultK3 <- kplaces(place_edu_pos, "Element", "Set", 1)

# 108 places and 8 k-places

ResultK3df <- as.data.frame(ResultK3$KPlacesData)
ResultK3df <- ResultK3df %>% mutate(nchar= nchar(PlaceLabel))

topk3places <- ResultK3df %>% filter(nchar == 14)

# build network of places and institutions


# Build adjacency matrix

bimod<-table(Result3$Edgelist$Places, Result3$Edgelist$Set) 
PlacesMat<-bimod %*% t(bimod)
diag(PlacesMat)<-0 

bimod2<-table(Result3$Edgelist$Set, Result3$Edgelist$Places)
PlacesMat2<-bimod2 %*% t(bimod2)
diag(PlacesMat2)<-0

# Create a network from the adjacency matrix:

library(igraph)
PlaNet1<-graph_from_adjacency_matrix(PlacesMat, mode="undirected", weighted = TRUE) # 126 nodes (places) and 1372 ties
PlaNet2<-graph_from_adjacency_matrix(PlacesMat2, mode="undirected", weighted = TRUE) # 139 nodes (institutions) and 266 ties


# create edge lists 

edgelist_engineer_1 <- as_edgelist(PlaNet1)
edgelist_engineer_2 <- as_edgelist(PlaNet2)

# create node lists for institutions (different categories and sectors)


edu_node <- engineers_education %>% select(University) %>% 
  rename(Element = University) %>%  
  mutate(Type = "education", Sector = "higher") %>% 
  unique()

pos_node <- engineers_positions %>% select(Organisation, Type) %>% 
  rename(Element = Organisation, Sector = Type) %>% 
  mutate(Type = "position") %>% 
  relocate(Type, .before = Sector) %>%
  unique()

element_node <- bind_rows(edu_node, pos_node)

# check duplicate 

element_node$Element[duplicated(element_node$Element)] # no duplicates 


# Visualize 

# network of places 

plot(PlaNet1, vertex.size = 5, 
     vertex.color = "orange", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     main="Network of American-educated engineers linked by organizations")


plot(PlaNet1, vertex.size = degree(PlaNet1)*0.08, 
     vertex.color = "orange", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     main="Network of American-educated engineers linked by organizations")

plot(PlaNet1, vertex.size = betweenness(PlaNet1)*0.05, 
     vertex.color = "orange", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.7, 
     main="Network of American-educated engineers linked by organizations")


# Network of organizations 

plot(PlaNet2, vertex.size = 5, 
     vertex.color = "steel blue", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.35, 
     main="American-educated engineers: Institutions linked by places")


plot(PlaNet2, vertex.size = betweenness(PlaNet2)*0.005, 
     vertex.color = "steel blue", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     main="American-educated engineers: Institutions linked by places")

plot(PlaNet2, vertex.size = betweenness(PlaNet2)*0.005, 
     vertex.color = "steel blue", 
     vertex.label.color = "black", 
     vertex.label.cex =  betweenness(PlaNet2)*5, 
     main="American-educated engineers: Institutions linked by places")


# Analyse


summary(PlaNet1) # 126  places, 1372  ties 
graph.density(PlaNet1) # density: 0.1742222
no.clusters(PlaNet1) # number of components: 10
clusters(PlaNet1)$csize # size of components (one big connected component with 115 nodes, 1 triads, and 8 isolated places
induced.subgraph(PlaNet1,vids=clusters(PlaNet1)$membership==4)

table(E(PlaNet1)$weight) # edge weight : 42 pairs with 2 ties in common


# extract main component (MC = main component)
PlaNet1MC <- induced.subgraph(PlaNet1,vids=clusters(PlaNet1)$membership==1)
summary(PlaNet1MC) # 115 nodes (places), 1369  edges (institutions)


summary(PlaNet2) # 139  organizations, 266  ties 
graph.density(PlaNet2) # density: 0.02773433
no.clusters(PlaNet2) # number of components: 10
clusters(PlaNet2)$csize # size of components (one big connected component with 124 nodes, 2 triads, 2 dyads, and 5 isolated places
induced.subgraph(PlaNet2,vids=clusters(PlaNet2)$membership==4)


# extract main component (MC = main component)
PlaNet2MC <- induced.subgraph(PlaNet2,vids=clusters(PlaNet2)$membership==1)
summary(PlaNet2MC) # 124 nodes (institutions), 259  edges

# extract metrics (main components)

# places 

degree1 <- degree((PlaNet1MC), normalized = TRUE)
eigen1 <- evcent(PlaNet1MC)$vector
betw1 <- betweenness(PlaNet1MC)
close1 <- closeness(PlaNet1MC)

centrality_places <- cbind(degree1, eigen1, betw1, close1) # compile
centrality_places_df <- as.data.frame(centrality_places) # convert into dataframe
centrality_places_df <- tibble::rownames_to_column(centrality_places_df, "PlaceLabel") 
centrality_places_df <- left_join(centrality_places_df, Result3df) # join with place detail

degree2 <- degree((PlaNet2MC), normalized = TRUE)
eigen2 <- evcent(PlaNet2MC)$vector
betw2 <- betweenness(PlaNet2MC)
close2 <- closeness(PlaNet2MC)

centrality_org <- cbind(degree2, eigen2, betw2, close2) # compile
centrality_org_df <- as.data.frame(centrality_org) # convert into dataframe
centrality_org_df <- tibble::rownames_to_column(centrality_org_df, "Organization") 
centrality_org_df <- left_join(centrality_org_df, org_type)
centrality_org_df <- centrality_org_df %>% relocate(Type, .after = Organization)
centrality_org_df <- centrality_org_df %>% unique()

# join with organization type 

org_type <- place_pos_to_bind %>% select(Set, Type) %>% rename(Organization = Set)
centrality_org_df$Type <- centrality_org_df$Type %>% replace_na("Education") # complete missing values


# extract place details (names of individuals)

split1 <- Result3df %>% mutate(PlaceDetail,
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


# Tie strength 

table(E(PlaNet2)$weight)

E(PlaNet2)[weight == 5]
E(PlaNet2)[weight == 4]
E(PlaNet2)[weight == 3]
E(PlaNet2)[weight == 2]
E(PlaNet2)[weight == 2]



# find communities

## detect communities with Louvain

set.seed(2023)
lvc1 <- cluster_louvain(PlaNet1MC) # groups: 6, mod: 0.42
lvc1b <- cluster_louvain(PlaNet1MC) # 6, mod: 0.42
lvc1c <- cluster_louvain(PlaNet1MC) # 6, mod: 0.42

lvc2 <- cluster_louvain(PlaNet2MC) # 9, mod: 0.55
lvc2b <- cluster_louvain(PlaNet2MC) # 8, mod: 0.55
lvc2c <- cluster_louvain(PlaNet2MC) # 8, mod: 0.54
lvc2d <- cluster_louvain(PlaNet2MC)  # 10, mod: 0.54
lvc2de <- cluster_louvain(PlaNet2MC) # 8, mod: 0.54

# plot communities 

V(PlaNet1MC)$group <- lvc1$membership # create a group for each community
V(PlaNet1MC)$color <- lvc1$membership # node color reflects group membership 

plot(lvc1, PlaNet1MC, vertex.label=V(PlaNet1MC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.7, 
     vertex.size=5,
     main="Communities of American-educated engineers (Louvain method)")

plot(lvc1, PlaNet1MC, 
     vertex.label = NA, 
     vertex.size=5,
     main="Communities of American-educated engineers (Louvain method)")

glvc1 <- induced_subgraph(PlaNet1MC, V(PlaNet1MC)$group==1) # 35 nodes
glvc2 <- induced_subgraph(PlaNet1MC, V(PlaNet1MC)$group==2) # 25 nodes
glvc3 <- induced_subgraph(PlaNet1MC, V(PlaNet1MC)$group==3) # 21 nodes
glvc4 <- induced_subgraph(PlaNet1MC, V(PlaNet1MC)$group==4) # 20 nodes
glvc5 <- induced_subgraph(PlaNet1MC, V(PlaNet1MC)$group==5) # 46 nodes
glvc6 <- induced_subgraph(PlaNet1MC, V(PlaNet1MC)$group==6)


plot(glvc1, 
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     vertex.size= 5,
     vertex.label.family="Helvetica Neue",  
     vertex.label.dist=1.5, 
     main="P1-Cornell")


plot(glvc2, 
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     vertex.size= 5,
     vertex.label.family="Helvetica Neue",  
     vertex.label.dist=1.5, 
     main="P2-Columbia-Michigan-Industries")

plot(glvc3, 
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     vertex.size= 5,
     vertex.label.family="Helvetica Neue",  
     vertex.label.dist=1.5, 
     main="P3-MIT")

plot(glvc4, 
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     vertex.size= 5,
     vertex.label.family="Helvetica Neue",  
     vertex.label.dist=1.5, 
     main="P4-Harvard-Communications")

plot(glvc5, 
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     vertex.size= 5,
     vertex.label.family="Helvetica Neue",  
     vertex.label.dist=1.5, 
     main="P5-Penn-Architects")


plot(glvc6, 
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     vertex.size= 5,
     vertex.label.family="Helvetica Neue",  
     vertex.label.dist=1.5, 
     main="P6-California-Illinois")



# Organizations 

V(PlaNet2MC)$group <- lvc2$membership  # create a group for each community
V(PlaNet2MC)$color <- lvc2$membership # node color reflects group membership 

plot(lvc2, PlaNet2MC, vertex.label=V(PlaNet2MC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     vertex.size=3,
     main="Communities of engineering organizations (Louvain method)")

plot(lvc2, PlaNet2MC, vertex.label = NA,  
     vertex.label.cex = 0.5, 
     vertex.size=3,
     main="Communities of engineering organizations (Louvain method)")


V(PlaNet2MC)$group <- lvc2b$membership  # create a group for each community
V(PlaNet2MC)$color <- lvc2b$membership # node color reflects group membership 

plot(lvc2b, PlaNet2MC, vertex.label=V(PlaNet2MC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.5, 
     vertex.size=3,
     main="Communities of engineering organizations (Louvain method)")

plot(lvc2b, PlaNet2MC, vertex.label = NA,  
     vertex.label.cex = 0.5, 
     vertex.size=3,
     main="Communities of engineering organizations (Louvain method)")

# extract membership 

# places 

places_com <- data.frame(lvc1$membership,
                         lvc1$names) %>% 
  group_by(lvc1.membership) %>% 
  add_tally() %>% # add size of clusters
  rename(PlaceLabel = lvc1.names, lvcluster = lvc1.membership, size = n)

places_com <- left_join(places_com, Result3df)


org_com9 <- data.frame(lvc2$membership,
                      lvc2$names)  %>% 
  group_by(lvc2.membership) %>%  
  add_tally() %>% # add size of clusters
  rename(Organisation = lvc2.names, lvcluster = lvc2.membership, 
         size = n) 

org_com8 <- data.frame(lvc2b$membership,
                       lvc2b$names)  %>% 
  group_by(lvc2b.membership) %>%  
  add_tally() %>% # add size of clusters
  rename(Organization = lvc2b.names, lvcluster = lvc2b.membership, 
         size = n) 


# extract communities (louvain 9)

V(PlaNet2MC)$group <- lvc2$membership  # create a group for each community
V(PlaNet2MC)$color <- lvc2$membership # node color reflects group membership 

glvcu1 <- induced_subgraph(PlaNet2MC, V(PlaNet2MC)$group==1) # 
glvcu2 <- induced_subgraph(PlaNet2MC, V(PlaNet2MC)$group==2) #  
glvcu3 <- induced_subgraph(PlaNet2MC, V(PlaNet2MC)$group==3) # 
glvcu4 <- induced_subgraph(PlaNet2MC, V(PlaNet2MC)$group==4) # 
glvcu5 <- induced_subgraph(PlaNet2MC, V(PlaNet2MC)$group==5) # 
glvcu6 <- induced_subgraph(PlaNet2MC, V(PlaNet2MC)$group==6) # 
glvcu7 <- induced_subgraph(PlaNet2MC, V(PlaNet2MC)$group==7) # 
glvcu8 <- induced_subgraph(PlaNet2MC, V(PlaNet2MC)$group==8)
glvcu9 <- induced_subgraph(PlaNet2MC, V(PlaNet2MC)$group==9)

vertex.label.family="Helvetica"

plot(glvcu1, vertex.label=V(PlaNet2MC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = degree(glvcu1)*0.055, 
     vertex.size= degree(glvcu1)*2, # node size proportionate to node degree (in cluster)
     main="O1-Cornell-Railway")



plot(glvcu2, vertex.label=V(PlaNet2MC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = degree(glvcu2)*0.055, 
     vertex.size= degree(glvcu2)*1.5, # node size proportionate to node degree (in cluster)
     main="O2-Columbia-Michigan-Industries")

plot(glvcu3, vertex.label=V(PlaNet2MC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = degree(glvcu3)*0.055, 
     vertex.size= degree(glvcu3)*1.5, # node size proportionate to node degree (in cluster)
     main="O3-MIT")

plot(glvcu4, vertex.label=V(PlaNet2MC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = degree(glvcu4)*0.1, 
     vertex.size= degree(glvcu4)*2, # node size proportionate to node degree (in cluster)
     main="O4-NCC")

plot(glvcu4, vertex.label=V(PlaNet2MC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.8, 
     vertex.size= 8, # node size proportionate to node degree (in cluster)
     main="O4-NCC-Jiaotong (Tangshan)")

plot(glvcu5, vertex.label=V(PlaNet2MC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = degree(glvcu5)*0.5, 
     vertex.size= degree(glvcu5)*5, # node size proportionate to node degree (in cluster)
     main="O5-River Conservancy (Jiangsu)")

plot(glvcu6, vertex.label=V(PlaNet2MC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = degree(glvcu6)*0.5, 
     vertex.size= degree(glvcu6)*10, # node size proportionate to node degree (in cluster)
     main="O6-River Conservancy (Outside Jiangsu)")

plot(glvcu7, vertex.label=V(PlaNet2MC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = degree(glvcu7)*0.25, 
     vertex.size= degree(glvcu7)*5, # node size proportionate to node degree (in cluster)
     main="O7-New Railway Construction Commission")

plot(glvcu8, vertex.label=V(PlaNet2MC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = degree(glvcu8)*0.2, 
     vertex.size= degree(glvcu8)*2, # node size proportionate to node degree (in cluster)
     main="O8-Harvard-Communications")

plot(glvcu9, vertex.label=V(PlaNet2MC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = degree(glvcu9)*0.225, 
     vertex.size= degree(glvcu9)*5, # node size proportionate to node degree (in cluster)
     main="O9-Lehigh-Northern Railways-Mines")

# extract communities of places 

# extract communities (louvain 9)

V(PlaNet1MC)$group <- lvc1$membership  # create a group for each community
V(PlaNet1MC)$color <- lvc1$membership # node color reflects group membership 

glvcp1 <- induced_subgraph(PlaNet1MC, V(PlaNet1MC)$group==1) # 
glvcp2 <- induced_subgraph(PlaNet1MC, V(PlaNet1MC)$group==2) #  
glvcp3 <- induced_subgraph(PlaNet1MC, V(PlaNet1MC)$group==3) # 
glvcp4 <- induced_subgraph(PlaNet1MC, V(PlaNet1MC)$group==4) # 
glvcp5 <- induced_subgraph(PlaNet1MC, V(PlaNet1MC)$group==5) # 
glvcp6 <- induced_subgraph(PlaNet1MC, V(PlaNet1MC)$group==6) # 


plot(glvcp1, vertex.label=V(PlaNet1MC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = degree(glvcp1)*0.02, 
     vertex.size= degree(glvcp1)*0.25, # node size proportionate to node degree (in cluster)
     main="PC1 - Cornell-trained engineers")


plot(glvcp2, vertex.label=V(PlaNet1MC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = degree(glvcp2)*0.05, 
     vertex.size= degree(glvcp2), # node size proportionate to node degree (in cluster)
     main="PC2 - Columbia-National Construction Commission")

plot(glvcp3, vertex.label=V(PlaNet1MC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = degree(glvcp3)*0.025, 
     vertex.size= degree(glvcp3)*0.25, # node size proportionate to node degree (in cluster)
     main="PC3 - MIT-trained engineers")

plot(glvcp4, vertex.label=V(PlaNet1MC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = degree(glvcp4)*0.1, 
     vertex.size= degree(glvcp4)*2, # node size proportionate to node degree (in cluster)
     main="PC4 - Harvard-Communications")

plot(glvcp5, vertex.label=V(PlaNet1MC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = degree(glvcp5)*0.5, 
     vertex.size= degree(glvcp5)*5, # node size proportionate to node degree (in cluster)
     main="PC5 - Penn-trained Architects")

plot(glvcp6, vertex.label=V(PlaNet1MC)$id,
     vertex.label.color = "black", 
     vertex.label.cex = degree(glvcp6)*0.25, 
     vertex.size= degree(glvcp6)*5, # node size proportionate to node degree (in cluster)
     main="PC6 - Illinois-California-trained Cantonese engineers")

# export edge and node lists 

write.csv(edgelist_engineer_1, "~/aum/data/engineers/output/edgelist_engineer_1.csv")
write.csv(edgelist_engineer_2, "~/aum/data/engineers/output/edgelist_engineer_2.csv")
write.csv(element_node, "~/aum/data/engineers/output/org_nodelist.csv")
write.csv(centrality_org_df, "~/aum/data/engineers/output/centrality_org.csv")
write.csv(centrality_places_df, "~/aum/data/engineers/output/centrality_places.csv")
write.csv(places_com, "~/aum/data/engineers/output/places_com.csv")
write.csv(org_com8, "~/aum/data/engineers/output/org_com8.csv")
write.csv(org_com9, "~/aum/data/engineers/output/org_com9.csv")


## identify engineers who cut accross sectors

subfield <- engineers_positions %>% group_by(Subfield, NameID, Type) %>% count()
cross <- subfield %>% group_by(Subfield, NameID) %>% count() %>% filter(n>1) # 32 cross-sector

cross_filtered <- engineers_positions %>% filter(NameID %in% cross$NameID)

non_cross <- subfield %>% group_by(Subfield, NameID) %>% count() %>% filter(n<2)  

non_cross_filtered <- engineers_positions %>% filter(NameID %in% non_cross$NameID)
