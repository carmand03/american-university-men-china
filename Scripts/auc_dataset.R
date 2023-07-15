# DATA DESCRIPTION

save.image("stats.RData")
load("~/aum/stats.RData")


# load packages

library(readr)
library(tidyverse)
library(plotly)
library(FactoMineR)
library(Factoshiny)

# load data
aucv2 <- read_delim("data/aucv2.csv", delim = ";",
escape_double = FALSE, trim_ws = TRUE)

names(aucv2)

# Nationality 

aucv2 %>% distinct(Name_full, Nationality) %>% group_by(Nationality) %>% count() # individuals
aucv2 %>% group_by(Nationality) %>% count() # degrees 

# Average degrees 

aucv2 %>% group_by(Name_full) %>% count()



univ_count <- aucv2 %>% group_by(University) %>% count() %>% mutate(percent = n/682*100)
univ_count <- aucv2 %>% group_by(University) %>% count()

aucv2 %>% 
  group_by(University)%>%
  count()%>%
  filter(n>10)%>%
  ggplot(aes(reorder(x=University, n), y =n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Most attended universities", 
       x = "Institutions totalling more than 5 curricula",
       y = "Number of curricula", 
       fill = NULL,
       caption = "Based on 'American University Men in China' (1936)")


univ_count %>% slice_max(n, n = 10)
top10 %>% slice_max(n, n = 2)

top10 <- aucv2 %>% 
  group_by(University)%>%
  count(sort = TRUE) %>% filter(n>20) 

top14 <- aucv2 %>% 
  group_by(University)%>%
  count(sort = TRUE) %>% filter(n>10) 

sum(top14$n)

# Periodization

integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

library(plotly)

p1 <- aucv2 %>% 
  drop_na(Year_start) %>%
  ggplot(aes(x=Year_start, fill = Nationality)) +
  geom_histogram() +
  scale_x_continuous(breaks = integer_breaks())+ 
  labs(title = "American University Men in China", 
       subtitle = "Period of study", 
       x = "Year",
       y = "Number of curricula", 
       fill = "Nationality", 
       caption = "Based on 'American University Men in China' (1936)")

fig1 <- ggplotly(p1)

fig1


p2 <- aucv2  %>% drop_na(Year_start) %>%
  ggplot(aes(reorder(Nationality, Year_start), Year_start, fill = Nationality)) +
  geom_boxplot(alpha = 0.8, show.legend = FALSE) +
  coord_flip()+
  labs(x = NULL, y = "Year")+  
  labs(title = "Period of studies: Comparative Periodization", 
       caption = "Based on 'American University Men in China' (1936)") 

fig2 <- ggplotly(p2)

fig2 %>% hide_legend()

# Degree and field of study 

field_nation_count <- aucv2 %>% distinct(Name_full, Nationality, Field_2) %>% 
  drop_na(Field_2) %>% 
  group_by(Nationality, Field_2) %>% count()

field2_chinese <- aucv2 %>% filter(Nationality == "Chinese") %>% 
  distinct(Name_full, Field_main, Field_2) %>% 
  drop_na(Field_2) %>% 
  group_by(Field_main, Field_2) %>% count(sort = TRUE)%>% 
  mutate(percent = n/265*100)

field_chinese <- aucv2 %>% filter(Nationality == "Chinese") %>% 
  distinct(Name_full, Field_main) %>% 
  drop_na(Field_main) %>% 
  group_by(Field_main) %>% count(sort = TRUE)  %>% 
  mutate(percent = n/265*100) # 265


field2_non_chinese <-aucv2 %>% filter(!Nationality == "Chinese") %>% 
  distinct(Name_full, Field_main, Field_2) %>% 
  drop_na(Field_2) %>% 
  group_by(Field_main, Field_2) %>% count() %>% 
  mutate(percent = n/187*100) # 187

field_non_chinese <- aucv2 %>% filter(!Nationality == "Chinese") %>% 
  distinct(Name_full, Field_main) %>% 
  drop_na(Field_main) %>% 
  group_by(Field_main) %>% count(sort = TRUE) %>% 
  mutate(percent = n/187*100) # 187

degree_non_chinese <- aucv2 %>% filter(!Nationality == "Chinese") %>% 
  distinct(Name_full, Degree_source) %>% 
  drop_na(Degree_source) %>% 
  group_by(Degree_source) %>% count(sort = TRUE) %>% 
  mutate(percent = n/226*100) 

degree_chinese <- aucv2 %>% filter(Nationality == "Chinese") %>% 
  distinct(Name_full, Degree_source) %>% 
  drop_na(Degree_source) %>% 
  group_by(Degree_source) %>% count(sort = TRUE) %>% 
  mutate(percent = n/345*100) 

# Employment data

unique <- aucv2 %>% distinct(Name_full, Employer_main)
sum(is.na(unique$Employer_main))
employer_main <- aucv2 %>% distinct(Name_full, Sector_1, Employer_main) %>% group_by(Sector_1) %>% count(Employer_main)
sector1<- aucv2 %>% distinct(Name_full, Sector_1) %>% group_by(Sector_1) %>% count()
aucv2 %>% distinct(Employer_main, Sector_1) %>% group_by(Sector_1) %>% count(sort = TRUE)

# exploration 

employment <- aucv2 %>% distinct(Name_full, Nationality, Sector_1, Employer_main) %>% filter(Sector_1 == "Central Government") 


# Correspondence analysis : code for making the four plots
  # sector 1 and field main
  # engineers
  # degree and field main
  # degree and field 2 

# focus on engineering

engineer <- aucv2 %>% distinct(Name_full, Field_main, Field_2, Sector_1, Sector_2) %>% filter(Field_main == "Engineering")
engineer_ca <- engineer %>% select(Field_2, Sector_2) %>% 
  group_by(Field_2, Sector_2) %>% 
  drop_na(Field_2) %>%
  drop_na(Sector_2) %>% 
  tally() %>% 
  spread(key = Field_2, value = n) 

# replace NA with 0
engineer_ca <- mutate_all(engineer_ca, ~replace(., is.na(.), 0))

# read first column as row names 
engineer_ca_tbl <- column_to_rownames(engineer_ca, var = "Sector_2") 

library(FactoMineR)
res.ca2 <- CA(engineer_ca_tbl)

Factoshiny(engineer_ca_tbl)

res.CA<-CA(engineer_ca_tbl,graph=FALSE)
plot.CA(res.CA,cex=0.9,cex.main=0.9,cex.axis=0.9,title="Employment of American-trained engineers, by specialization")

# cross-examine sector of employment (main/sub) and colleges

college_sector1 <- aucv2 %>% distinct(Name_full, University, Sector_1) %>% select(University, Sector_1)
college_sector2 <- aucv2 %>% distinct(Name_full, University, Sector_2) %>% select(University, Sector_2)

link <- college_sector1 %>%
  rename(source = University, target = Sector_1) %>%
  group_by(source, target) %>% 
  count() %>% 
  drop_na(target) %>% 
  rename(value = "n")

link <- college_sector2 %>%
  rename(source = University, target = Sector_2) %>%
  group_by(source, target) %>% 
  count() %>% 
  drop_na(target) %>% 
  rename(value = "n")

link_filter <- link %>% filter(value>1)

node <- data.frame(
  name=c(as.character(link_filter$source), 
         as.character(link_filter$target)) %>% unique()
)


# create unique id for each connection
link_filter$IDsource <- match(link_filter$source, node$name)-1 
link_filter$IDtarget <- match(link_filter$target, node$name)-1


# Make the flow chart

library(networkD3)

p <- sankeyNetwork(Links = link_filter, Nodes = node,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE)
p

# save the widget
library(htmlwidgets)
saveWidget(p, file=paste0( getwd(), "/univ_sector1.html"))
saveWidget(p, file=paste0( getwd(), "/univ_sector2.html"))

# Number of degrees (all, compare Chinese/Americans)
# Length of study (use time slices in auc4, all, compare Chinese and US, and periods)
# field main (compare Chinese and US, and periods)
# Honorary degree (context, examples)

# Number of degrees: average : 1.63 = 682/418

# Chinese average # 1.71

aucv2 %>% filter(Nationality == "Chinese") # 401 Chinese curricula
aucv2 %>% filter(Nationality == "Chinese") %>% distinct(Name_full) # 234 individuals
401/234 # 1.71

# Non-Chinese average # 1.52

aucv2 %>% filter(!Nationality == "Chinese") # 281 non-Chinese curricula
aucv2 %>% filter(!Nationality == "Chinese") %>% distinct(Name_full) # 184 non-Chinese individuals
281/184 # 1.52

aucv2 %>% filter(Nationality == "Western") # 275 Western (American) curricula
aucv2 %>% filter(Nationality == "Western") %>% distinct(Name_full) # 180 Western individuals
275/180 # 1.53

aucv2 %>% filter(Nationality == "Japanese") # 6 non-Chinese curricula
aucv2 %>% filter(Nationality == "Japanese") %>% distinct(Name_full) # 4 Japanese individuals
6/4 # 1.5

# box plot 

degree_count <- aucv2 %>% 
  drop_na(Nationality) %>% 
  group_by(Name_full, Nationality) %>% 
  tally()

p1 <- plot_ly(degree_count, x = ~Nationality, y = ~n) %>%
  add_boxplot(color = ~Nationality) 

p1 %>% 
  layout(title = list(text = "Chinese and non-Chinese graduates in the United States"), 
         yaxis = list(title = "Number of curricula per individual")) %>% hide_legend()


# number of degree and major field of study 

# load highest degrees 

library(readr)
highest_uniq <- read_delim("data/highest_uniq.csv",
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)

high_degree <- highest_uniq %>% select(Name_eng, Field_2, Field_main) %>% 
  mutate(field1 = replace_na(Field_main, "Undetermined")) %>% 
  mutate(field2 = replace_na(Field_2, "Undetermined"))%>% 
  rename(Name_full = Name_eng)
high_degree$Field_main <- NULL
high_degree$Field_2  <- NULL

# join with full list of curricula 

aucv3 <- aucv2 %>% select(Name_full, Degree_source, Year_start) %>% 
  rename(Year = Year_start, Degree= Degree_source)

aucv3 <- full_join(aucv3, high_degree)

# box plot 

degree_field_count <- aucv3 %>% 
  drop_na(field1) %>% 
  group_by(Name_full, field1) %>% 
  tally()

p2 <- plot_ly(degree_field_count, x = ~field1, y = ~n) %>%
  add_boxplot(color = ~field1) 


p2 %>% 
  layout(title = list(text = "American University Men of China"), 
         yaxis = list(title = "Number of curricula per discipline")) %>% hide_legend()


new_order <- with(degree_field_count, reorder(field1, n, median , na.rm=T))

# Draw the boxplot using this new order
boxplot(degree_field_count$field1 ~ new_order , ylab="sickness" , col="#69b3a2", boxwex=0.4 , main="")

library(tidyverse)
library(hrbrthemes)
library(viridis)
library(forcats)

p2 <- degree_field_count %>%
  mutate(field1 = fct_reorder(field1, n, .fun='median')) %>%
  ggplot( aes(x=reorder(field1, n), y=n, fill=field1)) +
  geom_boxplot() +
  coord_flip()+
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  ggtitle("American University Men of China (1936)") +
  xlab("") +
  ylab("Number of curricula per major field of study")

ggplotly(p2)

degree_field_count2 <- aucv3 %>% 
  drop_na(field2) %>% 
  group_by(Name_full, field2) %>% 
  tally()

p3 <- degree_field_count2 %>%
  mutate(field2 = fct_reorder(field2, n, .fun='median')) %>%
  ggplot( aes(x=reorder(field2, n), y=n, fill=field2)) +
  geom_boxplot() +
  coord_flip()+
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  ggtitle("American University Men of China (1936)") +
  xlab("") +
  ylab("Number of curricula per major field of study")

ggplotly(p3)


# length of study 

auc_length <- aucv2 %>% 
  pivot_longer(
    cols = starts_with("Year"),
    names_to = "start_end",
    names_prefix = "Year",
    values_to = "year") %>%
  mutate(start_end = str_remove_all(start_end,"_"))


auc_length <- auc_length %>%
  group_by(Name_full, Nationality) %>%
  summarise(start = min(year), end = max(year)) %>%
  mutate(length = (end-start+1))

auc_length <- full_join(auc_length, high_degree)

# length and nationality 

mean(auc_length$length,na.rm = TRUE)

mean(auc_length$length,na.rm = TRUE) # 3.904077
median(auc_length$length,na.rm = TRUE) # 1  
sd(auc_length$length,na.rm = TRUE) # 5.770066

chinese_length <- auc_length %>% filter(Nationality == "Chinese") 
mean(chinese_length$length, na.rm = TRUE) # 3.166667
sd(chinese_length$length, na.rm = TRUE) # 3.479246

western_length <- auc_length %>% filter(Nationality == "Western")
mean(western_length$length,na.rm = TRUE) # 4.877095
sd(western_length$length, na.rm = TRUE) # 7.744261

japanese_length <- auc_length %>% filter(Nationality == "Japanese") 
mean(japanese_length$length, na.rm = TRUE) # 3.5
sd(japanese_length$length, na.rm = TRUE) # 4.358899

plot_ly(auc_length, x = ~Nationality, y = ~length) %>%
  add_boxplot(color = ~Nationality) 

p4 <- auc_length %>%
  mutate(Nationality = fct_reorder(Nationality, length, .fun='median')) %>%
  ggplot( aes(x=reorder(Nationality, length), y=length, fill=Nationality)) +
  geom_boxplot() + 
  coord_flip()  + 
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  ggtitle("American University Men of China (1936)") +
  xlab("") +
  ylab("Length of studies (Years)")

ggplotly(p4)


# length and field of study 

p5 <- auc_length %>%
  mutate(field1 = fct_reorder(field1, length, .fun='median')) %>%
  ggplot( aes(x=reorder(field1, length), y=length, fill=field1)) +
  geom_boxplot() + 
  coord_flip()  + 
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  ggtitle("American University Men of China (1936)") +
  xlab("") +
  ylab("Length of studies (Years)")

ggplotly(p5)

# time slices (auc4)



# Honorary degree (context, examples)

honorary1 <- aucv2 %>% filter(Honorary == "Honorary")
honorary2 <- aucv2 %>% filter(Degree_source %in% c("LL.D.", "D.D."))
honorary <- bind_rows(honorary1, honorary2)
honorary <- honorary %>% unique()

honorary %>% group_by(Degree_source) %>% count(sort = TRUE)
