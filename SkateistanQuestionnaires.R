### setting up directory

setwd("D://personal/swc project/files")

##initialize libarry

library(dplyr)
library(ggplot2)
library(tidyr)
library(xlsx)
library(tidyverse)
library(plyr)
library(reshape2)
library(wordcloud)
library(tidytext)
library(ggpubr)
library(readxl)
library(stringi)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")





###loding files

### pre questionnaire BTS 2017-18

PreQ.PrevYear <- read_excel("Copy of Student Pre-Questionnaire Back-to-School 2017-2018 Group (Afghanistan).xlsx")

### cleaning the datasets:

PreQ.PrevYear <- tail(PreQ.PrevYear,-1)
# Removing emoticons because one set of questionnaire responses have them and the other don't
PreQ.PrevYear[] <- lapply(PreQ.PrevYear, gsub, pattern=':)', replacement='')
PreQ.PrevYear[] <- lapply(PreQ.PrevYear, gsub, pattern=':D', replacement='')
PreQ.PrevYear[] <- lapply(PreQ.PrevYear, gsub, pattern=':|', replacement='')
PreQ.PrevYear[] <- lapply(PreQ.PrevYear, gsub, pattern=':/', replacement='')
PreQ.PrevYear[] <- lapply(PreQ.PrevYear, gsub, pattern='_', replacement='')
PreQ.PrevYear[] <- lapply(PreQ.PrevYear, gsub, pattern='|', replacement='')
PreQ.PrevYear[] <- lapply(PreQ.PrevYear, gsub, pattern='/', replacement='')

#### renaming and uniting all the columns  ####

PreQ.PrevYear <- PreQ.PrevYear %>%
  replace(is.na(.), "") %>%
  dplyr::rename("Project_Site" = "Project Site:") %>%
  dplyr::rename("Gender" = "What is your gender?") %>%
  dplyr::rename("My experience coming to BTS classes has been" = "My experience coming to BTS classes has been... [R5]") %>%
  dplyr::rename("Why (experience coming to BTS classes)" = "...13") %>%
  dplyr::rename("Why (feeling safe at Skateistan)" = "...15") %>%
  dplyr::rename("Confidence since joining the program has" = "Having confidence means that you feel good about yourself and you believe you can take on new challenges, big or small, that you may face in life. Since joining the program my confidence has... [R2]") %>%
  dplyr::rename("Comment on confidence levels" = "...17") %>%
  dplyr::rename("I have learned a lot of useful skills and lessons since coming to BTS classes at Skateistan" = "I have learned a lot of useful skills and lessons since coming to BTS classes at Skateistan.") %>%
  dplyr::rename("Comment on useful skills learnt" = "...19") %>%
  dplyr::rename("Friends made at Skateistan" = "Since coming to Skateistan I have made a new friend from a different background (ethnicity, disability, family income)") %>%
  dplyr::rename("How many friends made/background" = "...21") %>%
  dplyr::rename("Role Model - Mother" = "When I have problems or I am upset, I can talk to: (you can choose more than one answer)") %>%
  dplyr::rename("Role Model - Father" = "...23") %>%
  dplyr::rename("Role Model - Brother/Sister" = "...24") %>%
  dplyr::rename("Role Model - Other family member" = "...25") %>%
  dplyr::rename("Role Model - Friends at Skateistan" = "...26") %>%
  dplyr::rename("Role Model - Skateistan Educator" = "...27") %>%
  dplyr::rename("Role Model - Someone older than me at Skateistan" = "...28") %>%
  dplyr::rename("Role Model - Other" = "...29") %>%
  dplyr::rename("Role Model - Would you like to say who?" = "...30") %>%
  dplyr::rename("Comment on reading and writing" = "...32") %>%
  dplyr::rename("Comment on good at math" = "...34") %>%
  dplyr::rename("Comment on good at skateboarding/sports" = "...36") %>%
  tidyr::unite("Tell us about your work history", "Tell us about your work history", "...38","...39", "...40", sep = "", remove = TRUE) %>%
  dplyr::rename("Would you like to say what kind of work? If you have stopped working, please explain why" = "...41") %>%
  dplyr::rename("My parents/family want me to complete my education until" = "My parents/family want me to complete my education until...") %>%
  dplyr::rename("Education level - Why/why not?" = "...43")
PreQ.PrevYear$`Friends made at Skateistan`[PreQ.PrevYear$`Friends made at Skateistan` == "Yes, more than one new friend from a different background"] <- "Yes, more than one new friend"
PreQ.PrevYear$`Friends made at Skateistan`[PreQ.PrevYear$`Friends made at Skateistan` == "Yes, one new friend from a different background"] <- "Yes, one new friend"
PreQ.PrevYear$`I am good at reading and writing`[PreQ.PrevYear$`I am good at reading and writing` == "Neural"] <- "Neutral"
PreQ.PrevYear$`I am good at reading and writing`[PreQ.PrevYear$`I am good at reading and writing` == "Strongly agree"] <- "Strongly Agree"



#### Visulization ######

### 1. gender studies

# cross tab for frequency counts for location and gender

Freq_table_gender_location<- table(PreQ.PrevYear$Project_Site,PreQ.PrevYear$Gender)
Freq_table_gender_location_df<- as.data.frame(Freq_table_gender_location)
Freq_table_gender_location_df<- Freq_table_gender_location_df[c(-1,-2),]  ### freq table for gender based on location 
names(Freq_table_gender_location_df)[1:2]<-paste(c("Project_Sites","Gender"))  ### renaming the column names

### creating an barplot ---- freq count of gender in project_sites 

gender_count_barplot<-ggplot(data=Freq_table_gender_location_df, aes(x=Project_Sites, y=Freq, fill=Gender)) +geom_bar(stat="identity", position=position_dodge())
gender_count_barplot<-gender_count_barplot+geom_text(aes(label=Freq), vjust=1.6, color="white",position = position_dodge(0.9), size=3.5)+scale_fill_brewer(palette="Paired")+theme_minimal()                
gender_count_barplot

#### role model studies

#extracting all role model columns with gender and project sites
Freq_table_role_model<- PreQ.PrevYear %>% 
  dplyr:: select("Project_Site","Gender","Role Model - Father","Role Model - Brother/Sister","Role Model - Other family member","Role Model - Friends at Skateistan","Role Model - Skateistan Educator","Role Model - Someone older than me at Skateistan","Role Model - Other","Role Model - Would you like to say who?")
Freq_table_role_model[Freq_table_role_model==""]<- "NULL"  # removing blanks with NA values



#### transforming role model column and calculating frequency count####

role_model_freq_counts<- lapply(Freq_table_role_model,table)

### converting to dataframe and reshape the structure of dataframe
role_model_freq_counts_reshape<-as.data.frame(melt(role_model_freq_counts)) #### reshaping the list to an required table
col_role_model<- c("L1","Var1","value")## column na,es of reshape dataframe
role_model_freq_counts_reshape<- role_model_freq_counts_reshape[,col_role_model] ## arranging column for role model dataframe


## spreading the structure of dataframe
role_model_freq_counts_reshape_table<-role_model_freq_counts_reshape%>%spread(Var1,value)


### Balloon plot of role model

rolemodel_freqcount_balloonplot<- ggballoonplot(role_model_freq_counts_reshape, fill = "value")+
  scale_fill_viridis_c(option = "C")


## sentimental analysis

#extracting all the Role Model - Would you like to say who? rows 

role_model_who_says<- role_model_freq_counts_reshape %>% 
  filter(L1=="Role Model - Would you like to say who?")

## transforming text from role model

role_model_who_says<-role_model_who_says[2]
role_vec <- as.matrix(role_model_who_says)
role_vec_spl_cha_remove<- gsub(".","",role_vec,fixed=TRUE)
role_vec_spl_cha_remove<- as.vector(role_vec_spl_cha_remove)


## extract role ansers  columns

##corpus

docs<- Corpus(VectorSource(role_vec_spl_cha_remove))

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)



dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
rownames(d)<- NULL ## remove rownames from dataframe
d<-d %>% filter(word !="null")

p<-ggplot(d,aes(word,freq))+geom_bar(stat="identity")+geom_text(aes(label=freq))
p<-p+theme(axis.text.x = element_text(angle = 90, hjust = 1))

word_rolemodel<-wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
word_rolemodel



###loding files

### pre questionnaire BTS 2017-18

PreQ.PrevYear <- read_excel("Copy of Student Pre-Questionnaire Back-to-School 2017-2018 Group (Afghanistan).xlsx")










 
 
 























































