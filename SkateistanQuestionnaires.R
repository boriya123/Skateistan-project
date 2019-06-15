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
library(tidytext)##
library(ggpubr)## ballomnplot
library(readxl)##reading excel
library(stringi)#string operations
library(tm)##wordcloud
library(SnowballC)##wordcloud
library(wordcloud)##wordcloud
library(RColorBrewer)##wordcloud
library(ggcorrplot)##correlogram plot
library(qdapRegex) ##removing emotions


### setting up directory

setwd("D://personal/swb project/files_bts")


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
Freq_table_gender_location_df<- Freq_table_gender_location_df[c(-1,-2),]### freq table for gender based on location 
names(Freq_table_gender_location_df)[1:2]<-paste(c("Project_Sites","Gender"))  ### renaming the column names

### creating an barplot ---- freq count of gender in project_sites 

gender_count_barplot<-ggplot(data=Freq_table_gender_location_df, aes(x=Project_Sites, y=Freq, fill=Gender)) +geom_bar(stat="identity", position=position_dodge())
gender_count_barplot<-gender_count_barplot+geom_text(aes(label=Freq), vjust=1.6, color="white",position = position_dodge(0.9), size=3.5)+scale_fill_brewer(palette="Paired")+theme_minimal()                
gender_count_barplot

#### role model studies

#extracting all role model columns with gender and project sites
Freq_table_role_model<- PreQ.PrevYear %>% 
  dplyr:: select("Project_Site","Gender","Role Model - Father","Role Model - Brother/Sister","Role Model - Other family member","Role Model - Friends at Skateistan","Role Model - Skateistan Educator","Role Model - Someone older than me at Skateistan","Role Model - Other","Role Model - Would you like to say who?")
Freq_table_role_model[Freq_table_role_model==""]<- "Not Available"  # removing blanks with NA values



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
rolemodel_freqcount_balloonplot


### heatmap
role_model_freq_counts_reshape_heatmap<-role_model_freq_counts_reshape%>%spread(L1,value)
role_model_freq_counts_reshape_heatmap<- as.data.frame(role_model_freq_counts_reshape_heatmap)
role_model_freq_counts_reshape_heatmap[is.na(role_model_freq_counts_reshape_heatmap)] <- 0
role_model_freq_counts_reshape_heatmap2 <- as.matrix(role_model_freq_counts_reshape_heatmap[, -1])
rownames(role_model_freq_counts_reshape_heatmap2) <- role_model_freq_counts_reshape_heatmap$Var1
heatmap(role_model_freq_counts_reshape_heatmap2)

### correlgram

role_model_correltion<-role_model_freq_counts_reshape_heatmap[,-1]
rownames(role_model_correltion)<-colnames(role_model_correltion)
M<-round(cor(role_model_correltion),1)
ggcorrplot(M,hc.order = TRUE,type='lower',lab=TRUE,lab_size=3, method="circle",colors = c("tomato2", "white", "springgreen3"), ggtheme=theme_bw,title="Correlogram of role models")



#### sentimental analysis #####

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

word_rolemodel<-wordcloud(words = d$word, freq = d$freq, min.freq = 1,max.words=100, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
word_rolemodel


###################################################################################
###################################################################################
###loding files

### pre questionnaire BTS 2018-19


PreQ.CurrYear <- read_excel("Copy of Student Pre-Questionnaire Back-to-School 2018-2019 Group (Afghanistan).xlsx")




PreQ.CurrYear <- tail(PreQ.CurrYear,-1)




require(dplyr)
PreQ.CurrYear <- PreQ.CurrYear %>%
  #as.Date(as.character(as.POSIXct("Start Date"),format="%Y-%m-%d")) %>%
  #as.Date(as.character(as.POSIXct("End Date"),format="%Y-%m-%d")) %>%
  replace(is.na(.), "") %>%
  tidyr::unite("Project.Site", "Project Site","...11", sep = "", remove = TRUE) %>%
  tidyr::unite("Gender", "What is your gender?", "...14","...15", sep = "", remove = TRUE) %>%
  tidyr::unite("I believe I can work well in a team/group", "I believe I can......16", "...17","...18", sep = "", remove = TRUE) %>%
  tidyr::unite("I believe I can speak in front of the class (public speaking)", "...19", "...20","...21", sep = "", remove = TRUE) %>%
  tidyr::unite("I believe I can help other people", "...22", "...23","...24", sep = "", remove = TRUE) %>%
  tidyr::unite("I believe I can ask for help when I have a problem", "...25", "...26","...27", sep = "", remove = TRUE) %>%
  tidyr::unite("I believe I can make mistakes and learn from them", "...28", "...29","...30", sep = "", remove = TRUE) %>%
  dplyr::rename("Examples (I believe section)" = "...31") %>%
  tidyr::unite("How do you feel traveling to/from Skateistan", "How do you feel...", "...33","...34", sep = "", remove = TRUE) %>%
  tidyr::unite("How do you feel in the skatepark", "...35", "...36","...37", sep = "", remove = TRUE) %>%
  tidyr::unite("How do you feel in the classroom", "...38", "...39","...40", sep = "", remove = TRUE) %>%
  tidyr::unite("How do you feel talking with an Educator at Skateistan", "...41", "...42","...43", sep = "", remove = TRUE) %>%
  tidyr::unite("How do you feel learning with my class volunteer / youth leader", "...44", "...45","...46", sep = "", remove = TRUE) %>%
  dplyr::rename("Examples (How do you feel section)" = "...47") %>%
  tidyr::unite("In general I like myself", "In general I.", "...49","...50", sep = "", remove = TRUE) %>%
  dplyr::rename("What makes you feel good/confident? What do you like about yourself?" = "...51") %>%
  tidyr::unite("I believe I can spend time studying", "I believe I can......52", "...53","...54", sep = "", remove = TRUE) %>%
  tidyr::unite("I believe I can do well in school", "...55", "...56","...57", sep = "", remove = TRUE) %>%
  tidyr::unite("I believe I can enjoy sport/skateboarding skills", "...58", "...59","...60", sep = "", remove = TRUE) %>%
  tidyr::unite("I believe I can learn new skateboard tricks", "...61", "...62","...63", sep = "", remove = TRUE) %>%
  dplyr::rename("Examples (I believe I can section)" = "...64") %>%
  tidyr::unite("I am good at reading", "I am good at....", "...66","...67", sep = "", remove = TRUE) %>%
  tidyr::unite("I am good at writing", "...68", "...69","...70", sep = "", remove = TRUE) %>%
  tidyr::unite("I am good at math", "...71", "...72","...73", sep = "", remove = TRUE) %>%
  tidyr::unite("I am good at skateboarding", "...74", "...75","...76", sep = "", remove = TRUE) %>%
  tidyr::unite("I am good at other sports", "...77", "...78","...79", sep = "", remove = TRUE) %>%
  dplyr::rename("Examples (I am good at section)" = "...80") %>%
  tidyr::unite("Over the past 2-3 weeks I have felt happy", "Over the past 2-3 weeks...", "...82","...83", sep = "", remove = TRUE) %>%
  tidyr::unite("Over the past 2-3 weeks I have felt calm and relaxed", "...84", "...85","...86", sep = "", remove = TRUE) %>%
  tidyr::unite("Over the past 2-3 weeks I wake up feeling fresh and rested", "...87", "...88","...89", sep = "", remove = TRUE) %>%
  tidyr::unite("Over the past 2-3 weeks My daily life has been filled with things that interest me", "...90", "...91","...92", sep = "", remove = TRUE) %>%
  dplyr::rename("Role Model - Mother" = "When I have problems or I am upset, I can talk to... (you can choose more than one) (Role Model)") %>%
  dplyr::rename("Role Model - Father" = "...94") %>%
  dplyr::rename("Role Model - Brother" = "...95") %>%
  dplyr::rename("Role Model - Sister" = "...96") %>%
  dplyr::rename("Role Model - Other family member" = "...97") %>%
  dplyr::rename("Role Model - Friends at Skateistan" = "...98") %>%
  dplyr::rename("Role Model - Skateistan Educator" = "...99") %>%
  dplyr::rename("Role Model - Someone older than me at Skateistan" = "...100") %>%
  dplyr::rename("Role Model - Other" = "...101") %>%
  dplyr::rename("Role Model - Not sure" = "...102") %>%
  dplyr::rename("Role Model - Would you like to say who? How do you feel when you talk to them about your problems?" = "...103") %>%
  tidyr::unite("My parents/family want me to complete my education until", "My parents/family want me to complete my education until...", "...105","...106", "...107", "...108", sep = "", remove = TRUE) %>%
  dplyr::rename("Education level - Why/why not?" = "...109")

### removing smiley
PreQ.CurrYear[] <- lapply(PreQ.CurrYear, rm_emoticon)





 
 























































