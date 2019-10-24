setwd("~/Analysis/")
library(tidyverse)
library(formattable)
library(tm)
library(rlist)
library(wordcloud)

df <- readxl::read_excel("~/Analysis/job_skills.xlsx")

colnames(df) <- c("company", "title", "category", "location",
                  "responsibilities", "minimumQualifications", 
                  "preferredQualifications")

# Incidence on Categories -------------------------------------------------

# Minimum Qualifications on Categories
dfMinQual <- df[,c("company", "title", "category", "location", "minimumQualifications")]
dfMinQual <- separate_rows(dfMinQual,"minimumQualifications", sep = "\n")

# Testing for data homogeneity
dfMinQual <- dfMinQual[str_detect(dfMinQual$minimumQualifications, regex("experience", ignore_case = TRUE)),]

# How many years of experience are necessary for these jobs?
dfMinQual$necessaryYears <- str_extract(dfMinQual$minimumQualifications, "[1-9]")
dfMinQual$necessaryYears[is.na(dfMinQual$necessaryYears)] <- "Undefined"

dfMinQualPlot <- dfMinQual[,c("category", "necessaryYears")]
dfMinQualPlot <- dfMinQualPlot %>% 
  group_by(category, necessaryYears) %>% 
  add_tally() %>% 
  unique()

dfMinQualPlot <- dfMinQualPlot[dfMinQualPlot$necessaryYears != "Undefined",]

dfMinQualPlot2 <- dfMinQualPlot %>% 
  group_by(category) %>% 
  summarise(n = sum(n)) %>% 
  arrange(desc(n)) %>% 
  ungroup()

top12 <- head(dfMinQualPlot2$category, 12)

dfMinQualPlot <- dfMinQualPlot[dfMinQualPlot$category %in% top12,]

dfMinQualPlot$category <- factor(dfMinQualPlot$category, levels = top12)

dfMinQualPlot <- arrange(dfMinQualPlot, category)

dfMinQualPlot$n <- as.numeric(dfMinQualPlot$n)
colnames(dfMinQualPlot) <- c("category", "necessaryYears", "numberOfTimes")
dfMQPlot <- ggplot(dfMinQualPlot, aes(x = reorder(dfMinQualPlot$necessaryYears, -numberOfTimes), y = dfMinQualPlot$numberOfTimes), fill = "category") +
  geom_bar(stat = 'identity') +
  xlab("Necessary years of experience") +
  ylab("Number of times requested") +
  ggtitle("Necessary years of experience x Number of times they were requested by Category") +
  theme_bw() +
  geom_line(y = mean(as.numeric(dfMinQualPlot$numberOfTimes))) +
  facet_wrap(.~ category, nrow = 3)

areas <- unique(df$category)

for(i in areas){
  if(exists("eachCategory")){
    eachCategory <- list.append(eachCategory, df[df$category == i,]) 
  }else{
    eachCategory <- list(df[df$category == i,])
  }
}

names(eachCategory) <- areas

for(i in 1:length(eachCategory)){
  eachCategory[[i]]$category <- str_remove_all(eachCategory[[i]]$category, "[\\(\\)\\.\\,]")
  eachCategory[[i]]$category <- str_replace_all(eachCategory[[i]]$category, "\\/", " ")
}

freqCat_function <- function(x){
freqCat <- as.data.frame(termFreq(eachCategory[[x]]$minimumQualifications))
colnames(freqCat) <- "frequency"
freqCat$word <- row.names(freqCat)
freqCat$word <- str_remove_all(freqCat$word, "[\\(\\)\\.\\,]")
freqCat$word <- str_replace_all(freqCat$word, "\\/", " ")
return(freqCat)
}


freqCat <- lapply(names(eachCategory), freqCat_function)

names(freqCat) <- areas
for(i in areas){
wcloud <- wordcloud2(freqCat[[i]][,c(2,1)])
htmlwidgets::saveWidget(wcloud,paste0("wcloud_",i,".html"),selfcontained = F)
}

# Preferred Qualifications on Categories
dfPrefQual <- df[,c("company", "title", "category", "location", "preferredQualifications")]
dfPrefQual <- separate_rows(dfPrefQual,"preferredQualifications", sep = "\n")
