setwd("~/Analysis/")
library(tidyverse)
library(formattable)
library(tm)
library(rlist)
library(wordcloud)
library(wordcloud2)
library(tidytext)

df <- readxl::read_excel("~/Analysis/job_skills.xlsx")
languages <- read_csv("~/Analysis/languages.csv")
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
dfMinQual$necessaryYears <- str_extract(dfMinQual$minimumQualifications, "[0-9]*")
dfMinQual$necessaryYears[dfMinQual$necessaryYears == ""] <- "Undefined"

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
dfMinQualPlot$necessaryYears <- as.numeric(dfMinQualPlot$necessaryYears)
dfMQPlot <- ggplot(dfMinQualPlot, aes(x = reorder(dfMinQualPlot$necessaryYears, necessaryYears), y = dfMinQualPlot$numberOfTimes), fill = "category") +
  geom_bar(stat = 'identity') +
  xlab("Necessary years of experience") +
  ylab("Number of times requested") +
  ggtitle("Necessary years of experience x Number of times they were requested by Category") +
  theme_bw() +
  facet_wrap(.~ category, nrow = 3)

dfMinQualPlotMedian <- dfMinQualPlot %>% 
  group_by(category) %>% 
  summarise(median = median(necessaryYears)) %>% 
  arrange(desc(median))
colnames(dfMinQualPlotMedian) <- c("Category", "Median")

medianCategoryTable <- formattable(dfMinQualPlotMedian)


# Technical Areas ---------------------------------------------------------

areas <- unique(df$category)

techAreas <- c("Technical Solutions" ,"Hardware Engineering" ,"Software Engineering","Data Center & Network","Technical Infrastructure", "IT & Data Management","Network Engineering")



dfMinQualTech <- dfMinQual[dfMinQual$category %in% techAreas,]
dfMinQualTech$minimumQualifications <- str_replace_all(dfMinQualTech$minimumQualifications, ",", " ")
dfMinQualTech$minimumQualifications <- str_replace_all(dfMinQualTech$minimumQualifications, "\\.", " ")

languages$name <- str_replace_all(languages$name, "\\/", "[\\\\/]")
languages$name <- str_replace_all(languages$name, regex("\\+"), "[\\\\+]")
languages$name <- str_replace_all(languages$name, "\\#", "[\\\\#]")
languages$name <- str_replace_all(languages$name, "\\!", "[\\\\!]")
languages$name <- str_replace_all(languages$name, "\\.", "[\\\\.]")
languages$name <- str_replace_all(languages$name, "\\(", "[\\\\(]")
languages$name <- str_replace_all(languages$name, "\\)", "[\\\\)]")
languages$name <- str_replace_all(languages$name, "\\-", "[\\\\-]")
languages$name <- str_replace_all(languages$name, "\\@", "[\\\\@]")


techLanguages <- paste(languages$name, collapse = "[,\\s\\)])|(\\b")

techLanguages <- paste0("(\\b",techLanguages,"[,\\s\\)])")


dfMinQualTech$languages <- str_extract_all(dfMinQualTech$minimumQualifications, regex(techLanguages, ignore_case = FALSE))


requiredLanguages <- unnest(dfMinQualTech, languages)
requiredLanguages$languages <- str_remove_all(requiredLanguages$languages, "\\)")
requiredLanguages$languages <- str_replace_all(requiredLanguages$languages, " ", "")
requiredLanguages <- requiredLanguages[-which(requiredLanguages$languages == "D"|requiredLanguages$languages == "T"),]
requiredLanguagesGraph <- requiredLanguages %>%
  select(category, languages) %>%  
  group_by(category, languages) %>% 
  add_tally() %>% 
  unique()

ggplot(requiredLanguagesGraph, aes(x = reorder(languages, -n), y = n, fill = category)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  xlab("Languages") +
  ylab("Count") +
  labs(fill = "Category") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5) ) +
  facet_wrap(~category, scales = 'free')


requiredLanguagesWordcloud <- requiredLanguages %>%
  select(languages) %>%  
  group_by(languages) %>% 
  add_tally() %>% 
  unique()
wcloud <- wordcloud2(requiredLanguagesWordcloud)
htmlwidgets::saveWidget(wcloud,paste0("languages_wcloud_.html"),selfcontained = F)



# Preferred Category ------------------------------------------------------

# Preferred Qualifications on Categories
dfPrefQual <- df[,c("company", "title", "category", "location", "preferredQualifications")]
dfPrefQual <- separate_rows(dfPrefQual,"preferredQualifications", sep = "\n")

areas <- unique(df$category)

for(i in areas){
  if(exists("eachCategoryPQ")){
    eachCategoryPQ <- list.append(eachCategoryPQ, df[df$category == i,]) 
  }else{
    eachCategoryPQ <- list(df[df$category == i,])
  }
}

names(eachCategoryPQ) <- areas

for(i in 1:length(eachCategoryPQ)){
  eachCategoryPQ[[i]]$category <- str_remove_all(eachCategoryPQ[[i]]$category, "[\\(\\)\\.\\,]")
  eachCategoryPQ[[i]]$category <- str_replace_all(eachCategoryPQ[[i]]$category, "\\/", " ")
  eachCategoryPQ[[i]]$preferredQualifications <- str_remove_all(eachCategoryPQ[[i]]$preferredQualifications, regex(words, ignore_case = TRUE))
}

View(eachCategoryPQ[[i]]$preferredQualifications)
freqCat_function <- function(x){
  freqCat <- as.data.frame(termFreq(eachCategoryPQ[[x]]$preferredQualifications))
  colnames(freqCat) <- "frequency"
  freqCat$word <- row.names(freqCat)
  freqCat$word <- str_remove_all(freqCat$word, "[\\(\\)\\.\\,]")
  freqCat$word <- str_replace_all(freqCat$word, "\\/", " ")
  return(freqCat)
}

namesCategoryPQ <- names(na.omit(eachCategoryPQ))
namesCategoryPQ <- na.omit(namesCategoryPQ)
freqCat <- lapply(namesCategoryPQ, freqCat_function)

names(freqCat) <- areas

for(i in techAreas){
  wcloud <- wordcloud2(freqCat[[i]][,c(2,1)])
  htmlwidgets::saveWidget(wcloud,paste0("wcloudPF_",i,".html"),selfcontained = F)
}

