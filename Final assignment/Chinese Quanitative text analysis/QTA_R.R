library("pdftools")
library(quanteda)
library(tesseract)
#tesseract_download("chi_sim")
require(readtext)
library(topicmodels) # LDA
library(stm) # STM
library(ldatuning) # data-driven k selection
library(ggplot2)
library(stm)
#install.packages("igraph")
library(igraph) 

setwd("E:/QTA_Project")
text47 <- tesseract::ocr("E:/QTA_Project/中华民国史档案资料汇编 第5辑第2编 军事1_47.png", engine = "chi_sim")
cat(text)

final_text47 <- paste(text47, collapse = " ")
setwd("E:/QTA_Project")
output_file47 <- file("ROC_WWII_text47.txt")
writeLines(final_text47, output_file47)
close(output_file47)

setwd("E:/QTA_Project")
text46 <- tesseract::ocr("E:/QTA_Project/中华民国史档案资料汇编 第5辑第2编 军事1_46.png", engine = "chi_sim")
cat(text)

final_text46 <- paste(text46, collapse = " ")
setwd("E:/QTA_Project")
output_file46 <- file("ROC_WWII_text46.txt")
writeLines(final_text46, output_file46)
close(output_file46)

txt46 <- readtext("E:/QTA_Project/ROC_WWII_text46.txt")
txt47 <-readtext("E:/QTA_Project/ROC_WWII_text47.txt")

txt467 <- rbind(txt46, txt47)




### Covert 1st Archieve Collection

setwd("E:/QTA_Project")
images1 <- pdftools::pdf_convert('Data/中华民国史档案资料汇编 第5辑第2编 军事1.pdf', dpi = 600)

setwd("E:/QTA_Project")
text1 <- tesseract::ocr(images1, engine = "chi_sim") # convert images to text using OCR

cat(text1[3]) # display text from the 3rd page of the PDF

final_text1 <- paste(text1, collapse = " ")
setwd("E:/QTA_Project")
output_file1 <- file("ROC_WWII_text1.txt")
writeLines(final_text1, output_file1)
close(output_file1)



# Convert 2nd Archieve Collection 
setwd("E:/QTA_Project")
images2 <- pdftools::pdf_convert('Data/中华民国史档案资料汇编 第5辑第2编 军事2.pdf', dpi = 600)

setwd("E:/QTA_Project")
text2 <- tesseract::ocr(images2, engine = "chi_sim") # convert images to text using OCR


cat(text2[3]) # display text from the 3rd page of the PDF

final_text2 <- paste(text2, collapse = " ")
setwd("E:/QTA_Project")
output_file2 <- file("ROC_WWII_text2.txt")
writeLines(final_text2, output_file2)
close(output_file2)

# Convert 3rd Archive Collection 
setwd("E:/QTA_Project")
image3 <- pdftools::pdf_convert('Data/中华民国史档案资料汇编 第5辑第2编 军事3.pdf', dpi = 600)

setwd("E:/QTA_Project")
text3 <- tesseract::ocr(images3, engine = "chi_sim") # convert images to text using OCR


cat(text3[3]) # display text from the 3rd page of the PDF

final_text3 <- paste(text3, collapse = " ")
setwd("E:/QTA_Project")
output_file3 <- file("ROC_WWII_text3.txt")
writeLines(final_text3, output_file3)
close(output_file3)


# Convert 4th Archieve Collection 
setwd("E:/QTA_Project")
image4 <- pdftools::pdf_convert('Data/中华民国史档案资料汇编 第5辑第2编 军事4.pdf')
save.image()

setwd("/Volumes/One Touch/QTA_Project")
text4 <- tesseract::ocr(image4, engine = "chi_sim") # convert images to text using OCR


cat(text4[3]) # display text from the 3rd page of the PDF

final_text4 <- paste(text4, collapse = " ")
setwd("/Volumes/One Touch/QTA_Project")
output_file4 <- file("ROC_WWII_text4.txt")
writeLines(final_text4, output_file4)
close(output_file4)

output_file1 <- readtext("/Volumes/One Touch/QTA_Project/ROC_WWII_text1.txt")
output_file2 <- readtext("/Volumes/One Touch/QTA_Project/ROC_WWII_text2.txt")
output_file3 <- readtext("/Volumes/One Touch/QTA_Project/ROC_WWII_text3.txt")
output_file4 <- readtext("/Volumes/One Touch/QTA_Project/ROC_WWII_text4.txt")

txt <- rbind(output_file1, output_file2, output_file3, output_file4)

final_text <- paste(txt, collapse = " ")
setwd("/Volumes/One Touch/QTA_Project")
output_file <- file("ROC_WWII_text.txt")
writeLines(final_text, output_file)
close(output_file)
# save the binded files as txt file

save(txt, file = "ROC_WWII_text.csv")
# save the binded files as csv file

read.csv("/Volumes/One Touch/QTA_Project/ROC_WWII_text.csv")

corp <- corpus(txt)

# Chinese stopwords
ch_stop <- stopwords("zh", source = "misc")

# tokenize
ch_toks_pre<- corp %>% 
  tokens(remove_punct = TRUE) %>%
  tokens_remove(pattern = ch_stop)

# collacation 
library("quanteda.textstats")
ch_col <- textstat_collocations(ch_toks_pre, size = 2, min_count = 20)
knitr::kable(head(ch_col, 10))

ch_toks <- tokens_compound(ch_toks_pre, pattern = ch_col[ch_col$z > 10,])

# construct a dfm
ch_dfm <- dfm(ch_toks)
topfeatures(ch_dfm)


stmdfm <- convert(ch_dfm, to = "stm") # convert quanteda dfm to stm format (helps with memory)

K =35

modelFit <- stm(documents = stmdfm$documents,
                vocab = stmdfm$vocab,
                K = K,
                data = stmdfm$meta,
                max.em.its = 50000,
                init.type = "Spectral",
                seed = 2023,
                verbose = TRUE)

saveRDS(modelFit, "data/modelFit5") #This is the third model
print(modelFit)
labelTopics(modelFit)


modelFit <- readRDS("data/modelFit5") 


plot.STM(modelFit, 
         type = "summary", 
         labeltype = "prob",
         text.cex = 0.7,
         main = "Topic prevalence and top terms")

## first optimalisation, 200 interations, remove "said","say","says", "also"
## second optimalisation, 200 interations, remove "according", "going"

## fifth, plus prefevlence, month 





## Topic Correlation:

topic_correlations <- topicCorr(modelFit)
plot.topicCorr(topic_correlations,
               vlabels = seq(1:ncol(modelFit$theta)),
               vertex.color = "white",
               main = "Topic correlations")


## Topic Quality (Semantic coherence and exclusivity)

topicQuality(model = modelFit,
             documents = stmdfm$documents,
             xlab = "Semantic Coherence",
             ylab = "Exclusivity",
             labels = 1:ncol(modelFit$theta),
             M = 15)

