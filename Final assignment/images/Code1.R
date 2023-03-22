install.packages("tesseract")
library("pdftools")
library(quanteda)
library(tesseract)
tesseract_download("chi_sim") # just run once
require(readtext)

# Convert 2nd Archieve Collection 
setwd("~/Desktop/QTA_Spring23/Final assignment")
image2 <- pdftools::pdf_convert('~/Desktop/QTA_Spring23/Final assignment/中华民国史档案资料汇编 第5辑第2编 军事2.pdf', dpi = 600)

setwd("~/Desktop/QTA_Spring23/Final assignment")
text2 <- tesseract::ocr(image2,engine = "chi_sim") # convert images to text using OCR

#text2.1 <- tesseract::ocr(image2) # convert images to text using OCR


cat(text2[3]) # display text from the 3rd page of the PDF

final_text2 <- paste(text2, collapse = " ")
setwd("~/Desktop/QTA_Spring23/Final assignment")
output_file2 <- file("ROC_WWII_text2.txt")
writeLines(final_text2, output_file2)
close(output_file2)

# Convert 3rd Archive Collection 
setwd("~/Desktop/QTA_Spring23/Final assignment")
image3 <- pdftools::pdf_convert('~/Desktop/QTA_Spring23/Final assignment/中华民国史档案资料汇编 第5辑第2编 军事3.pdf', dpi = 600)

setwd("~/Desktop/QTA_Spring23/Final assignment")
text3 <- tesseract::ocr(image3,engine = "chi_sim") # convert images to text using OCR


cat(text3[3]) # display text from the 3rd page of the PDF

final_text3 <- paste(text3, collapse = " ")
setwd("~/Desktop/QTA_Spring23/Final assignment")
output_file3 <- file("ROC_WWII_text3.txt")
writeLines(final_text3, output_file3)
close(output_file3)


# Convert 4th Archieve Collection 
setwd("~/Desktop/QTA_Spring23/Final assignment")
image4 <- pdftools::pdf_convert('~/Desktop/QTA_Spring23/Final assignment/中华民国史档案资料汇编 第5辑第2编 军事4.pdf', dpi = 600)
save.image()

setwd("~/Desktop/QTA_Spring23/Final assignment")
text4 <- tesseract::ocr(image4,engine = "chi_sim") # convert images to text using OCR


cat(text4[3]) # display text from the 3rd page of the PDF

final_text4 <- paste(text4, collapse = " ")
setwd("~/Desktop/QTA_Spring23/Final assignment")
output_file4 <- file("ROC_WWII_text4.txt")
writeLines(final_text4, output_file4)
close(output_file4)
writeLines(final_text4, output_file4)
close(output_file4)


txt <- rbind(output_file1, output_file2, output_file3, output_file4)

save(txt, file = "ROC_WWII_text.csv")


