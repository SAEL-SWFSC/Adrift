#Merge Title Page and Adrift Report into single, Final Document
#StoreDocument in docs folder

#install.packages("qpdf)
# ?qpdf::pdf_combine  #check documentation
library(here)
titlePage <- here("titlePage", "Title_Pages.pdf")
endPage <- here("titlePage", "endPage.pdf")
report <- here("docs", "ADRIFTReport.pdf")
output <- here("docs", "AdriftReport_Complete.pdf")

qpdf::pdf_combine(input = c(titlePage, report, endPage),
                  output = output)
