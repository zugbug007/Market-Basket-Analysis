#install and load package arules
library(arules)
library(arulesViz)
library(readxl)
library(plyr)
library(dplyr)
library(arulesViz)
library(colorspace)
library(RColorBrewer)
library(shinythemes)

#October 2019
retail <- read_excel("order_products_oct19.xlsx")
#Delete Columns and Update Names
retail$units <- NULL
retail$segment.id <- NULL
retail$segment.name <- NULL

retail <- retail[complete.cases(retail), ]
retail %>% mutate(product = as.factor(product))

InvoiceNo <- as.numeric(as.character(retail$evar69))
cbind(retail,InvoiceNo)
glimpse(retail)

transactionData <- ddply(retail,c("InvoiceNo"),function(df1)paste(df1$product,collapse = ","))
#Remove unneeded columns
transactionData$InvoiceNo <- NULL

#Rename column to items
colnames(transactionData) <- c("items")

#Check format
write.csv(transactionData,"market_basket_transactions.csv", quote = FALSE, row.names = FALSE)

#Read in the transaction data into basket format
tr <- read.transactions('market_basket_transactions.csv', format = 'basket', sep=',')
#Check transactions
df_tr <- as(tr, "data.frame")
#Show summary of the transaction data
#summary(tr)

#Generate the Apriori Rules
# Min Support as 0.001, confidence as 0.001.
association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.001,maxlen=10, minlen = 1))
summary(association.rules)
df_association.rules<- as(association.rules, "data.frame")

#Do the Magic, Abracadabra!
ruleExplorer(association.rules)
