#install and load package arules
library(arules)
library(arulesViz)
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
library(arulesViz)
library(colorspace)
library(RColorBrewer)
library(plotly)
library(shinythemes)

#read Adobe data extract into excel into R dataframe
#Monthly version
#retail <- read_excel('order_products_1xlsx.xlsx')
#Sept 2019
retail <- read_excel("order_products_sept19.xlsx")
#Delete Columns and Update Names
retail$units <- NULL
retail$segment.id <- NULL
retail$segment.name <- NULL

retail <- retail[complete.cases(retail), ]
retail %>% mutate(product = as.factor(product))

InvoiceNo <- as.numeric(as.character(retail$evar69))
#InvoiceNo2 <- as.data.frame(as.numeric(as.character(retail$evar69)))
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
tr
#Show summary of the transaction data
summary(tr)

# Simple Visualisation of quatity data
# Create an item frequency plot for the top 25 items
#Item Frequency not Unit frequency
itemFrequencyPlot(tr,topN=25,horiz=TRUE,type="absolute",col=brewer.pal(8,'Pastel2'), xlab='Units Sold',main="Product Purchases: Sept '19", ylab="")


#itemFrequencyPlot(tr,topN=25,type="relative",col=brewer.pal(8,'Pastel2'),main="Relative Item Frequency Plot")
#Generate the Apriori Rules
# Min Support as 0.001, confidence as 0.8.
association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.001,maxlen=10, minlen = 1))
summary(association.rules)
df_association.rules<- as(association.rules, "data.frame")

#Do the Magic!
ruleExplorer(association.rules)

#Stop
inspect(association.rules[1:10])
shorter.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.08,maxlen=3))
inspect(shorter.association.rules[1:10])
df_shorter.association.rules<- as(shorter.association.rules, "data.frame")
plot(shorter.association.rules, method = "graph",  engine = "htmlwidget")

#Touring Passes
touring.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.001),appearance = list(default="lhs",rhs=c("National Trust Touring Pass - Admit One 14 Days","National Trust Touring Pass - Admit One 7 Days","National Trust Touring Pass - Admit One 14 Days","National Trust Touring Pass - Admit Two 14 Days","National Trust Touring Pass - Family 14 Days","National Trust Touring Pass - Family 7 Days")))
df_touring.association.rules<- as(touring.association.rules, "data.frame")
inspect(head(touring.association.rules))
plot(touring.association.rules, method = "graph",  engine = "htmlwidget")


# Filter rules with confidence greater than 0.4 or 40%
subRules<-association.rules[quality(association.rules)$confidence>0.0001]
ruleExplorer(association.rules)

#stop

top10subRules <- tail(subRules, n = 100, by ="count")
df_top10subRules <- as(top10subRules, "data.frame")
plot(top10subRules, method = "graph",  engine = "htmlwidget")

top10subRules <- head(subRules, n = 100, by ="count")
df_top10subRules <- as(top10subRules, "data.frame")
plot(subRules, method = "graph",  engine = "htmlwidget")
ruleExplorer(subRules)
#Stop
plot(top10subRules, method = "grouped matrix",  engine = "default")

plot(top10subRules, method = "scatterplot",  engine = "htmlwidget")

plot(top10subRules, method = "two-key plot",  engine = "htmlwidget")
plot(top10subRules, method = "matrix",  engine = "htmlwidget")
#plot(top10subRules, engine = "interactive")
#plot(top10subRules, method="grouped matrix", col = grey.colors(10), gp_labels = gpar(col = "blue", cex=1, fontface="italic"))
## Reorder rules
plot(top10subRules, method="matrix", control = list(reorder = "none"))
plot(top10subRules, method="matrix", control = list(reorder = "support/confidence"))
plot(top10subRules, method="matrix", control = list(reorder = "similarity"))

## Scatterplot with custom colors
library(colorspace) # for sequential_hcl
plot(top10subRules, control = list(col=sequential_hcl(100)))
plot(top10subRules, col=sequential_hcl(100))
plot(top10subRules, col=grey.colors(50, alpha =.8))

plotly_arules(top10subRules, jitter = 10, 
              marker = list(opacity = .7, size = 10, symbol = 1), 
              colors = c("blue", "green")) 
plotly_arules(top10subRules, method = "matrix") 
plotly_arules(top10subRules)
plotly_arules(top10subRules, measure = c("support", "lift"), shading = "confidence")
plotly_arules(top10subRules, method = "two-key plot")

inspectDT(top10subRules)
ruleExplorer(top10subRules)
#saveAsGraph(top10subRules, "rules.graphml")

#top10subRules <- head(subRules, n =150, by = "count")
#plot(top10subRules, method="grouped", measure="support", control=list(col=sequential_hcl(100)))

#Plot SubRules
#plot(subRules)
#plot(subRules,method="two-key plot")
#plotly_arules(subRules)
#saveAsGraph(head(subRules, n = 1000, by = "lift"), file = "rules.graphml")
#Filter top 20 rules with highest lift
#subRules2<-head(subRules, n=20, by="lift")
#plot(subRules2, method="paracoord")


