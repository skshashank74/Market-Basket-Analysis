# to find the best selling combination of product through Market Basket Analysis

#Load the libraries
require(arules)
require(arulesViz)

install.packages("arulesViz")

#upload the data
US_data <- read.csv("US_data_product_1jan_31july.csv")

#check the data
head(US_data)

#split the data in sparse matrix form
i <- split(US_data$Product,US_data$Transaction_id)

#check the data
head(i)
summary(i)

#convert the data in transaction format
library(arules)
txid <- as(i, "transactions")

#check the data
inspect(txid[1:100])

#support of txid
support_txid <- itemFrequency(txid)

#download data in excel
write.csv(support_txid, file = 'support12.csv')

#graph plot for support
itemFrequencyPlot(txid, topN =20)

#apriori algorithm for two length 
MBA <- apriori(txid, parameter = list(support = 0.001, confidence = 0.1, maxlen = 2))

#check the data and inspect in content
summary(MBA)
inspect(MBA)
inspect(sort(MBA, by = "lift", decreasing = TRUE)[1:10])

#download the data in excel
MBA1 <- as(MBA, "data.frame")
write.csv(MBA1, file = 'Market_analysis.csv')

#graph lot for support confidence lift
{% highlight r %}
library("arulesViz")
plot(MBA)
{% endhighlight %}


# find redundant rules
subset.matrix <- is.subset(MBA, MBA)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)

# remove redundant rules
MBA2 <- MBA[!redundant]
inspect(MBA2)
summary(MBA2)

write.csv(redundant, file = 'redundant.csv')
------
  
## Create different visualizations
plot(MBA, method="graph", main="", control=list(layout=igraph::with_graphopt()))
plot(MBA, method="paracoord")
plot(MBA, method="grouped")
## plot(rules,method="graph",interactive=TRUE,shading=NA) 
##In plot.rules(rules, method = "graph", interactive = TRUE, shading = NA) :
##The parameter interactive is deprecated. Use engine='interactive' instead.
plot(MBA,method="graph",engine='interactive',shading=NA)