# Set Working Directory Based on your devices
getwd()
setwd("/Users/Juliet/Documents/Monash_Uni/FIT3152/Assignment/1")

# Install Packages (If you have not have these packages)
install.packages("ggplot2")
install.packages("reshape2")
install.packages("dplyr")

# Load the Packages
library(ggplot2)
library(reshape2)
library(dplyr)

# Create individual data based on requirements
rm(list =ls())
set.seed(321112602) # XXXXXXXX = your student ID
webforum <- read.csv("webforum.csv")
webforum <- as.data.frame(webforum [sample(nrow(webforum), 20000), ]) # 20000 rows

# A1
# Create a column that has Year Month Value with Date Format
YM <- format(as.Date(webforum$Date), "%Y-%m-%d")
YM <- format(as.Date(YM), "%Y-%m")
YM <- as.Date(paste(YM,"-01",sep=""))

# Count the number of participants' activities per month
tab_month <- table(cut(YM, 'month'))
total_month <- data.frame(Date = format(as.Date(names(tab_month)), "%Y-%m"), frequency = as.vector(tab_month))

# Count the average of the number of participants' activities per year
tab_year <- table(cut(YM, 'year'))
total_year <- data.frame(Date = format(as.Date(names(tab_year)), "%Y"), frequency = as.vector(tab_year))
total_year$frequency <- total_year$frequency/12
colnames(total_year) <- c("Date","AverageThreads")
total_year

# Order total_month to see the start date that is 2002-01
total_month[order(total_month$Date),]

# Plot time series graph
series <- ts(total_month$frequency, frequency = 12, start = c(2002))
plot(series, xaxt = "n", main = "Activity of Participants' Over The Months From 2002 - 2011", xlab = "", ylab = "Frequency of Thread")
tsp <- attributes(series)$tsp
dates <- seq(as.Date("2002-01-01"), by = "month", along = series)
axis(1, at = seq(tsp[1], tsp[2], along = series), labels = format(dates, "%Y-%m"),las = 2)
mtext("Date", side=1, line= 4.1)
# Plot time series decomposition graph
decomposed_series <- decompose(series)
plot(decomposed_series)

#A2
# Average Per Month for WC, Analytic, Clout, Authentic, Tone
avg_wc_summary <- aggregate(cbind(webforum$WC, webforum$Analytic, webforum$Clout, webforum$Authentic, webforum$Tone), list(format(as.Date(webforum$Date), "%Y-%m")), mean)
colnames(avg_wc_summary) <- c("Date","WC", "Analytic","Clout","Authentic","Tone")

# Average Per Month for ppron, i, we, you, shehe, they
avg_pron <- aggregate(cbind(webforum$ppron, webforum$i, webforum$we, webforum$you, webforum$shehe, webforum$they) , list(format(as.Date(webforum$Date), "%Y-%m")), mean)
colnames(avg_pron) <- c("Date","ppron","i","we","you","shehe","they")

# Average Per Month for posemo, negemo, anx, anger, sad
avg_emo <- aggregate(cbind(webforum$posemo, webforum$negemo, webforum$anx, webforum$anger, webforum$sad) , list(format(as.Date(webforum$Date), "%Y-%m")), mean)
colnames(avg_emo) <- c("Date","posemo","negemo","anx","anger","sad")

# Average Per Month for focuspast, focuspresent, focusfuture
avg_focus <- aggregate(cbind(webforum$focuspast, webforum$focuspresent, webforum$focusfuture) , list(format(as.Date(webforum$Date), "%Y-%m")), mean)
colnames(avg_focus) <- c("Date","focuspast","focuspresent","focusfuture")

# Average Per Month for Tone, posemo, negemo
avg_tone <- aggregate(cbind((webforum$Tone/10), webforum$posemo, webforum$negemo) , list(format(as.Date(webforum$Date), "%Y-%m")), mean)
colnames(avg_tone) <- c("Date","Tone","posemo","negemo")

# Average Per Month for All Linguistic Variables
avg_all <- aggregate(cbind(webforum$WC,webforum$Analytic, webforum$Clout, webforum$Authentic, webforum$Tone,webforum$ppron, webforum$i, webforum$we, webforum$you, webforum$shehe, webforum$they,webforum$posemo, webforum$negemo, webforum$anx, webforum$anger, webforum$sad,webforum$focuspast, webforum$focuspresent, webforum$focusfuture) , list(format(as.Date(webforum$Date), "%Y-%m")), mean)
colnames(avg_all) <- c("Date","WC","Analytic","Clout","Authentic","Tone","ppron","i","we","you","shehe","they","posemo","negemo","anx","anger","sad","focuspast","focuspresent","focusfuture")

# Graph for Levels of WC, Analytic, Clout, Authentic, Tone Per Month
melt_wc_summary <- melt(avg_wc_summary,id ="Date")
wc_summary <- ggplot(melt_wc_summary,aes(x = Date,y = value,colour = variable, group = variable)) + geom_line()
wc_summary <- wc_summary + labs(title = "Levels of WC, Analytic, Clout, Authentic, Tone Per Month ", x = "Date", y = "Frequency", fill = guide_legend(title="Linguistic Variables"))
wc_summary <- wc_summary + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90), legend.position = c(0.5, 0.98), legend.direction="horizontal")
wc_summary <- wc_summary +  guides(color=guide_legend(title="Linguistic Variables")) 
wc_summary                               

# Graph for Levels of ppron, i, we, you, shehe, they
melt_pron <- melt(avg_pron,id ="Date")
pron <- ggplot(melt_pron,aes(x = Date,y = value,colour = variable, group = variable)) + geom_line()
pron <- pron + labs(title = "Levels of ppron, i, we, you, shehe, they Per Month ", x = "Date", y = "Frequency", fill = guide_legend(title="Linguistic Variables"))
pron <- pron + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90), legend.position = c(0.5, 0.98), legend.direction="horizontal")
pron <- pron +  guides(color=guide_legend(title="Linguistic Variables", nrow = 1)) 
pron 

# Graph for Levels of posemo, negemo, anx, anger, sad
melt_emo <- melt(avg_emo,id ="Date")
emo <- ggplot(melt_emo,aes(x = Date,y = value,colour = variable, group = variable)) + geom_line()
emo <- emo + labs(title = "Levels of posemo, negemo, anx, anger, sad Per Month ", x = "Date", y = "Frequency", fill = guide_legend(title="Linguistic Variables"))
emo <- emo + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90), legend.position = c(0.5, 0.98), legend.direction="horizontal")
emo <- emo +  guides(color=guide_legend(title="Linguistic Variables", nrow = 1)) 
emo 

# Graph for Levels of focuspast, focuspresent, focusfuture
melt_focus <- melt(avg_focus,id ="Date")
focus <- ggplot(melt_focus,aes(x = Date,y = value,colour = variable, group = variable)) + geom_line()
focus <- focus + labs(title = "Levels of focuspast, focuspresent, focusfuture Per Month ", x = "Date", y = "Frequency", fill = guide_legend(title="Linguistic Variables"))
focus <- focus + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90), legend.position = c(0.5, 0.98), legend.direction="horizontal")
focus <- focus +  guides(color=guide_legend(title="Linguistic Variables", nrow = 1)) 
focus 

# Graph for Levels of Tone, posemo, negemo
melt_tone <- melt(avg_tone,id ="Date")
tone <- ggplot(melt_tone,aes(x = Date,y = value,colour = variable, group = variable)) + geom_line()
tone <- tone + labs(title = "Levels of Tone, posemo, negemo Per Month ", x = "Date", y = "Frequency", fill = guide_legend(title="Linguistic Variables"))
tone <- tone + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90), legend.position = c(0.5, 0.98), legend.direction="horizontal")
tone <- tone +  guides(color=guide_legend(title="Linguistic Variables", nrow = 1)) 
tone 

# Relationship Between Linguistic Variables
# Relationship of Ppron with Other Linguistic Variables
ppron <- lm(ppron ~ .-Date, data = avg_all)
summary(ppron)

# Relationship of Negemo with Other Linguistic Variables
negemo <- lm(negemo ~ . -Date, data = avg_all)
summary(negemo)

# Relationship of Tone with Other Linguistic Variables
tone <- lm(Tone ~ . -Date, data = avg_all)
summary(tone)

# Relationship of Analytic with Other Linguistic Variables
analytic <- lm(Analytic ~ . -Date, data = avg_all)
summary(analytic)

# B
# Analyse Top 10 Most Thread In The Forum
# Get the total number of each thread in the forum
unique_threadID <- as.data.frame(tally(group_by(webforum, ThreadID)))
colnames(unique_threadID) <- c("ThreadID", "Count")

# Select top 10 ThreadID that have the most number of threads
unique_threadID <- unique_threadID[order(unique_threadID$Count, decreasing  = TRUE),]
top10_threadID <- unique_threadID[1][1:10,]
top10_threadID <- webforum[webforum$ThreadID  %in% top10_threadID,]

# To Compare the Happiness of Each Thread, use posemo as our Independent Variable 
posemo <- lm(posemo ~ . -Date-Time-ThreadID-AuthorID, data = top10_threadID)
summary(posemo)

# Take only ThreadID, posemo, negemo, Tone columns
top10_threadID <- top10_threadID[, c('ThreadID', 'posemo','negemo','Tone')]

# Create a Box Plot for posemo, Tone, negemo
op <- par(mfrow=c(1, 3))  ## set par
boxplot(top10_threadID$posemo~top10_threadID$ThreadID,las = 2, xlab = "", ylab = "posemo's Frequency", col = c("red","orange","light yellow","green","light blue","violet","purple","pink","light green","yellow"))
title(xlab = "ThreadID", line = 4)
boxplot(top10_threadID$Tone~top10_threadID$ThreadID,las = 2, main = "Comparison of Happiness Linguistic Variables Between ThreadIDs",xlab = "", ylab = "Tone's Frequency", col = c("red","orange","light yellow","green","light blue","violet","purple","pink","light green","yellow"))
title(xlab = "ThreadID", line = 4)
boxplot(top10_threadID$negemo~top10_threadID$ThreadID,las = 2, xlab = "", ylab = "negemo's Frequency", col = c("red","orange","light yellow","green","light blue","violet","purple","pink","light green","yellow"))
title(xlab = "ThreadID", line = 4)
par(op)

## Statistical Test
# posemo
t.test(top10_threadID[2][top10_threadID$ThreadID == 472752,], top10_threadID[2][top10_threadID$ThreadID != 472752,], "greater", conf.level = 0.99)
p1 <- 0.0003868
t.test(top10_threadID[2][top10_threadID$ThreadID == 773564,], top10_threadID[2][top10_threadID$ThreadID != 773564,], "greater", conf.level = 0.99)
p2 <- 0.01437

# negemo
t.test(top10_threadID[3][top10_threadID$ThreadID == 472752,], top10_threadID[3][top10_threadID$ThreadID != 472752,], "less", conf.level = 0.99)
n1 <- 0.01739
t.test(top10_threadID[3][top10_threadID$ThreadID == 773564,], top10_threadID[3][top10_threadID$ThreadID != 773564,], "less", conf.level = 0.99)
n2 <- 0.01162

# Tone
t.test(top10_threadID[4][top10_threadID$ThreadID == 472752,], top10_threadID[4][top10_threadID$ThreadID != 472752,], "greater", conf.level = 0.99)
t1 <- 5.479e-09
t.test(top10_threadID[4][top10_threadID$ThreadID == 773564,], top10_threadID[4][top10_threadID$ThreadID != 773564,], "greater", conf.level = 0.99)
t2 <- 3.015e-06

# Create data frame to compare the p-value for 472752 and 773564 ThreadID for the observed linguistic variable
compare <- data.frame(ThreadID = c(472752, 773564), posemo = c(p1,p2), negemo = c(n1,n2), Tone = c(t1,t2))
compare

# C1
# Install Packages
install.packages("igraph")
install.packages("igraphdata")

# Load Packages
library(igraph)
library(igraphdata)

# Choose over a month (2002-03) online activity
social_network <- webforum[webforum$Date >= as.Date("2002-03-01") & webforum$Date <= as.Date("2002-03-31"),]
# Check whether it has more than 30 authors
length(unique(social_network$AuthorID))
# Check if 1 author posted more than one thread
social_network[order(social_network$AuthorID),]

# Take only ThreadID and AuthorID columns
thread_author <- as.data.frame(social_network[,1:2])
colnames(thread_author) <- c("ThreadID","AuthorID")

# Create a separate data frame to store distinct author IDs
unique_authorID <- as.data.frame(unique(thread_author$AuthorID))
colnames(unique_authorID) <- "AuthorID"

# Make an empty graph
g <- make_empty_graph(directed = FALSE)

# Add vertices using “for loop”
for (i in 1 : nrow(unique_authorID)) {
  g <- add_vertices(g, 1, name = as.character(unique_authorID$AuthorID[i]))
}

# Loop through each Thread
for (k in unique(thread_author$ThreadID)){
temp = thread_author[(thread_author$ThreadID == k),]
 # Combine related Author IDs to make an edge list
if (nrow(temp) > 1) {
Edgelist = as.data.frame(t(combn(temp$AuthorID,2))) 
colnames(Edgelist) = c("P1","P2") 
for (i in 1:nrow(Edgelist)){ 
  g <- add_edges(g,c(as.character(Edgelist$P1[i]),as.character(Edgelist$P2[i])))
}}}

# Plot the graph
plot(g)

# Because there are some loop in the graph, we need to simplify the graph
g <- simplify(g)

# Plot the graph once more. It is noticeable that the size of the circle covered the other author IDs 
plot(g)

# Resize the size of the circle depending on the importance of each author ID
deg <- degree(g)
V(g)$size <- deg/1.7

# Plot the final social network graph
plot(g, main = "Authors' Social Network in March 2002")

# C2
# Which authors are the most important across the network graph?
order(closeness(g), decreasing = TRUE)
order(betweenness(g), decreasing = TRUE)
order(degree(g), decreasing = TRUE)
order(evcent(g)$vector, decreasing = TRUE)

# Most important Author for the graph is the one who has vertices 1 with 1740 as his/her AuthorID
V(g)[1]

# Observed the language the most important authors than the others.
# Take the AuthorID and all of the other linguistic variables and save it as a data frame of comparison
comparison <- as.data.frame(social_network[, c(2,5:23)])

# Analyze LIWC Variables that makes 1740 the most important author
# Statistical t-test
# they
t.test(comparison[12][comparison$AuthorID == 1740,], comparison[12][comparison$AuthorID != 1740,], "less", conf.level = 0.99)
# negemo
t.test(comparison[14][comparison$AuthorID == 1740,], comparison[14][comparison$AuthorID != 1740,], "less", conf.level = 0.99)
# anx
t.test(comparison[15][comparison$AuthorID == 1740,], comparison[15][comparison$AuthorID != 1740,], "less", conf.level = 0.99)