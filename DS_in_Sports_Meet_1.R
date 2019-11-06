library(rvest)  # Should be installed along with 'xml2'
library(plotrix) # Also install it please.

url_link <- 'https://www.sports-reference.com/cfb/schools/houston/2017/gamelog/'

# Read the HTML webpage for that link into an R object
url <- read_html(url_link) 

# Find a 'table' on that HTML page
table_one <- xml_find_all(url, "//table") 
class(table_one)

# Obtain the actual contents of the table, wrapped into a data frame
table_one <- html_table(table_one)
class(table_one)

table_one
length(table_one)

table_one[[1]]
class(table_one[[1]])

# Looks promising, but there's one big issue - there were two header rows in the table,
# and only the first one got accounted for. Hence the second header row ('Rk', 'Date', 'Opponent' etc)
# is considered as the first OBSERVATION rather than COLUMN NAMES.

# That can be fixed via hard-coding:
tab.col.names <- table_one[[1]][1,]
table_one[[1]] <- table_one[[1]][-1,]
colnames(table_one[[1]]) <- tab.col.names


# Also, we don't quite need the LAST ROW, which is just totals for the year:
# we can always calculate those on our own.
table_one[[1]] <- table_one[[1]][-nrow(table_one[[1]]),]


# Let's now take some variable summaries, e.g. average no. of completions:
mean(table_one[[1]]$Cmp)

# What's wrong? Check the types of variables in data frame resulting from read_html operation:
summary(table_one[[1]])

# All are automatically treated as characters. We'd like to convert most of those (but NOT ALL) to numeric.
# In particular, everything except Date, Home/Away marker, Opponent and Result.
head(table_one[[1]])

# Function as numeric comes to rescue:
table_one[[1]][,-c(2:5)] <- lapply(table_one[[1]][,-c(2:5)],as.numeric)

# Now verify that everything is in order:
head(table_one[[1]])
summary(table_one[[1]])


# Get pairwise scatterplots for different variables
pairs(table_one[[1]]) # Error - get rid of non-numerical stuff

pairs(table_one[[1]][,-c(1:5)])
# A bit problematic to read, isn't it?

# Let's just look at subsets of variables
pairs(table_one[[1]][,c(6:10)])
pairs(table_one[[1]][,c(11:19)])

# Honestly, there's not too much you can ever see from 12 data points.





#################################
### GETTING DATA ON MORE TEAMS ##
#################################


# Let's get tables on several teams, to obtain a larger sample of games.
# For that, first we need to specify the url-versions of team names we're interested in:
team_names <- c('houston', 'rice', 'texas-tech','temple','tulsa')

# Next, we need to code up a method to automatically creat a full URL for each team name.
# It is done via 'paste()' function.

# E.g., how can we get the name:
#  'https://www.sports-reference.com/cfb/schools/houston/2017/gamelog/'
# via paste() function and the team name 'houston'?

url_link <- paste('https://www.sports-reference.com/cfb/schools/',team_names[1],'/2017/gamelog/')
url_link

# Issue here? There are white spaces at string connection points.
# Use 'sep=' option to get rid of those.

url_link <- paste('https://www.sports-reference.com/cfb/schools/',team_names[1],'/2017/gamelog/',
                  sep="")
url_link

# And we got it from here - now we could simply apply all of the aforementioned code
# to this calculated url.
# Now, how do we automatically loop through all the team names? Easy:
#  Loop through indices (i=1,...,5), where for each i we:
#       Calculate the url for team_names[i]
#       Extract the table.


# We'll keep all the tables in one big data frame object,
# and also REPLACE THE USELESS FIRST 'Rk' COLUMN for TEAM NAME column A
offense_data <- NULL
team_names <- c('houston', 'rice', 'texas-tech','temple','tulsa')

for (i in 1:length(team_names)){
  url_link <- paste('https://www.sports-reference.com/cfb/schools/',team_names[i],'/2017/gamelog/',
                    sep="") 
  url <- read_html(url_link) 
  table_one <- xml_find_all(url, "//table")
  table_one <- html_table(table_one)[[1]]
  
  tab.col.names <- table_one[1,]
  table_one <- table_one[-1,]
  colnames(table_one) <- tab.col.names
  
  table_one <- table_one[-nrow(table_one),]
  table_one[,-c(2:5)] <- lapply(table_one[,-c(2:5)],as.numeric)
  table_one[,1] <- team_names[i]
  colnames(table_one)[1] <- "Team"
  
  print(head(table_one,2)) # Print out extracted table for the team.
  cat("\n")
  offense_data <- rbind(offense_data, table_one)
}


dim(offense_data)
head(offense_data)

# Get pairwise scatterplots for different variables
# Let's just look at subsets of variables
pairs(offense_data[,c(6:10)])
pairs(offense_data[,c(11:19)])


## Get correlation matrix.
offense_data_num <- offense_data[,-c(1:5)]
cor(offense_data_num)  # Tough to read.

## Try plotting?
library(plotrix)
color2D.matplot(cor(offense_data_num),
                cs1=c(1,0),cs2=c(1,0),cs3=c(1,0),
                show.legend=T)

# We are not really interested in SIGN of correlation (whether it's positive or negative),
# but more in just the STRENGTH of correlation. Apply 'abs()' function.

color2D.matplot(abs(cor(offense_data_num)),
                cs1=c(1,0),cs2=c(1,0),cs3=c(1,0),
                show.legend=T)

# Cleaner, but still a bit dirty. We could THRESHOLD values, so that
# all correlations less than the thresholds are SET TO 0.
# This may be done via 'ifelse()'

abs.cor.mat <- abs(cor(offense_data_num))
color2D.matplot(ifelse(abs.cor.mat>=0.8, abs.cor.mat,0),
                cs1=c(1,0),cs2=c(1,0),cs3=c(1,0),
                show.legend=T)

colnames(abs.cor.mat)

## To pretty up the plot and make it much more informative, we could take
## numerous steps, e.g. making the variable names show up along rows and columns.

color2D.matplot(ifelse(abs.cor.mat>=0.8, abs.cor.mat,0),
                cs1=c(1,0),cs2=c(1,0),cs3=c(1,0),
                show.legend = T,
                xlab='',
                ylab='',
                axes=F)
par(las=2)
axis(1,at=c(1:ncol(abs.cor.mat))-0.5,labels=colnames(abs.cor.mat))
par(las=1)
axis(2,at=c(ncol(abs.cor.mat):1)-0.5,labels=colnames(abs.cor.mat))






## 
thresholded.corr <- function(data,thr=0.8){
  cor.mat <- abs(cor(data))
  return(ifelse(cor.mat > thr,cor.mat,0))
}



no.TDs <- subset(Full_Team_data_numeric, select = -c(TD,TD.1))

color2D.matplot(thresholded.corr(no.TDs),
                cs1=c(1,0),cs2=c(1,0),cs3=c(1,0),
                show.legend = T,
                xlab='',
                ylab='',
                axes=F)
par(las=2)
axis(1,at=c(1:ncol(no.TDs))-0.5,labels=colnames(no.TDs))
par(las=1)
axis(2,at=c(ncol(no.TDs):1)-0.5,labels=colnames(no.TDs))



################################################
################################################



## From "INTRO TO STAT LEARNING" on COLLINEARITY

# 6. Collinearity.
# 
# Collinearity refers to the situation in which two or more predictor variables collinearity
# are closely related to one another.
# 
# CREDIT CARD DATA EXAMPLE... In other words, since limit and rating tend to increase or decrease together, it can be difficult to determine how each one separately is associated with the response, balance .
# 
# !!!!!! PAGE 100 - GREAT GRAPHIC DEMONSTRATING COLLINEARITY ISSUES !!!!!!
#   
#   
#   Since collinearity reduces the accuracy of the estimates of the regression coefficients, it causes the standard error for β̂ j to grow. 
# 
# Recall that the t-statistic for each predictor is calculated by dividing β̂ j by its standard error => 
#   
#   collinearity results in a decline in the t-statistic =>
#   
#   in the presence of collinearity, we may fail to reject H 0 : β j = 0. 
# 
# This means that the power of the hypothesis test—the probability of correctly
# detecting a non-zero coefficient—is reduced by collinearity.
# 
# Ways to DETECT COLLINEARITY:
#   - A simple way to detect collinearity is to look at the correlation matrix
# of the predictors.
# - Unfortunately, not all collinearity problems can be detected by inspection of the correlation matrix: it is possible for collinearity to exist between three or more variables even if no pair of variables has a particularly high correlation. We call this situation multicollinearity.
# Instead of inspecting the correlation matrix, a better way to assess multi-
#   collinearity is to compute the variance inflation factor (VIF).
# As a rule of thumb, a VIF value that exceeds 5 or 10 indicates a problematic amount of collinearity.
# 
# DEALING with COLLINEARITY:
#   - The first is to drop one of the problematic variables from the regression.
# - The second solution is to combine the collinear variables together into a single predictor. For instance, we might take the average of standardized versions of limit and rating in order to create a new variable that measures credit worthiness.







### Some crazy fancy solution
# https://stackoverflow.com/questions/43476819/not-able-to-scrape-a-second-table-within-a-page-using-rvest






# Unfortunately, the second table on that webpage won't come as easy:
#  It is hidden under '//comment' tag.

find_add_tables <- function(url){
  x <- xml2::xml_find_all(read_html(url),"//comment()") %>% {
    #Find only commented nodes that contain the regex for html table markup
    raw_parts <- as.character(.[grep("\\</?table", as.character(.))])
    # Remove the comment begin and end tags
    strip_html <- stringi::stri_replace_all_regex(raw_parts, c("<\\!--","-->"),c("",""),
                                                  vectorize_all = FALSE)
    # Loop through the pieces that have tables within markup and 
    # apply the same functions
    lapply(grep("<table", strip_html, value = TRUE), function(i){
      rvest::html_table(xml_find_all(read_html(i), "//table")) %>% 
        .[[1]]
    })
  }
  return(x)
}


# Additional tables are within the comment tags, ie <!-- tables -->
# Which is why your xpath is missing them.
# First get the commented nodes
alt_tables <- xml2::xml_find_all(urlbbref,"//comment()") %>% {
  #Find only commented nodes that contain the regex for html table markup
  raw_parts <- as.character(.[grep("\\</?table", as.character(.))])
  # Remove the comment begin and end tags
  strip_html <- stringi::stri_replace_all_regex(raw_parts, c("<\\!--","-->"),c("",""),
                                                vectorize_all = FALSE)
  # Loop through the pieces that have tables within markup and 
  # apply the same functions
  lapply(grep("<table", strip_html, value = TRUE), function(i){
    rvest::html_table(xml_find_all(read_html(i), "//table")) %>% 
      .[[1]]
  })
}
# Put all the data frames into a list.
all_tables <- c(
  table_one, alt_tables
)