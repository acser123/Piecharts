library (readxl)
library (ggplot2)
library (dplyr)


# Open PDF file for writing
pdf("c:/Users/acser/Documents/R/2022Q1_CustomerAuthenticationSurvey/2022StateOfCustomerAuthentication.pdf", paper="a4")

# Set chart font sizes
#set(findobj(h,'type','text'),'fontsize',10)


# read the XLS file into an R frame
authn <- read_excel("c:/Users/acser/Documents/R/2022Q1_CustomerAuthenticationSurvey/Customer Authentication 2022_final.xlsx")
dt <- data.frame(authn)
# get all column names

cols <- colnames(dt)
#cols

# Apply processing in function(i)to all columns of the authn table
lapply(colnames(authn)[1:ncol (authn)], function(i){
 # res <- authn[, c("Progress", i)]

# Load the i th column into res
res <- authn[, c(i)]

# print i, the column header (title)
#print (i)


# get factors from the column selected
#f <- factor(authn$Progress) 

f <- res

t <- table(f) # tabulate the count of factors
t <- sort(t, decreasing = TRUE) # order the factors in decreasing order
df <- data.frame(t) # convert to frame

# calculate Freq percentages round to 1 decimal digits
sdf <- df %>% mutate(proc = round((Freq/sum(Freq) * 100), digits=1)) 

#sdf

# set up pie chart plotting

# Percentages drive slice sizes
slices <- sdf$proc

# f is the name of the factor by default, this is the label
 lbls <- sdf$f  

# lbls
# proc is from the above ordering
pct <- sdf$proc
# add percents to labels
lbls <- paste(lbls, pct, sep="= " ) 
# ad % to labels
lbls <- paste(lbls,"%",sep="" ) 
lbls <- paste(strwrap(lbls)) 

# draw the pie chart, cex is the magnification factor for text size
pie(slices, cex=0.7, labels = strwrap(lbls),  main = strwrap(i)) 

})

# Turn off device writing to the PDF file
dev.off()

