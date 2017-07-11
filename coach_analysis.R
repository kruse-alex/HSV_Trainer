#############################################################################################################################################
# PREPARE DATA
#############################################################################################################################################

# pkgs
require(sqldf)
require(tidyverse)
require(data.table)
require(TraMineR)
require(splitstackshape)

# get data
setwd("K:/Consulting/13_Alex_Data_Analyst/Datenanalyse_Projekte/Weitere/buli")
mydata = read.csv("1liga.csv", header = T, sep = ";")
trainer = read.csv("trainer.csv", header = T, sep = ";")
trainer$beginning = as.Date(trainer$beginning, "%d.%m.%Y")
trainer$ending = as.Date(trainer$ending, "%d.%m.%Y")

mydata = select(mydata, Datum, Team..H., Team..A., Tore..H., Tore..A.)
mydata$Datum = as.Date(as.character(mydata$Datum), "%Y%m%d")
mydata = filter(mydata, Team..H. == "Hamburger SV" | Team..A. == "Hamburger SV")
mydata$check = mydata$Team..H. == "Hamburger SV"
mydata$Tore..H. = as.numeric(as.character(mydata$Tore..H.))
mydata$Tore..A. = as.numeric(as.character(mydata$Tore..A.))
mydata$ergebnis[mydata$Tore..H. == mydata$Tore..A.] = "unentschieden"
mydata$ergebnis[mydata$Team..H. == "Hamburger SV" & (mydata$Tore..H. > mydata$Tore..A.)] = "sieg"
mydata$ergebnis[mydata$Team..A. == "Hamburger SV" & (mydata$Tore..A. > mydata$Tore..H.)] = "sieg"
mydata$ergebnis[mydata$Team..H. != "Hamburger SV" & (mydata$Tore..H. > mydata$Tore..A.)] = "niederlage"
mydata$ergebnis[mydata$Team..A. != "Hamburger SV" & (mydata$Tore..A. > mydata$Tore..H.)] = "niederlage"

mydata = mydata[complete.cases(mydata),]
mydata$ergebnis = as.factor(mydata$ergebnis)

mydata = select(mydata, Datum, ergebnis)

mydata = as.data.table(mydata)
trainer = as.data.table(trainer)

result = sqldf("select * from mydata
                left join trainer
                on mydata.Datum between trainer.beginning and trainer.ending")

result = as.data.frame(result)
rm(mydata, trainer)
result = select(result, name, ergebnis, Datum)

#############################################################################################################################################
# ANALYZE SEQUENCES
#############################################################################################################################################

# create sequnce per user and add converion value
result$ergebnis <- as.character(result$ergebnis)
result = result[complete.cases(result),]
seq <- result %>%
  group_by(name) %>%
  summarise(path = as.character(list(ergebnis)),
            Datum = Datum[1])

# sort by date
seq = seq[order(seq$Datum, decreasing = T), ]
seq$Datum = NULL

# clean paths
seq$path <- gsub("c\\(|)|\"|([\n])","", seq$path)
seq$path <- gsub(",","\\1 \\2>", seq$path)

# split path into single columns
seq.table <- cSplit(as.data.table(seq), "path", ">")

# create sequence object with maximum length (length 20 = 99% data)
seq.seq <- seqdef(seq.table[,2:205], id = seq.table$name) 

#############################################################################################################################################
# SOME SEQUENCE ANALYSIS FUNCTIONS
#############################################################################################################################################

# state distribution plot
seqiplot(seq.seq, cpal=c("red","green","yellow"), tlim = 1:37)
