---
title: "NPS Survey Analysis"
author: "Eve Johns"
date: "August 28, 2017"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Data Exploration
## 1. Load Data
Load Packages
```{r load package, message=FALSE, warning=FALSE, results="hide"}
library(knitr)
library(markdown)
library(data.table)
library(dplyr)
library(ggplot2)
```
```{r load data, message=FALSE, warning=FALSE, results="hide"}
set.seed(2)
file <- "H:/Eve Johns/Projects/NPS/NPS_Data.csv"
DT <- read.csv(file, header= T)
DT <- data.frame(DT)
```

## 2. Data Overview
1) The dimension of the dataset (rows, columns)
```{r Overview, echo=TRUE, message=TRUE, warning=FALSE}
dim(DT2)
```

2) The column names
```{r variables, message=FALSE, warning=FALSE, results="hide"}
names(DT2)
```



3) Check descriptive statistics
```{r summary, message=FALSE, warning=FALSE, results="hide"}
summary(DT)
round(sapply(DT, mean), 2)
```

4) Factor NPS variable
```{r NPS_f}
NPS_f <- factor(as.integer(DT$NPS))
NPS_f
levels(NPS_f) = list(Detractors = 0:6, Neutrals = 7:8, Promoters = 9:10)
NPS_f
NPS_f <- split(DT$NPS, NPS_f)
summary(NPS_f)
Promoters <- NPS_f$Promoters
Neutrals <- NPS_f$Neutrals
Detractors <- NPS_f$Detractors
```

## 3. Clean the data

1) Check missing values
```{r nonresponse, warning=FALSE, results="hide"}
miss.value <- function(x){sum(is.na(x)/length(x))}
DT.miss <- apply(X=DT, MARGIN = 2, FUN = miss.value)
round(DT.miss, 2)
sort(sapply(DT, function(x) {sum(is.na(x))}), decreasing=TRUE)
```

2) Missing Value Imputation
Clean the dataset, rename columns
```{r train, warning=FALSE, results="hide"}
colnames(DT)

DT2 <- DT[, !(names(DT) %in% c("Email", "CIF", "StartDate","EndDate","Finished","ChildrenUnder18","ChildrenCount","HouseholdIncome","PrimaryResidence","Freq_Tablet","Freq_SmartPhone","Freq_Computer","RatingReason","ProductSelection"))]

#DT2 <- select_if(DT, is.numeric)
#DT2 <- DT2[,c(4,5,8,9,14,15,37,38,39,44)]
colnames(DT2)
#colnames(DT2)[1] <- "NumofProducts"
#colnames(DT2)[2] <- "Balance"
#colnames(DT2)[3] <- "NPS"
#colnames(DT2)[4] <- "OnlineBanking"
#colnames(DT2)[5] <- "ResolutionTimes"
#colnames(DT2)[6] <- "Service"
#colnames(DT2)[7] <- "PrimaryBank"
#colnames(DT2)[8] <- "Referred"
#colnames(DT2)[9] <- "Income"
#colnames(DT2)[10] <- "Why"
#colnames(DT2)
sort(sapply(DT2, function(x) { sum(is.na(x)) }), decreasing=TRUE)

#Income, ReferralProgram, Referred, PrimaryBank, ResolutionTimes, OnlineBanking, Service, NPS have missing values.
```

3) Impute the missing values with KNN
```{r imp, message=FALSE, warning=FALSE, results="hide"}
#DT2 <- na.omit(DT2)
```
```{r DT2 for model}
DT2 <- DT
```


Confirm no missing values
```{r confirm, warning=FALSE}
sum(sapply(DT2, function(x) {sum(is.na(x))}))
dim(DT2)
colnames(DT2)
```


## 4. Data Visualization
1) Correlation Matrix
```{r newDT, message=FALSE, warning=FALSE, include=FALSE, results="hide"}
#head(DT2, 10)
#names(DT2)
#dim(DT2)
DT3 <- select_if(DT2, is.numeric)
dim(DT3)
colnames(DT3)
sum(sapply(DT3, function(x) {sum(is.na(x))}))
```

Create correlation Matrix
```{r corr, echo=TRUE, message=FALSE, warning=FALSE}
cor <- round(cor(DT3),2)
cor
```

2) Correlation Plot
```{r corrplot1, message=FALSE, warning=FALSE, include=FALSE}
library(reshape2)
melted_cor <- melt(cor)
head(melted_cor)
ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) + geom_tile()
get_lower_tri <- function(cor){
    cor[upper.tri(cor)] <- NA
    return(cor)
  }
get_upper_tri <- function(cor){
    cor[lower.tri(cor)]<- NA
    return(cor)
}
upper_tri <- get_upper_tri(cor)
lower_tri <-  get_lower_tri(cor)
upper_tri
lower_tri
melted_cor <-  melt(upper_tri, na.rm = TRUE)
ggplot(data = melted_cor, aes(Var2, Var1, fill = value))+
 geom_tile(color = "white")+
 scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
   midpoint = 0, limit = c(-1,1), space = "Lab", 
   name="Pearson's nCorrelation") +
  theme_minimal()+ 
 theme(axis.text.x = element_text(angle = 45, vjust = 1, 
    size = 12, hjust = 1))+
 coord_fixed()
```

Need to reorder correlation plot
```{r reorder, echo=TRUE, message=FALSE, warning=FALSE}
reorder_cor <- function(cor){
dd <- as.dist((1-cor)/2)
hc <- hclust(dd)
cor <- cor[hc$order, hc$order]
}
cor <-  reorder_cor(cor)
upper_tri <- get_upper_tri(cor)
melted_cor <- melt(upper_tri, na.rm = TRUE)
ggheatmap <- ggplot(melted_cor, aes(Var2, Var1, fill = value))+
 geom_tile(color = "white")+
 scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
   midpoint = 0, limit = c(-1,1), space = "Lab", 
    name="Pearson's Correlation") +
  theme_minimal()+ # minimal theme
 theme(axis.text.x = element_text(angle = 45, vjust = 1, 
    size = 12, hjust = 1))+
 coord_fixed()

ggheatmap + 
geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  panel.grid.major = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.ticks = element_blank(),
  legend.justification = c(1, 0),
  legend.position = c(0.6, 0.7),
  legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                title.position = "top", title.hjust = 0.5))

```

3) NPS rating distribution plot 
NPS Frequency Count Plot
```{r NPS, echo=TRUE, message=FALSE, warning=FALSE}
library(scales)
NPS_f <- factor(as.integer(DT2$NPS))
#NPS_f
levels(NPS_f) <- list(Detractors = 0:6, Neutrals = 7:8, Promoters = 9:10)
Category <- factor(NPS_f)
Hist_CustCount <- ggplot(DT2, aes(x=NPS_f, fill = Category, color = Category)) + geom_histogram(stat = "Count") + xlab("Customer Categories") + labs(title = "Customer Category Count") + theme(plot.title = element_text(hjust = 0.5))
Hist_CustCount
```
```{r NPSvsBal}
stripchart(TotalBalance ~ NPS_f, method = "stack", pch=19, data = DT2)
outlier_TotalBalance <- boxplot.stats(DT2$TotalBalance)$out
outlier_TotalBalance

#outlier function
outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- return(invisible(dt))
}

outlierKD(DT2, TotalBalance)
stripchart(TotalBalance ~ NPS_f, method = "stack", pch=19, data = DT2)

```
4) NPS Frequency Percentage Plot
```{r NPS2, echo=TRUE, message=FALSE, warning=FALSE}
Hist_CustPerc <- ggplot(DT2, aes(x = NPS_f, fill = Category, color = Category)) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + scale_y_continuous(labels = percent) + labs(title = "Customer Category Percentage", y = "Percent", x = "Customer Category") +theme(plot.title = element_text(hjust = 0.5))
Hist_CustPerc
```
Factor OnlineBanking
NPS vs. OnlineBanking
```{r OnlineBanking}
OnlineBanking_f <- factor((DT2$OnlineBanking))
levels(OnlineBanking_f) <- list(Bad = 1:2, Neutral =3, Good = 4:5)
#OnlineBanking_f
with(DT2, table(NPS_f, OnlineBanking_f))
plot(NPS_f ~ OnlineBanking_f)
```
OnlineBanking vs. Customerdays 
```{r CustomerDays}
with(DT2, do.call(rbind, tapply(CustomerDays, OnlineBanking_f, function(x) c(M = mean(x), SD = sd(x)))))
plot(DT2$CustomerDays ~ OnlineBanking_f, Data = DT2)
```
NPS by Primary Bank Status
```{r CustomerDays2}
Primarybank_f <- factor(DT2$Primarybank)
levels(Primarybank_f) <- list(Yes = 1, No =2)
table_PrimaryBank <- with(DT2, table(NPS_f, Primarybank_f))
table_PrimaryBank
plot(NPS_f ~ Primarybank_f)
```


5) Word cloud of NPS rating reason
Load packges
```{r pacakges, message=FALSE, warning=FALSE, results="hide"}
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
```

Load the column [Why] as corpus
```{r why, echo=TRUE, message=FALSE, warning=FALSE}
why <- DT2$Why
is.vector(why)
words <- Corpus(VectorSource(why))
```

Inspect the contents of column why
```{r inspect, echo=TRUE, message=FALSE, warning=FALSE, results="hide"}
inspect(words)
```

Cleaning the text:

a) clean up the special characters from the text.
```{r trans, message=FALSE, warning=FALSE, results="hide"}
toSpace <- content_transformer(function(x,pattern) gsub(pattern," ", x))
words  <- tm_map(words, toSpace, "/")
words  <- tm_map(words, toSpace, "@")
words  <- tm_map(words, toSpace, "NULL")
```

b) Remove numbers, english common stopwords, punctuations, extra white spaces
```{r remove, message=FALSE, warning=FALSE, results="hide"}
words <- tm_map(words,removeNumbers)
words <- tm_map(words, removeWords, stopwords("english"))
words <- tm_map(words, removePunctuation)
words <- tm_map(words, stripWhitespace)
words <- tm_map(words, removeWords, c("although","back","anyone","ago","around", "full", "will", "getting", "thing", "sometimes", "person", "made", "think", "keep", "overall", "etc", "minimum","customers", "really", "now","used","its","when","need", "still","business","bank","banks","always","also","one","can","get","account","atm","internet","website","the"))
```

c) Build term-document matrix
```{r matrix, message=FALSE, warning=FALSE, results="hide"}
dtm <- TermDocumentMatrix(words)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing = TRUE)
d <- data.frame(word = names(v), freq=v)
class(d)
#head(d,10)
```

d) Find frequent terms
```{r freq, message=FALSE, warning=FALSE, include=FALSE}
findFreqTerms(dtm, lowfreq = 10)
```

e) Generate the word cloud
```{r cloud, echo=TRUE, message=FALSE, warning=FALSE}
set.seed(10)

wordcloud(words = d$word, freq = d$freq, min.freq = 8, max.words = 100, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(n=8, "Dark2"))
```


# Model Fitting
```{r mlogit, eval=FALSE, include=FALSE}
library(nnet)
fit1 <- multinom()

library(foreign)
NPS_f <- factor(as.integer(DT2$NPS))
#NPS_f
levels(NPS_f) <- list(Detractors = 0:6, Neutrals = 7:8, Promoters = 9:10)
#DT2$NPS = as.factor(NPS_f)
#summary(DT2)

library(mlogit)
#DT3 = mlogit.data(DT2, varying = NULL, choice = "NPS", shape = "wide")
#head(DT3)
#colnames(DT3)
#fit1 = mlogit(NPS ~ 1 | CustomerDays + TotalBalance + OnlineBanking + DepositHoldTimes + ResolutionTimes + CustomerSupport + ManageYourAccount + FindingAccountInfo + WireTransfer + Primarybank + Referred + ReferralProgram, data = DT3, reflevel = "Promoters")
#summary(fit1)

colnames(DT2)
as.factor(DT2$NPS)
DT2$NPS1 <- relevel(NPS_f, ref = "Promoters" )


library(mlogit)
DT3 <- mlogit.data(DT2, varying = NULL, choice = "NPS", shape = "wide")
NPS1_fit <- mlogit(NPS ~ 1 | CustomerDays + TotalBalance + Age + OnlineBanking + MobileApp + DepositHoldTimes + ResolutionTimes + CustomerSupport + TrasferIn + TransferOut + ManageYourAccount + FindingAccountInfo + WireTransfer + Primarybank + Referred + ReferralProgram, data =DT3, reflevel = "Promoters")
step(NP1_fit, direction = "both")
summary(NPS1_fit)


fullmod_NPS1 <- glm(NPS1)

```


## Ordinal Logistics Regression Model Fit
```{r ordinal}

library(ordinal)
library(foreign)
library(MASS)
library(ggplot2)
library(Hmisc)
library(reshape2)

head(DT2)
#Using polr to estimate an ordered logistic regression, Hess=TRUE to let the model output show the observed information matrix from optimization which is used to get standard errors.
m <- polr(NPS ~ OnlineBanking + MobileApp + DepositHoldTimes + ResolutionTimes + CustomerSupport + TrasferIn + TransferOut + ManageYourAccount + FindingAccountInfo + WireTransfer + Primarybank + Referred + ReferralProgram, data = DT2, Hess = TRUE)
summary(DT2)
DT3 <- na.omit(DT2)
m2<- polr(NPS ~ OnlineBanking + MobileApp + DepositHoldTimes + ResolutionTimes + CustomerSupport + TrasferIn + TransferOut + ManageYourAccount + FindingAccountInfo + WireTransfer + Primarybank + Referred + ReferralProgram, data = DT3, Hess = TRUE)
summary(m2)
ctable <- coef(summary(m2))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
#ctable
ctable[,4] <- round(ctable[,4],8)
ctable

#Check again with Stepwise Selection
step(m2)

```
NPS ~ OnlineBanking + CustomerSupport + TrasferIn + FindingAccountInfo + Referred + ReferralProgram


##Logistic Regression
###Logistic Regression for Promoters
```{r logit1}
DT_omit <- DT[!is.na(DT$NPS),]
#summary(DT_omit)
#as.numeric(DT_omit$NPS)
DT_omit$NPS <- ifelse(DT_omit$NPS > 8, 1,0)
#DT_omit$NPS
fit_Promo <- glm(NPS ~ OnlineBanking + CustomerSupport + TrasferIn + FindingAccountInfo + Referred + ReferralProgram, data = DT_omit, family = "binomial")
summary(fit_Promo)
```

###Logistic Regression for Detractors
```{r logit2}
DT_omit$NPS
DT_omit$NPS <- ifelse(DT_omit$NPS < 7 , 1, 0)
fit_Detr <- glm(NPS ~ OnlineBanking + CustomerSupport + TrasferIn + FindingAccountInfo + Referred + ReferralProgram, data = DT_omit, family = "binomial")
summary(fit_Detr)
```

## Multinomial
```{r Multinomial}
library(nnet)
fit1 <- multinom()

library(foreign)
NPS_f <- factor(as.integer(DT2$NPS))
#NPS_f
levels(NPS_f) <- list(Detractors = 0:6, Neutrals = 7:8, Promoters = 9:10)
#DT2$NPS = as.factor(NPS_f)
#summary(DT2)

library(mlogit)
#DT3 = mlogit.data(DT2, varying = NULL, choice = "NPS", shape = "wide")
#head(DT3)
#colnames(DT3)
#fit1 = mlogit(NPS ~ 1 | CustomerDays + TotalBalance + OnlineBanking + DepositHoldTimes + ResolutionTimes + CustomerSupport + ManageYourAccount + FindingAccountInfo + WireTransfer + Primarybank + Referred + ReferralProgram, data = DT3, reflevel = "Promoters")
#summary(fit1)

colnames(DT2)
as.factor(DT3$NPS)
DT2$NPS1 <- relevel(NPS_f, ref = "Promoters" )


library(mlogit)
DT3 <- mlogit.data(DT2, varying = NULL, choice = "NPS", shape = "wide")
NPS1_fit <- mlogit(NPS ~ 1 | CustomerDays + TotalBalance + Age + OnlineBanking + MobileApp + DepositHoldTimes + ResolutionTimes + CustomerSupport + TrasferIn + TransferOut + ManageYourAccount + FindingAccountInfo + WireTransfer + Primarybank + Referred + ReferralProgram, data =DT3, reflevel = "Promoters")
step(NP1_fit, direction = "both")
summary(NPS1_fit)


fullmod_NPS1 <- glm(NPS1)
```


