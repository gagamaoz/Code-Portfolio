### MH3511 Project
### Date Created - 08 March 2022 1539H

# Guide to using Github
# 1. Reload and fetch origin
# 2. Download the new updated file
# 3. Edit your code
# 4. Save your code
# 5. Github app will update and show you a summary of your edits
# 6. Give a title to your edits under Update Script.R and a description of your edits
# 7. Push your edit by selecting "commit to main"

### Import Required Libraries
library(data.table)
library(corrplot)
library(dplyr)
library(moments)
library(rpart)

### Import Data
fifaData <- fread("FIFA21_Dataset.csv", header = TRUE, encoding = "UTF-8")

### Data Description - Preliminary data cleaning ###

## Eliminate irrelevant columns
fifaData <- fifaData[, c('Age','Nationality','Potential','Club','Wage','International Reputation','Height','Weight','Reactions','Best Position','Best Overall Rating')]

## Convert all clumns related to money to numerical data type

# Remove Wage = 0
fifaData <- fifaData[fifaData$Wage != "€0"]

# Function for cleaning the columns related to money
cleanMoney <- function(x) {
    x <- gsub("\\€", "", x) # Remove the euro symbol
    suffix <- substr(x, nchar(x), nchar(x)) # Get the suffix of "K" / "M"{
    if (suffix == "K") {
        x <- as.numeric(substr(x, 1, nchar(x) - 1)) * 10^3
    } else if (suffix == "M") {
        x <- as.numeric(substr(x, 1, nchar(x) - 1)) * 10^6
    } else {
        x <- as.numeric(x)
    }
    
    return(x)
}

## Clean all columns related to money
fifaData$Wage <- cleanMoney(fifaData$Wage)

## Remove redundant information 
# Converting Weight - In Pounds
fifaData$Weight <- gsub("[a-z]", "", fifaData$Weight)
fifaData$Weight <- as.numeric(fifaData$Weight)

# Converting Height - From Feet To Cm
fifaData$Height <- gsub("[']", "", fifaData$Height)
fifaData$Height <- as.numeric(fifaData$Height)
n <- nchar(fifaData$Height)
fifaData$Height <- as.numeric(substr(fifaData$Height, 1, 1)) * 0.3048 + as.numeric(substr(fifaData$Height, 2, n)) * 0.0254 # Converting feet & inches to meters

## Filtering for clubs in English Premier League
english <- c("Arsenal",
             "Aston Villa",
             "Brighton & Hove Albion",
             "Burnley",
             "Chelsea",
             "Crystal Palace",
             "Everton",
             "Fulham",
             "Leeds United",
             "Leicester City",
             "Liverpool",
             "Manchester City",
             "Manchester United",
             "Newcastle United",
             "Sheffield United",
             "Southampton",
             "Tottenham Hotspur",
             "West Bromwich Albion",
             "West Ham United",
             "Wolverhampton Wanderers")
fifaData <- fifaData[fifaData$Club %in% english, ]

## Creating new variable lwage (Attempt to normalize wage)
fifaData[, 'lwage'] = log(fifaData$Wage)

summary(fifaData)
str(fifaData)


### Description and Cleaning of data set

# Exploratory Data Analysis For Dependent Variable - Wage

# 3.1 Summary statistics for wage
summary(fifaData$Wage)
skewness(fifaData$Wage) # 2.08 > 0 Thus Right-Skewed
kurtosis(fifaData$Wage) # 10.53 > 3 Thus Leptokurtic Based On https://www.geeksforgeeks.org/skewness-and-kurtosis-in-r-programming/

hist(fifaData$Wage, breaks = 50) # Right-Skewed
qqnorm(fifaData$Wage)
qqline(fifaData$Wage,col='red')

# 3.1 Summary statistics for lwage
summary(fifaData$lwage)
skewness(fifaData$lwage)
kurtosis(fifaData$lwage)

hist(fifaData$lwage)
xpt = seq(from=6,to=13,by=0.01)
ypt = dnorm(xpt,mean=mean(fifaData$lwage),sd=sd(fifaData$lwage))
ypt = ypt * length(fifaData$lwage) * 0.5
lines(xpt,ypt,col='red')
qqnorm(fifaData$lwage)
qqline(fifaData$lwage,col='red')


# 3.2 - Summary statistics for other variables
# 3.2.1 - Age
boxplot(fifaData$Age)
qqnorm(fifaData$Age)
qqline(fifaData$Age,col='red')

# 3.2.2 - Potential
boxplot(fifaData$Potential)
qqnorm(fifaData$Potential)
qqline(fifaData$Potential,col='red')

# 3.2.3 - Reaction
boxplot(fifaData$Reactions)
qqnorm(fifaData$Reactions)
qqline(fifaData$Reactions,col='red')

# 3.2.4 - Best Overall Rating
boxplot(fifaData$`Best Overall Rating`)
qqnorm(fifaData$`Best Overall Rating`)
qqline(fifaData$`Best Overall Rating`,col='red')

# 3.2.5 - Height
boxplot(fifaData$Height)
qqnorm(fifaData$Height)
qqline(fifaData$Height,col='red')

# 3.2.6 - Weight
boxplot(fifaData$Weight)
qqnorm(fifaData$Weight)
qqline(fifaData$Weight,col='red')

# remove three outlier as suggested from the boxplot on potential
fifaData = fifaData[!(fifaData$Potential < quantile(fifaData$Potential,0.25) - 1.5 * IQR(fifaData$Potential))]
# remove four outlier as suggested from the boxplot on weight
fifaData = fifaData[!(fifaData$Weight > quantile(fifaData$Weight,0.75) + 1.5 * IQR(fifaData$Weight))]

# 3.3 - Should have 796 observations of 12 variables by now
str(fifaData)


### Statistical Analysis ###

# 4.1 - Correlation between ln(Wage) and other continuous variables
fifaData_numeric <- fifaData %>% dplyr::select(where(is.numeric))
fifaData_clean <- cor(fifaData_numeric, use = "pairwise.complete.obs")
corrplot(fifaData_clean, type = "lower", method = "color", addCoef.col = "black", number.cex = 0.6, tl.cex = 0.5,diag = F,title='Correlation plot for fifaData numerical variables',mar=c(0,0,2,0))

# 4.2 Statistical Tests

# 4.2.1 - Relation between Wage and Best Position
boxplot(lwage ~ `Best Position`, data = fifaData, main="Boxplot of log(wage) vs Best Position")
# 4.2.1 - ANOVA Model
aov(fifaData$lwage~factor(fifaData$`Best Position`))
summary(aov(fifaData$lwage~factor(fifaData$`Best Position`))) #pvalue=0.00356, reject null hyp --> not all means are equal
# 4.2.1 - Pairwise t-test by position
pairwise.t.test(fifaData$lwage, fifaData$`Best Position`, p.adjust.method = "none")

# Group positions into categories
fifaData$cat = 0
fwd = c("LW", "RW", "RF", "LF", "ST", "CF")
mf = c("CAM", "CM", "CDM", "RM", "LM")
def = c("RWB", "LWB", "RB", "LB", "CB")
gk = "GK"
fifaData$cat[fifaData$`Best Position` %in% fwd] <- "Forward" 
fifaData$cat[fifaData$`Best Position` %in% mf] <- "Midfielder" 
fifaData$cat[fifaData$`Best Position` %in% def] <- "Defender"
fifaData$cat[fifaData$`Best Position` %in% gk] <- "Goalkeeper"

#Pairwise t test
# 4.2.1 - ANOVA model
aov(fifaData$lwage~factor(fifaData$cat))
summary(aov(fifaData$lwage~factor(fifaData$cat)))
# 4.2.1 - Pairwise t-test by position category
pairwise.t.test(fifaData$lwage, fifaData$cat, p.adjust.method = "none")


# 4.2.2 - The single most important measure that is affecting ln(Wage)

# 4.2.2.1 - Univariate linear regression

# against Age
model1 = lm(lwage~Age,data = fifaData)
summary(model1)
qqnorm(model1$residuals)
qqline(model1$residuals,col='red')

# against Potential
model2 = lm(lwage~Potential,data = fifaData)
summary(model2)
qqnorm(model2$residuals)
qqline(model2$residuals,col='red')

# against Reaction
model3 = lm(lwage~Reactions,data = fifaData)
summary(model3)
qqnorm(model3$residuals)
qqline(model3$residuals,col='red')

# against Best overall rating
model4 = lm(lwage~`Best Overall Rating`,data = fifaData)
summary(model4)
qqnorm(model4$residuals)
qqline(model4$residuals,col='red')

# 4.2.2.2 - Multiple Linear Regression
model5 = lm(lwage~Age + Potential + Reactions + `Best Overall Rating`,data=fifaData)
model5_step <- step(model5,direction='backward')
summary(model5_step)

# Multivariate linear regression without potential
model6 = lm(lwage~Age + Reactions + `Best Overall Rating`,data=fifaData)
model6_step <- step(model6,direction='backward')
summary(model6_step)

# 4.2.3 - Relationship between players' wages and their clubs

boxplot(Wage ~ Club, data = fifaData)

# Top 6 clubs = Arsenal, Chelsea, Liverpool, Manchester City, Manchester United, Tottenham Hotspur
top6Clubs <- c('Arsenal', 'Chelsea', 'Liverpool', 'Manchester City', 'Manchester United', 'Tottenham Hotspur')
fifaData$top6 <- ifelse(fifaData$Club %in% top6Clubs, 1, 0)

# Top 6 clubs players
top6players <- fifaData[fifaData$top6 == 1]
non6players <- fifaData[fifaData$top6 == 0]

var.test(top6players$Wage, non6players$Wage)
# P-value < 2.2e-16, reject null hyp --> the variances are not equal

t.test(top6players$Wage, non6players$Wage, var.equal = FALSE)
# P-value < 2.2e-16, reject null hyp --> the means are not equal

# 4.2.3 - Create a table comparing International Reputation and Top6
IR_Top6 <- table(fifaData$top6, fifaData$`International Reputation`)
rownames(IR_Top6)=c("non6", "top6")
colnames(IR_Top6)=c("int rep 1", "int rep 2", "int rep 3", "int rep 4")

# 4.2.3 - Expected values for International Reputation and Top6 (Optional)
colsum = matrix(colSums(IR_Top6), ncol=4)
rowsum = matrix(rowSums(IR_Top6), ncol=1)
exIR_Top6 = rowsum %*% colsum / sum(colsum)

# 4.2.3 - Chi-square test
chisq.test(IR_Top6)

# 4.2.4 - Wage quartiles vs International Reputation
# 4.2.4 - Adding new column WageQuartile
summary(fifaData$Wage) # To find wage quartiles
fifaData$WageQuartile <- ifelse(fifaData$Wage <= 40000, (ifelse(fifaData$Wage <= 10000, 1, 2)), (ifelse(fifaData$Wage <= 63250, 3, 4)))

# 4.2.4 - Create a table comparing International Reputation and Wage quartiles
Wage_IR <- table(fifaData$WageQuartile, fifaData$`International Reputation`)
rownames(Wage_IR)=c("1st Qu.", "2nd Qu.", "3rd Qu.", "4th Qu.")
colnames(Wage_IR)=c("int rep 1", "int rep 2", "int rep 3", "int rep 4")

# 4.2.4 - Finding expected values (Optional)
colsum = matrix(colSums(Wage_IR), ncol=4)
rowsum = matrix(rowSums(Wage_IR), ncol=1)
exWage_IR = rowsum %*% colsum / sum(colsum)

# 4.2.4 - Chi-square test
chisq.test(Wage_IR)

# 4.2.5 - Log wage to best overall rating has correlation coefficient of 0.9
fifaData$LwagePerOverall <- fifaData$lwage / fifaData$`Best Overall Rating`

# 4.2.5 - All nationalities
nationalities <- unique(sort(fifaData$Nationality))
col <- rep("Blue", length(nationalities))
col[nationalities == "England"] <- "Red"
boxplot(LwagePerOverall ~ Nationality, data = fifaData, col = col) # England players have inflated wage per overall

# 4.2.5 - Countries with more than 15 players
newData <- fifaData %>% count(Nationality, sort = TRUE) %>% filter(n > 15)
nationalities <- sort(newData$Nationality)
col <- rep("Blue", length(nationalities))
col[nationalities == "England"] <- "Red"
requiredData <- fifaData[fifaData$Nationality %in% nationalities, ]
boxplot(LwagePerOverall ~ Nationality, data = requiredData, col = col) # England players have inflated wage per overall

# ANOVA For Wage Vs Nationality
summary(aov(Wage ~ Nationality, data = requiredData))
pairwise.t.test(requiredData$Wage, requiredData$Nationality, p.adjust.method = "none")

# We see that Republic of Ireland, Scotland and Wales have high p-value with england, meaning they
# have similar distributions, thus we shall group all the Great Britain players together
great_britain <- c("England", "Wales", "Scotland", "Republic of Ireland")
fifaDataGB <- copy(fifaData)
fifaDataGB[fifaDataGB$Nationality %in% great_britain, "Nationality"] <- "The UK & Ireland"

# All nationalities - Great Britain
nationalities <- unique(sort(fifaDataGB$Nationality))
col <- rep("Blue", length(nationalities))
col[nationalities == "The UK & Ireland"] <- "Red"
boxplot(LwagePerOverall ~ Nationality, data = fifaDataGB, col = col) # England players have inflated wage per overall

# Countries with more than 10 players
newData <- fifaDataGB %>% count(Nationality, sort = TRUE) %>% filter(n > 10)
nationalities <- sort(newData$Nationality)
col <- rep("Blue", length(nationalities))
col[nationalities == "The UK & Ireland"] <- "Red"
requiredData <- fifaDataGB[fifaDataGB$Nationality %in% nationalities, ]
boxplot(LwagePerOverall ~ Nationality, data = requiredData, col = col) # England players have inflated wage per overall

# ANOVA For Wage Vs Nationality
summary(aov(Wage ~ Nationality, data = requiredData))
pairwise.t.test(requiredData$Wage, requiredData$Nationality, p.adjust.method = "none")
pairwise.t.test(requiredData$Wage, requiredData$Nationality, p.adjust.method = "none", alternative = "less")

# ---------------------------------------------------------------------------------------------------------------------







## Fabian
# Univariate Linear regression of log(wage) against x

# Boxplots of various variables
boxplot(fifaData$Age)
boxplot(fifaData$Potential)
boxplot(fifaData$Reactions)
boxplot(fifaData$`Best Overall Rating`)

head(sort(fifaData$Potential,decreasing=F),3)










## Yoshiki
IR_Top6_percentage <- matrix(c((418/(418+131)), (110/(110+57)), (25/(25+51)), 1/11, 
                               (131/(131+418)), (57/(110+57)), (51/(51+25)), 10/11)*100, 
                             ncol=4, byrow=TRUE)
IR_Top6_percentage



Wage_IR


# ---- Nationality ----
fifaData_numeric <- fifaData %>% dplyr::select(where(is.numeric))
corrplot(cor(fifaData_numeric, use = "pairwise.complete.obs"), 
         type = "upper",
         method = "color",  
         addCoef.col="black",
         number.cex = 0.6,
         tl.cex = 0.6,
         col = COL2('RdBu', 10),
         diag = FALSE,
         cl.pos = "b") 

# Decision Tree - CART
model <- rpart(Wage ~ ., data = fifaData, method = "anova")
pred <- predict(model, fifaData)
sqrt(mean((pred - fifaData$Wage) ^ 2))
mean(fifaData$Wage)

# Yosh Corrplot (columns 41:49)
yosh.fifaDataClean <- epl %>% dplyr::select(7, 41:49, -"Best Position")
str(yosh.fifaDataClean)
corrplot(cor(yosh.fifaDataClean, use = "pairwise.complete.obs"), 
         type = "upper",
         method = "color",  
         addCoef.col="black",
         number.cex = 0.6,
         tl.cex = 0.6,
         col = COL2('RdBu', 10),
         diag = FALSE,
         cl.pos = "b")