###############################################################################
# Analysis of past borrower data in effort to find commonalities
###############################################################################

# ensurePkg tests whether the packages that run_analysis uses are installed and, if not, installs them.
ensurePkg <- function(x) {
    if (!require(x,character.only = TRUE)) {
        install.packages(x,dep=TRUE, repos="http://cran.r-project.org")
        if(!require(x,character.only = TRUE)) stop("Package not found")
    }
}
ensurePkg("ggplot2")
ensurePkg("plyr")
ensurePkg("scales")
ensurePkg("corrplot")

# Read in the .xlsx file, if not already done so
#if(!exists("borrowers")) {
    borrowers <- read.csv("BorrowerDemos.csv", na.strings = "")
#}

# Clean up some data issues
borrowers$Race.Descr <- gsub(", ", "", borrowers$Race.Descr)
borrowers$Funded.Date <- as.Date(as.character(borrowers$Funded.Date), "%m/%d/%y")
# Subset to just loans since the new loans debuted in April and for third-party loans only
borrowers <- subset(borrowers, borrowers$Funded.Date > "2014-03-31" & borrowers$Loan.Type == "Third Party Lending")
# Convert loan amount to a numeric
borrowers$Loan.Amt <- as.numeric(gsub(",", "", borrowers$Loan.Amt))
# Get rid of columns we don't care about, to make it more manageable
borrowers[, c(2,5:6,11:12,18:26,28:29,43:51,54:63)] <- list(NULL)


# Fix the inconsistency among strategy descriptions
both <- grep("^Both", borrowers$Preferred.Strategy, ignore.case=TRUE)
for(i in both) {
    borrowers[i, 12] <- "Both"
}
rent <- grep("Rent|Hold", borrowers$Preferred.Strategy, ignore.case=TRUE)
for(i in rent) {
    borrowers[i, 12] <- "Rent"
}
# "Flip" doesn't exist as a factor so it must be added
borrowers$Preferred.Strategy <- revalue(borrowers$Preferred.Strategy, c("Fix/Flip" = "Flip"))
flip <- grep("Flip", borrowers$Preferred.Strategy, ignore.case=TRUE)
for(i in flip) {
    borrowers[i, 12] <- as.factor("Flip")
}
rent <- grep("lease", borrowers$Preferred.Strategy, ignore.case=TRUE)
for(i in rent) {
    borrowers[i, 12] <- "Other"
}

# And combine races
black <- grep("black", borrowers$Race.Descr, ignore.case=TRUE)
for(i in black) {
    borrowers[i, 29] <- "Black"
}
other <- grep("^[[:space:]]*$", borrowers$Race.Descr, ignore.case=TRUE)
for(i in other) {
    borrowers[i, 29] <- "Other"
}

# Get rid of all unused factors
borrowers <- droplevels(borrowers)

# Split by preferred strategy
strategySplit <- split(borrowers, borrowers$Preferred.Strategy)
lapply(strategySplit, function(x) mean(na.omit(x$Age.at.Loan))) # No real differences

# Age
table(borrowers$Age.at.Loan)
mean(na.omit(borrowers$Age.at.Loan))
median(na.omit(borrowers$Age.at.Loan))
# Race
table(borrowers$Race.Descr)
with(borrowers, addmargins(table(Race.Descr, Sex)))
addmargins(round(with(borrowers, prop.table(table(Race.Descr, Sex))), 2))
with(borrowers, ftable(Race.Descr, Sex, Marital.Status))
summary(xtabs(~Race.Descr+Sex+Marital.Status, data=borrowers))
# Sex
table(borrowers$Sex)
# Years in School
table(borrowers$Yrs.in.School)
# US Citizenship
table(borrowers$US.Citizen)
# Loan Amount
mean(borrowers$Loan.Amt)
median(borrowers$Loan.Amt)
# State
table(borrowers$PropertyState)


## Variables by program 
ddply(borrowers, .(Loan.Program, Race.Descr, Preferred.Strategy), summarize, 
      count=length(Loan.Program), 
      meanAge = round(mean(Age.Today, na.rm = TRUE), 1), 
      meanYrsInv = round(mean(Years.investing, na.rm = TRUE), 1), 
      meanLoanAmount = comma(round(mean(Loan.Amt, na.rm = TRUE), 0)), 
      married = length(Sex)
)



# Correlations?
colors <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
borrowerCor <- borrowers[, c(3, 9:11, 13, 19:21, 23:25)]
borrowerCorr <- cor(borrowers, use="pairwise.complete.obs")
corrplot(borrowerCorr, method="shade", tl.col="black", tl.srt=45, order="AOE", col=colors(300))


