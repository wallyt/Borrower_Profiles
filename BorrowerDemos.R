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
borrowers[, c(1:3,5:6,8,11:12,18:26,28:29,32,42:51,53:63,65)] <- list(NULL)


# Fix the inconsistency among strategy descriptions
both <- grep("^Both", borrowers$Preferred.Strategy, ignore.case=TRUE)
for(i in both) {
    borrowers[i, 9] <- "Both"
}
rent <- grep("Rent|Hold", borrowers$Preferred.Strategy, ignore.case=TRUE)
for(i in rent) {
    borrowers[i, 9] <- "Rent"
}
# "Flip" doesn't exist as a factor so it must be added
borrowers$Preferred.Strategy <- revalue(borrowers$Preferred.Strategy, c("Fix/Flip" = "Flip"))
flip <- grep("Flip", borrowers$Preferred.Strategy, ignore.case=TRUE)
for(i in flip) {
    borrowers[i, 9] <- as.factor("Flip")
}
rent <- grep("lease", borrowers$Preferred.Strategy, ignore.case=TRUE)
for(i in rent) {
    borrowers[i, 9] <- "Other"
}

# And combine races
black <- grep("black", borrowers$Race.Descr, ignore.case=TRUE)
for(i in black) {
    borrowers[i, 23] <- "Black"
}
other <- grep("^[[:space:]]*$", borrowers$Race.Descr, ignore.case=TRUE)
for(i in other) {
    borrowers[i, 23] <- "Other"
}
borrowers$Race.Descr <- as.factor(borrowers$Race.Descr)

# Get rid of all unused factors
borrowers <- droplevels(borrowers)

# Calculate correlation coefficients
hetcor(borrowers[, 1:23], std.err = FALSE)

#############################################################################
# Analysis
#############################################################################

## Generate tables for correlations above/below +- 0.5
addmargins(round(with(borrowers, prop.table(table(Sex, Loan.Purpose))), 2)) # M&F both prefer refi 2:1
addmargins(round(with(borrowers, prop.table(table(US.Citizen, Loan.Purpose))), 2)) # Irrelevant since 96% are citizens
addmargins(round(with(borrowers, prop.table(table(Years.investing, Full.or.Part.time.Investor))), 2)) # 52% of PTs have <= 5 yrs vs 35% of FTs
# ^ About 48/54 are FT/PT, respectively
quantile(borrowers$Years.investing, na.rm = TRUE) # Median experience is 7 years
addmargins(round(with(borrowers, prop.table(table(Preferred.Strategy, Full.or.Part.time.Investor))), 2)) # PTs hold (48%), FTs flip or both (71%)
# ^ 35% rent, 26% flip, 32% both, 8% other: compare to survey
addmargins(round(with(borrowers, prop.table(table())), 2))

## Exploring other relationships that aren't correlated
addmargins(round(with(borrowers, prop.table(table(Sex, Marital.Status))), 2)) # Men slightly more likely to be married

# Split by preferred strategy
strategySplit <- split(borrowers, borrowers$Preferred.Strategy)
lapply(strategySplit, function(x) mean(na.omit(x$Age.at.Loan))) # No real differences

# Age
table(borrowers$Age.at.Loan) # FIND A BETTER WAY TO GROUP AND SHOW
mean(na.omit(borrowers$Age.at.Loan))
median(na.omit(borrowers$Age.at.Loan)) # Mean ~ Median

# Race
addmargins(round(prop.table(table(borrowers$Race.Descr)), 2)) # 54% white, 34% black, 6% hispanic
addmargins(round(with(borrowers, prop.table(table(Race.Descr, Sex))), 2)) # 55% of men are white, 32% black; women slightly more likely to be black: 52% white, 40% black
# ^ 42% white men, 25% black men, 13% white women, 10% black women
addmargins(round(with(borrowers, prop.table(table(Race.Descr, Age.at.Loan))), 2))
addmargins(round(with(borrowers, prop.table(table(Race.Descr, Sex, Marital.Status))), 2))
# ^ 23% married white men, 18% unmarried white men, 10% married black men, 15% unmarried black men
# ^ 6% married white women, 6% unmarried white women, 4% married black women, 6% unmarried black women

# US Citizenship
addmargins(table(borrowers$US.Citizen)) # Despite our pitching that they don't have to be citizens, 96% are

# Years in School, but get rid of outliers
quantile(borrowers$Yrs.in.School, na.rm=TRUE)
boxplot(borrowers$Yrs.in.School, outline = FALSE, horizontal = TRUE, col = "bisque")
# ^ Median is 16 years, implying that our median borrower has an undergrad degree

# Loan Amount
mean(borrowers$Loan.Amt)
median(borrowers$Loan.Amt)
addmargins(round(with(borrowers, prop.table(table(Loan.Amt, Marital.Status))), 2)) # FIND A BETTER WAY TO GROUP AND SHOW
addmargins(round(with(borrowers, prop.table(table(Loan.Amt, Sex))), 2)) # FIND A BETTER WAY TO GROUP AND SHOW
addmargins(round(with(borrowers, prop.table(table(Loan.Amt, Race.Descr))), 2)) # FIND A BETTER WAY TO GROUP AND SHOW
addmargins(round(with(borrowers, prop.table(table(Loan.Amt, Preferred.Strategy))), 2)) # FIND A BETTER WAY TO GROUP AND SHOW


## Variables by program 
ddply(borrowers, .(Loan.Program, Race.Descr, Preferred.Strategy), summarize, 
      count=length(Loan.Program), 
      meanAge = round(mean(Age.Today, na.rm = TRUE), 1), 
      meanYrsInv = round(mean(Years.investing, na.rm = TRUE), 1), 
      meanLoanAmount = comma(round(mean(Loan.Amt, na.rm = TRUE), 0)), 
      married = length(Sex)
)

# calculates chi-squared, but it's not significant: summary(xtabs(~Race.Descr+Sex+Marital.Status, data=borrowers))


# State
table(borrowers$PropertyState) # Not useful since we limit our geography...map it for effect
ensurePkg("maps")
stateMap <- map_data("state")
states <- data.frame(borrowers$PropertyState)
colnames(states) <- "abbrev"


# Create a theme with the background elements removed
themeClean <- function(base_size = 12) {
    require(grid) #Needed for unit() function
    theme_grey(base_size) %+replace%
        theme(
            axis.title = element_blank(),
            legend.position = "none", 
            axis.text = element_blank(),
            panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.ticks.length = unit(0, "cm"),
            axis.ticks.margin = unit(0, "cm"),
            panel.margin = unit(0, "lines"),
            plot.title = element_text(size=18, vjust=2), 
            plot.margin = unit(c(1, 0, 0, 0), "lines"),
            complete = TRUE
        )
}

png("borrower_property_states.png", width=1200, height=1200)
gMap <- ggplot(data=states, aes(map_id = abbrev))
gMap + geom_map(map=stateMap, color="black", aes(fill=count)) + 
    scale_fill_gradient2(low="white", mid="white", high="#00558c") + 
    expand_limits(x = county_map$long, y=county_map$lat) + 
    geom_text(data = regionCenter, aes(x = clong -0.5, y = clat, label = region, fill=NULL, map_id = NULL), size = 6, show_guide=F) + 
    coord_map("mercator") + 
    labs(title="Past Borrowers' Properties, by State") + 
    themeClean()
dev.off()



