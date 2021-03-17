
# Part 1)

df <- read.csv("http://people.bu.edu/kalathur/datasets/myPrimes.csv")

# 1a)

library(ggplot2)
lastdigit_table <- table(df$LastDigit)
lastdigit_df <- data.frame(lastdigit = names(lastdigit_table), 
                              count = as.numeric(lastdigit_table))
lastdigit_df$lastdigit = as.character(lastdigit_df$lastdigit)
ggplot(data = lastdigit_df, aes(x = lastdigit, y = count)) + geom_bar(stat = "identity") +
  ggtitle("Barplot of the last digit")

# 1b)

firstdigit_table <- table(df$FirstDigit)
firstdigit_df <- data.frame(firstdigit = names(firstdigit_table), 
                           count = as.numeric(firstdigit_table))
firstdigit_df$firstdigit = as.character(firstdigit_df$firstdigit)
ggplot(data = firstdigit_df, aes(x = firstdigit, y = count)) + geom_bar(stat = "identity") +
  ggtitle("Barplot of the first digit")

# Part 2)

# 2a)

us_quarters <- read.csv("http://people.bu.edu/kalathur/datasets/us_quarters.csv")

# first, notice that the minimum or maximum number for each mint are unique. 
sort(us_quarters$DenverMint)[1:5]
sort(us_quarters$DenverMint, decreasing = TRUE)[1:5]
sort(us_quarters$PhillyMint)[1:5]
sort(us_quarters$PhillyMint, decreasing = TRUE)[1:5]

# DenverMint
us_quarters$State[which.max(us_quarters$DenverMint)] # Connecticut
us_quarters$State[which.min(us_quarters$DenverMint)] # Oklahoma

# PhillyMint
us_quarters$State[which.max(us_quarters$PhillyMint)] # Virginia
us_quarters$State[which.min(us_quarters$PhillyMint)] # Iowa

# 2b)

# times 1000 since the data is in thousand dollars
quarters_total <- apply(us_quarters[,-1], 2, sum)*1000*0.25

# 2c)

barplot(cbind(DenverMint, PhillyMint) ~ State, data = us_quarters, beside = TRUE, 
        col = c("red", "blue"), las = 2, cex.axis = 0.8, ylim = c(0, 1000000), 
        xlab = "", cex.names = 0.7)
legend('topright', names(us_quarters)[2:3], fill = c("red", "blue"))

options(digits=15)

# 2d)

with(us_quarters, plot(DenverMint, PhillyMint, 
                       main = "Scatter plot of the number of coins between two Mints"))

# 2e)

Mint_data <- data.frame(Mint = c(rep("Denver Mint", nrow(us_quarters)), 
                                 rep("Philly Mint", nrow(us_quarters))), 
                        Value = c(us_quarters$DenverMint, us_quarters$PhillyMint))
boxplot(Value ~ Mint, Mint_data, 
        main = "Boxplot of the number of quarters produced by each Mint")

# 2f)

Denver_fivenum <- fivenum(us_quarters$DenverMint)
Philly_fivenum <- fivenum(us_quarters$PhillyMint)

# Denver
Denver_bound = c(Denver_fivenum[2]-1.5*(Denver_fivenum[4]-Denver_fivenum[2]), 
                 Denver_fivenum[4]+1.5*(Denver_fivenum[4]-Denver_fivenum[2]))

# Philly
Philly_bound = c(Philly_fivenum[2]-1.5*(Philly_fivenum[4]-Philly_fivenum[2]), 
                 Philly_fivenum[4]+1.5*(Philly_fivenum[4]-Philly_fivenum[2]))

# find outliers
Denver_isOutlier <- (us_quarters$DenverMint < Denver_bound[1]) | (us_quarters$DenverMint > Denver_bound[2])
Philly_isOutlier <- (us_quarters$PhillyMint < Philly_bound[1]) | (us_quarters$PhillyMint > Philly_bound[2])

us_quarters$State[Denver_isOutlier]
us_quarters$State[Philly_isOutlier]

## Part 3

x <- cbind(c(25,20), c(10, 40), c(15, 30))
rownames(x) <- c("Men", "Women")
colnames(x) <- c("NFL", "NBA", "NHL")
dimnames(x) <- list(Gender = rownames(x), Sport = colnames(x))

# 3a)

n = sum(x)

# marginal distribution
margin.table(x, margin = 1)
apply(x, 1, sum) # for Gender

margin.table(x, margin = 2)
apply(x, 2, sum) # for Sport

# 3b)

addmargins(x, margin = c(1,2))

# 3c)

# marginal distribution
apply(x, 1, sum)/n # for Gender
apply(x, 2, sum)/n # for Sport

# conditional proportion
prop.table(x, margin = 1)
prop.table(x, margin = 2)

# 3d)

mosaicplot(x, color = heat.colors(3), 
           main = "MosaicPlot of Sport vs Gender")

barplot(x, beside = TRUE, col = c("blue", "red"), ylab = "count", 
        main = "Barplot of Gender separate by Sport Type")
legend("topleft", c("Men", "Women"), fill = c("blue", "red"))

## Part 4

# 4a)

stocks <- read.csv("http://people.bu.edu/kalathur/datasets/faang.csv")
pairs(stocks[,-1])

# 4b)

cor(stocks[,-1])

# 5a)

scores <- read.csv("http://people.bu.edu/kalathur/datasets/scores.csv")
h <- hist(scores$Score, xlab = "Score", main = "Histogram of Student Score")

counts <- h$counts
breaks <- h$breaks
for (i in 1:length(counts)){
  print(paste0(counts[i], " students in range (", breaks[i], ",", breaks[i+1], "]"))
}

# 5b

A_count = sum(counts[which(breaks == 70):which(breaks == 85)], na.rm = TRUE)
B_count = sum(counts[which(breaks == 50):which(breaks == 65)], na.rm = TRUE)
C_count = sum(counts[breaks > 30 & breaks <= 45])

print(paste0(C_count, " students in C grade range (30,50]"))
print(paste0(B_count, " students in B grade range (50,70]"))
print(paste0(A_count, " students in A grade range (70,90]"))

