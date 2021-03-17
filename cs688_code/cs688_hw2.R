library(ggplot2)

####################### Question 1

#install.packages("RISmed")
library(RISmed)
#help(package="RISmed")

terms <- c("influenza", "obesity", "cancer", "covid-19")
years <- 2016:2020

# create function to extract the frequency based on year and term
terms_freq = function(year, term){
  res <- EUtilsSummary(term, type="esearch", db="pubmed", datetype='pdat', mindate=year, maxdate=year, retmax=10)
  return(QueryCount(res))
}

# query from the E-utility servers
freq_counts <- lapply(terms, function(x) sapply(years, function(y) terms_freq(y, x)))
names(freq_counts) <- terms
freq_counts <- do.call("rbind", freq_counts)
colnames(freq_counts) <- 2016:2020

# plot area chart
freq_counts
time <- rep(2016:2020, 4)
frequency <- as.numeric(t(freq_counts))
term_group <- rep(terms, each = 5)
freq_dataframe <- data.frame(time, frequency, term_group)

ggplot(freq_dataframe, aes(x=time, y=frequency, fill=term_group)) + geom_area() + 
  ggtitle("Area Chart of the frequencies for terms Influenza, Obesity, Cancer, Covid-19")

####################### Question 2

terms <- c("influenza", "covid-19", "depression", "mental health", "physical activity", "wearable")
years <- 2019:2020

# query from the E-utility servers
freq_counts <- lapply(terms, function(x) sapply(years, function(y) terms_freq(y, x)))
names(freq_counts) <- terms
freq_counts <- do.call("rbind", freq_counts)
colnames(freq_counts) <- 2019:2020
freq_df <- data.frame(terms, freq_counts)
row.names(freq_df) <- NULL

#install.packages("ggalt")
library(ggalt)

ggplot(freq_df, aes(y=terms, x=X2019, xend=X2020)) + 
  geom_dumbbell(size = 1, colour_x = "red", colour_xend = "blue", ot_guide=TRUE, dot_guide_size=0.25) + 
  xlab("frequency") + 
  ggtitle("Dumbbell-chart of the keywords change from 2019 to 2020")

####################### Question 3

library(maps)
library(ggmap)

county <- c("Barnstable", "Berkshire", "Bristol", "Dukes", 
           "Essex", "Franklin", "Hampden", "Hampshire", "Middlesex", "Nantucket", 
           "Norfolk", "Plymouth", "Suffolk", "Worcester")
confirmed_cases <- c(502, 355, 1394, 12, 3413, 148, 1694, 224, 5983, 9, 2838, 
                     2141, 5579, 2128)

# obtain MA fips 
data(county.fips)
MA_counties <- paste("massachusetts", tolower(county), sep = ",")
MA_fips <- county.fips[county.fips$polyname %in% MA_counties,]

# create the map
colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#980043")
color_choice <- as.numeric(cut(confirmed_cases, c(0, 100, 500, 1000, 2000, 5000, 10000)))
map("county", "Massachusetts", col = colors[color_choice], fill = TRUE, resolution = 0, 
    lty = 0, projection = "polyconic")
title("Choropleth map to show the number of confirmed Covid-19 cases in Massachusetts")
leg.txt <- c("0-100", "101-500", "501-1000", "1001-2000", "2001-5000", ">5001")
legend("bottom", leg.txt, horiz = TRUE, fill = colors)

