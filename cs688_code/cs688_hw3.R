
######### Part 1

#install.packages('ndjson') # for stream_in()
library(ndjson)

# indiegogo_keywords_freq_extractor: 
# input: 
# gz_file is the path to a gz_file
# keywords is a vector contains all the desired keywords
indiegogo_keywords_freq_extractor <- function(gz_file, keywords){

  # obtain the year and month information from the gz path 
  year <- as.numeric(gsub(".*([0-9]{4}).*", "\\1", gz_file))
  month <- month.abb[as.numeric(gsub(".*-([0-9]{2})-.*", "\\1", gz_file))] # use month.abb change it to month abbreviation
  
  # read data from json
  indiegogo_json <- stream_in(gz_file)
  # element name for category before 2017 is "category_name"
  # 2018 and after, it changes to "category"
  if (year <= 2017) {
    categories <- indiegogo_json$data.category_name
  } else {
    categories <- indiegogo_json$data.category
  }
  
  # count the frequencies for each keyword
  freq <- sapply(keywords, function(x)
    sum(categories == x))
  
  # put everything in a dataframe
  out <- data.frame(keywords, year, month, freq = as.numeric(freq))
  return(out)
  
}

# get all gz_file names
gz_files <- list.files(pattern = "*.gz")
keywords <- c("Education", "Energy & Green Tech", "Health & Fitness", "Fashion & Wearables", "Wellness")
keywords_frequency_list <- lapply(gz_files, function(x) indiegogo_keywords_freq_extractor(x, keywords))
keywords_frequency <- do.call("rbind", keywords_frequency_list)
keywords_frequency$year <- factor(keywords_frequency$year)

library(dplyr)
library(ggplot2)

# density plot for each keyword
keywords_frequency %>% filter(keywords == "Education") %>%
  ggplot() + geom_density(aes(x = freq, group = year, fill = year), alpha = 0.5) + 
  ylim(0, 0.001) + ggtitle("Distribution of the keywords 'Education' by year")

keywords_frequency %>% filter(keywords == "Energy & Green Tech") %>%
  ggplot() + geom_density(aes(x = freq, group = year, fill = year), alpha = 0.5) + 
  ylim(0, 0.0075) + ggtitle("Distribution of the keywords 'Energy & Green Tech' by year")

keywords_frequency %>% filter(keywords == "Health & Fitness") %>%
  ggplot() + geom_density(aes(x = freq, group = year, fill = year), alpha = 0.5) + 
  ylim(0, 0.002) + ggtitle("Distribution of the keywords 'Health & Fitness' by year")

keywords_frequency %>% filter(keywords == "Fashion & Wearables") %>%
  ggplot() + geom_density(aes(x = freq, group = year, fill = year), alpha = 0.5) + 
  ylim(0, 0.002) + ggtitle("Distribution of the keywords 'Fashion & Wearables' by year")

keywords_frequency %>% filter(keywords == "Wellness") %>%
  ggplot() + geom_density(aes(x = freq, group = year, fill = year), alpha = 0.5) + 
  ylim(0, 0.005) + ggtitle("Distribution of the keywords 'Wellness' by year")

# wrap density plot (all together)
ggplot(keywords_frequency) + geom_density(aes(x = freq, group = year, fill = year), alpha = 0.5) +
  facet_wrap(~keywords, scales = 'free') + ylim(0, 0.0075)

# wrap density plot (separate by two groups)
keywords_frequency %>% filter(keywords %in% c("Energy & Green Tech", "Health & Fitness")) %>%
ggplot() + geom_density(aes(x = freq, group = year, fill = year), alpha = 0.5) +
  facet_wrap(~keywords, scales = 'free') + ylim(0, 0.0075)

keywords_frequency %>% filter(!keywords %in% c("Energy & Green Tech", "Health & Fitness")) %>%
  ggplot() + geom_density(aes(x = freq, group = year, fill = year), alpha = 0.5) +
  facet_wrap(~keywords, scales = 'free') + ylim(0, 0.0045)

######### Part 2a

data_part2 <- keywords_frequency %>% 
  filter(keywords %in% c("Health & Fitness", "Fashion & Wearables")) %>%
  filter(year %in% c("2018", "2019", "2020"))
health <- data_part2 %>% filter(keywords == "Health & Fitness")
fashion <- data_part2 %>% filter(keywords == "Fashion & Wearables")

#### parametric test: 
# paired t-test to compare the mean frequency between the two groups
t.test(health$freq, fashion$freq, paired = TRUE)

#### non-parametric test 1:   
# Mann-Whitney U Test / Wilcoxon signed rank test
wilcox.test(health$freq, fashion$freq, paired = TRUE)

#### non-parametric test 2:   
# Kolmogorov-Smirnov Test (ignore the year/time effect)
ks.test(health$freq, fashion$freq)
ggplot(data_part2) + geom_density(aes(x = freq, group = factor(keywords), fill = factor(keywords)), alpha = 0.5) + 
  ggtitle("Density plot for 'Fashion & Wearables' and 'Health & Fitness', ignore the time effect")

######### Part 2b

#install.packages("effsize")
library(effsize)

res = cohen.d(health$freq, fashion$freq, paired = TRUE, return.dm = TRUE)
print(res)

######### Part 3

cor.test(health$freq, fashion$freq, method = 'pearson')
cor.test(health$freq, fashion$freq, method = 'spearman')
cor.test(health$freq, fashion$freq, method = 'kendall')



