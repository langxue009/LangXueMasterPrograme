
# HW5

# 1a

boston <- read.csv( "http://people.bu.edu/kalathur/datasets/bostonCityEarnings.csv", 
                    colClasses = c("character", "character", "character", "integer", "character"))
library(ggplot2)
ggplot(boston, aes(x = Earnings)) + geom_histogram() + 
   scale_x_continuous(breaks = seq(40000, 400000, 20000), lim = c(40000, 400000)) + 
   ggtitle("Histogram of the Employee Earnings in Boston")

mean(boston$Earnings)
sd(boston$Earnings)

# 1b

set.seed(2020)
earnings = boston$Earnings
xbars = sapply(1:5000, function(x) mean(sample(earnings, size = 10)))
ggplot(data.frame(xbars), aes(x = xbars)) + geom_histogram() + 
   xlab("sample means of earnings") + 
   ggtitle("Histogram of the earnings sample means from 5000 samples of this data of size 10")
mean(xbars) # 108739.9
sd(xbars) # 15690.47

# 1c

set.seed(2020)
earnings = boston$Earnings
xbars = sapply(1:5000, function(x) mean(sample(earnings, size = 40)))
ggplot(data.frame(xbars), aes(x = xbars)) + geom_histogram() + 
   xlab("sample means of earnings") + 
   ggtitle("Histogram of the earnings sample means from 5000 samples of this data of size 40")
mean(xbars) # 108723.5
sd(xbars) # 7924.661

# 2a

set.seed(2020)
x = rnbinom(5000, size = 3, prob = 0.5)
barplot(prop.table(table(x)), xlab = "distinct values from negative binomial distribution", 
        main = "Distribution of the negative binomial distribution with size = 3 and prob = 0.5",
        ylab = "proportions")

# 2b

set.seed(1234)
n_size = seq(10,40,10)
x_resample = lapply(n_size, function(n) sapply(1:1000, function(y) mean(sample(x, size = n))))

par(mfrow = c(2,2))
for (i in 1:4){
   hist(x_resample[[i]], density = TRUE, probability = TRUE, 
        xlab = "sample mean", xlim = c(0, 6), 
        ylim = c(0, 1.1),
        main = paste("Histogram of the sample means from \n1000 samples in 2a with size", n_size[i]))
}

# 2c

mean(x); sd(x)
lapply(x_resample, function(x) c(mean(x), sd(x)))

# 3a

# top five department 
dep_names = names(rev(tail(sort(table(boston$Department)),5)))

# subset 
small_boston <- boston[boston$Department %in% dep_names, ]

# random sampling
library(sampling)
set.seed(4812)
sample_3a = small_boston[srswor(50, nrow(small_boston)) == 1,]

# frequencies
table(sample_3a$Department)

# percentages
prop.table(table(sample_3a$Department))

# 3b

N = nrow(small_boston)
n = 50
k <- ceiling(N/n)
k # 117

r <- sample(k, 1)
r # 111

s <- seq(r, by = k, length = n)
s[s>N] <- s[s > N]-N

sample_3b = small_boston[s,]

# frequencies
table(sample_3b$Department)

# percentages
prop.table(table(sample_3b$Department))

# 3c

# Calculate the inclusion probabilities using the Earnings variable
pik <- inclusionprobabilities(small_boston$Earnings, n)
s <- UPsystematic(pik)
sample_3c = small_boston[s == 1,]

# frequencies
table(sample_3c$Department)

# percentages
prop.table(table(sample_3c$Department))

# 3d

small_boston <- small_boston[order(small_boston$Department),]
strata_size <- round(round(prop.table(table(small_boston$Department))*n,1))
st.boston <- strata(small_boston, stratanames = "Department", size = strata_size, method = "srswor")
sample_3d <- getdata(small_boston, st.boston)

# frequencies
table(sample_3d$Department)

# percentages
prop.table(table(sample_3d$Department))

# 3e

mean(small_boston$Earnings)
mean(sample_3a$Earnings)
mean(sample_3b$Earnings)
mean(sample_3c$Earnings)
mean(sample_3d$Earnings)




