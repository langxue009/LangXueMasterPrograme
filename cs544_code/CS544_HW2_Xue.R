
############### Part 2

library(prob)
s <- rolldie(3, nside = 6, makespace = TRUE)

#a. the sum of the rolls is greater than 10

sum_die <- function(row){
  count <- 0
  for(x in row){
    count = count + x
  }
  return(count)
}

s1 <- addrv(s, FUN = sum_die, name = "sum")
s_sum <- marginal(s1, var = "sum")
s_sum[-(1:8),]

# P(The sum of the rolls is greater than 10) is 0.5
sum(s_sum[-(1:8),2]) 

#b. all the three rolls are identical
s <- rolldie(3, nside = 6, makespace = TRUE)

three_identical <- data.frame()

for(a in 1:216){
  if(identical(s[a,1],s[a,2]) && identical(s[a,2],s[a,3])){
    three_identical <- rbind(three_identical, s[a,])
  }
}
three_identical
sum(three_identical[,4])

#c. only two of the three rolls are identical

s <- rolldie(3, nside = 6, makespace = TRUE)

two_identical <- data.frame()

for(b in 1:216){
  if(identical(s[b,1],s[b,2]) | identical(s[b,1],s[b,3]) | identical(s[b,2],s[b,3])){
    two_identical <- rbind(two_identical, s[b,])
  }
}
dim(two_identical)

for(c in 1:96){
  if(identical(two_identical[c,1],two_identical[c,2]) && 
     identical(two_identical[c,2],two_identical[c,3])){
    two_identical <- two_identical[-c,]
  }
}

two_identical

# P(Only two of the three rolls are identical) = 0.4166667
sum(two_identical$probs)

#d.

none_identical <- s[!(s$X1 == s$X2) & (s$X1 == s$X3),]

none_identical

# P(None of the three rolls are identical) = 0.1388889

sum(none_identical$probs)

#e.

is_greater_than_10 <- apply(s[,1:3], 1, sum) > 10

# first, outcomes such that sum of the rolls is greater than 10
(greater_than_10 <- s[is_greater_than_10,])

cond1 <- (greater_than_10$X1 == greater_than_10$X2) | (greater_than_10$X1 == greater_than_10$X3) | (greater_than_10$X2 == greater_than_10$X3)
cond2 <- (greater_than_10$X1 == greater_than_10$X2) & (greater_than_10$X1 == greater_than_10$X3)
greater_than_10[cond1,]
greater_than_10[cond2,]

# then, the outcomes that only two of the three rolls are identical given that the sum of the rolls is greater than 10
outcome_e <- greater_than_10[cond1 & (!cond2),]

# P( the sum of the rolls is greater than 10) = 0.5
sum(greater_than_10$probs)

# P(Only two of the three rolls are identical & the sum of the rolls is greater than 10) = 0.2083333
sum(outcome_e$probs)

# P(Only two of the three rolls are identical given that the sum of the rolls is greater than 10) = 
# P(Only two of the three rolls are identical & the sum of the rolls is greater than 10)/P( the sum of the rolls is greater than 10) 
# = 0.4166667
sum(outcome_e$probs)/sum(greater_than_10$probs)

#f.

s$sum <- apply(s[,1:3], 1, sum)
sum_marginal <- marginal(s, var = "sum")
sum_marginal

# P(the sum of the rolls is greater than 10) = 0.5
sum(sum_marginal$probs[sum_marginal$sum > 10])

#g.

s$identical <- (s$X1 == s$X2) & (s$X1 == s$X3)
identical_marginal <- marginal(s, var = "identical")
identical_marginal

# P(all the rolls are identical) = 0.02777778

#h.

cond1 <- (s$X1 == s$X2) | (s$X1 == s$X3) | (s$X2 == s$X3) # at least two are identical
cond2 <- (s$X1 == s$X2) & (s$X1 == s$X3) # all three are identical
s$only_two_identical <- cond1 & (!cond2)

only_two_identical_marginal <- marginal(s, var = "only_two_identical")
only_two_identical_marginal

# P(only two out of the three are identical) = 0.4166667

#i.

s$none_identical <- !cond1
marginal(s, var = "none_identical")

# P(none of the three are identical) = 0.5555556

############### Part 3

# with for loop
sum_of_first_N_odd_squares <- function(n) {
  total_sum = 0
  for (i in 1:n) {
    total_sum = total_sum + (2 * i - 1) ^ 2
  }
  return(total_sum)
}

sum_of_first_N_odd_squares(2)
sum_of_first_N_odd_squares(5)
sum_of_first_N_odd_squares(10)

# without for loop
sum_of_first_N_odd_squares_V2 <- function(n) {
  return(sum(seq(1, 2*n-1, 2)^2))
}

sum_of_first_N_odd_squares_V2(2)
sum_of_first_N_odd_squares_V2(5)
sum_of_first_N_odd_squares_V2(10)

############### Part 4
dow <- read.csv("http://people.bu.edu/kalathur/datasets/DJI_2020.csv")
head(dow)

# 4a
sm <- summary(dow$Close)
names(sm) <- c("Min", "Q1", "Q2", "Mean", "Q3", "Max")
sm

sprintf("%s Quartile variation is %.1f",
       c('First', 'Second', 'Third', 'Fourth'), 
        diff(sm[-4]))

# 4b

min_value <- sm['Min']
min_loc <- which(dow$Close == min_value)
sprintf("The minimum Dow value of %.f is at row %d on %s", 
        min_value, min_loc, dow$Date[min_loc])

# 4c

max_value <- sm['Max']
max_loc <- which(dow$Close == max_value)
sprintf("The maximum Dow value of %.f is at row %d on %s", 
        max_value, max_loc, dow$Date[max_loc])

# 4d

# subset the data from the date we bought the fund
dow_subset <- dow[which.min(dow$Close):nrow(dow),]

entry_price <- dow_subset$Close[1]
sell_loc <- which.max(dow_subset$Close - entry_price)
sell_price <- dow_subset$Close[sell_loc]
gain <- 100*(sell_price-entry_price)/entry_price

sprintf("I would sell on %s when Dow is at %.f for a gain of %.2f%%", 
        dow_subset$Date[sell_loc], sell_price, gain)

# 4e

dow$DIFFS <- c(0, diff(dow$Close))
head(dow)

# 4f

sprintf("%d days Dow closed higher than previous day", sum(dow$DIFFS>0))
sprintf("%d days Dow closed lower than previous day", sum(dow$DIFFS<0))

# 4g

subset(dow, dow$DIFFS > 1000)

# 4f

subset(dow, dow$DIFFS < -1000)

# 4i

max_gain_loc <- which.max(dow$DIFFS)
max_gain <- dow$DIFFS[max_gain_loc]

sprintf("The maximum one-day Gain of %d (%d->%d) is at row %d on %s", 
        max_gain, 
        dow$Close[max_gain_loc-1], 
        dow$Close[max_gain_loc],
        max_gain_loc, 
        dow$Date[max_gain_loc])

# 4j

max_loss_loc <- which.min(dow$DIFFS)
max_loss <- -dow$DIFFS[max_loss_loc]

sprintf("The maximum one-day Loss of %d (%d->%d) is at row %d on %s", 
        max_loss, 
        dow$Close[max_loss_loc-1], 
        dow$Close[max_loss_loc], 
        max_loss_loc,
        dow$Date[max_loss_loc])

