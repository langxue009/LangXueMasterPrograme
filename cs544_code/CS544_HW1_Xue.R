
#part1(a)
scores <- c(59,46,76,60,49,65,82,68,99,52)

#How many students took the exam
length(scores)
#Using index

scores[1:2]
#Using indexing, show the expression for accessing the first two items
scores[c(1,length(scores))]

#Using indexing, show the expression for accessing the first and last items
scores[c(length(scores)/2, (length(scores)/2)+1)]




#part1(b)
#Use median(scores) to show the median of the data
median(scores)

#Using comparison operators, write the R expression for scores less than or equal to the median of the data. 
scores <= median(scores)

#Using comparison operators, write the R expression for scores greater than the median of the data
scores > median(scores)

#Using the sum function, write the R expression for the number of scores less than or equal to the median of the data
sum(scores <= median(scores))

#Using the sum function, write the R expression for the number of scores greater than the median of the data
sum(scores > median(scores))




#part1(c)
#write the R expression for all the scores that are less than or equal to the median value of the data
scores[scores <= median(scores)]

# write the R expression for all the scores that are greater than or equal to the median
scores[scores >= median(scores)]




#part1(d)
#write the R expression for all the scores that are less than or equal to the median value of the data
scores[rep(c(TRUE, FALSE), length(scores)/2)]

# write the R expression for the even indexed values from the scores
scores[rep(c(FALSE,TRUE), length(scores)/2)]




#part1(e)
#write the R expression for the odd indexed values from the scores
scores[seq(1,length(scores)-1, 2)]

#write the R expression for the even indexed values from the scores
scores[seq(2,length(scores), 2)]




#part1(f), Using the paste function with LETTERS, show the code for the following output
paste(LETTERS[1:length(scores)], "=", scores, sep = "")




#part1(g), Create a matrix of size 2 x 5 using the scores data
scores.matrix <- matrix(scores, nrow = 2, ncol = length(scores)/2, byrow = TRUE)
scores.matrix




#part1(h)
scores.matrix[,c(1,length(scores)/2)]




#part1(i)
rownames(scores.matrix) <- paste("Quiz_", 1:nrow(scores.matrix), sep = "")
colnames(scores.matrix) <- paste("Student_", 1:ncol(scores.matrix), sep = "")




#part1(j)
scores.matrix[,c(1,length(scores)/2)]






#part2(a)
dow <- data.frame(Month = c("Jan", "Feb", "Mar", "Apr", "May"),
                  Open = c(28639, 28320, 25591, 21227, 24121),
                  High = c(29374, 29569, 27102, 24765, 24350),
                  Low = c(28170, 24681, 18214, 20735, 23361),
                  Close = c(28256, 25409, 21917, 24346, 24331))
dow




#part2(b)
summary(dow[,-1])




#part2(c)
dow[,c("Month", "Open", "Close")]




#part2(d)
dow[c(1,nrow(dow)),]




#part2(e)
dow[c(1,nrow(dow)),c("Month", "High", "Low")]




#par2(f)
#logical indexing
dow[dow$Low > 22000,]
#subset function
subset(dow,dow$Low > 22000)




#part2(g)
#logical indexing
dow[dow$Open > 25000 & dow$Low > 25000,]
#subset function
subset(dow,dow$Open > 25000 & dow$Low > 25000)




#part2(h)
dow$Volatility <- dow$High - dow$Low
dow




#part2(i)
subset(dow, dow$Volatility == max(dow$Volatility))




#part2(j)
dow[dow$Volatility == min(dow$Volatility), ]















