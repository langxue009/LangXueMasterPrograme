library(png)
# #install.packages('imager')
# library(imager)
# #install.packages("magick")
# library("magick")

####### Definitions

# horizontal direction is x axis, it starts from 1, 2, 3, ... toward the right direction. 1 means pixel 1 in horizontal direction
# vertical direction is y axis, it starts from 1, 2, 3, ... downward. 1 means pixel 1 in vertical direction downward. 
# each image has 4 channels, the first three channels are for RGB, the last channel contains value 1 in all pixels, so we can ignore it
# in other words, for each pixel, we have number (r,g,b,1)

#### The idea of algorithm 1:

# Full search, need to search through (chicken_w-bug_w+1)*(chicken_h-bug_h+1) = 13892370 sub-images

#### The idea of algorithm 2:

# First, i want to find special sum of every pixel. The special sum of a pixel is created by suming r+g+b+1
# Then, i want to find within the bug image, which special sum occur the least amount of time. 
# I found that the special sum 1.67058823529412 occur 23 times in bug image. 

# Next, i calculate the special sum for every pixel in the chicken_with_bug image. 
# Using the seocond test image, I found 915 pixels have the special sum match 1.67058823529412

# Then, i'm going to put a window starting from these 915 pixels to create a sub-image from the chicken_with_bug image. 
# When all the values in all channels match with the bug image, then the algorithm will stop and 
# report back the x and y coordinates. 
# The for-loop in this case only need to search 915 sub-images. 


####### read bug and clean chicken, and insert bugs in 3 different locations

bug <- readPNG("Bug Final.png")
chicken_clean <- readPNG("Chickens clean.png")
insert_bug <- function(x, y,
                       chicken_clean_img = chicken_clean, bug_img = bug, 
                       output_filename = "chicken_with_bug_modified.png"){
  bug_h = dim(bug_img)[1] # bug image height
  bug_w = dim(bug_img)[2] # bug image width
  chicken_clean_img[y:(y+bug_h-1),x:(x+bug_w-1),1] = bug_img[,,1]
  chicken_clean_img[y:(y+bug_h-1),x:(x+bug_w-1),2] = bug_img[,,2]
  chicken_clean_img[y:(y+bug_h-1),x:(x+bug_w-1),3] = bug_img[,,3]
  writePNG(chicken_clean_img, target = output_filename)
}

insert_bug(1, 1, output_filename = "chicken_with_bug_test1.png")
insert_bug(1800, 1300, output_filename = "chicken_with_bug_test2.png")
insert_bug(3100, 1500, output_filename = "chicken_with_bug_test3.png")

####### 'algorithm 1' to search the position of the image

chicken_with_bug <- readPNG("chicken_with_bug_test1.png")
bug_h = dim(bug)[1] # bug image height
bug_w = dim(bug)[2] # bug image width
chicken_h = dim(chicken_with_bug)[1]
chicken_w = dim(chicken_with_bug)[2]

count = 0
for (x in c(1:(chicken_w-bug_w+1))){
  for (y in c(1:(chicken_h-bug_h+1))){
    count = count + 1
    if (all(chicken_with_bug[y:(y+bug_h-1),x:(x+bug_w-1),] == bug)){
      print(c(x,y))
    }
    if (count %% 1000 == 0){
      print(paste0("Already searched ", count, " windows."))
    }
  }
}

####### 'algorithm 2' to search the position of the image

chicken_with_bug <- readPNG("chicken_with_bug_test3.png")
bug_h = dim(bug)[1] # bug image height
bug_w = dim(bug)[2] # bug image width
chicken_h = dim(chicken_with_bug)[1] # chicken image height
chicken_w = dim(chicken_with_bug)[2] # chicken image height

# sum intensity of the bug image across the channels
bug_code <- apply(bug, c(1,2), function(x) sum(x))
bug_code_table <- table(bug_code)
bug_code_table_min <- bug_code_table[which.min(bug_code_table)]

# unique pixel sum which occur the least number of times in bug image (key)
special_sum = as.numeric(names(bug_code_table_min)) 
special_sum_coord_in_bug = which(round(bug_code, 6) == round(special_sum, 6), arr.ind = TRUE)
spec_row_index = special_sum_coord_in_bug[1,1]
spec_column_index = special_sum_coord_in_bug[1,2]

# sum intensity of the chicken_with_bug image across the channels
chicken_with_bug_code <- apply(chicken_with_bug, c(1,2), function(x) sum(x))

# coordinate in chicken_with_bug image that match the special sum
special_sum_coord_in_chicken = which(round(chicken_with_bug_code, 6) == round(special_sum, 6), arr.ind = TRUE)

# scope to search from the special pixel in the chicken_with_bug image
x_left_right_range = c(spec_column_index-1, bug_w-spec_column_index) # (left, right) from the special pixel
y_up_down_range = c(spec_row_index-1, bug_h-spec_row_index) # (up, down) from the special pixel

count = 0
for (i in 1:nrow(special_sum_coord_in_chicken)){
  
  # coordinate of the subimage in the chicken_with_bug image
  x_left = special_sum_coord_in_chicken[i,2]-x_left_right_range[1]
  x_right = special_sum_coord_in_chicken[i,2]+x_left_right_range[2]
  y_up = special_sum_coord_in_chicken[i,1]-y_up_down_range[1]
  y_down = special_sum_coord_in_chicken[i,1]+y_up_down_range[2]
  
  # check whether the sub_image is still completely covered by the big image
  in_range = (x_left >= 1) & (x_right <= chicken_w) & (y_up >= 1) & (y_down <= chicken_h)
  count = count + 1; print(count)
  
  # search through all the matched pixels
  if (in_range){
    sub_image <- chicken_with_bug[y_up:y_down, x_left:x_right, 1:3]
    if (all(sub_image == bug[,,1:3])){
      image_location = data.frame(x_coord = x_left, y_coord = y_up)
      break
    }
  }
  
}

image_location
