# read dataset with appropriate modes of DS's columns
chickdf = read.table(
  "dataset.txt", 
  sep=',',
  colClasses = c(weight="numeric", Time="numeric", Chick="factor", Diet="factor"))

# split DF into subDFs containing measurements of a single chick
chickdf_split_by_chick <- split(chickdf, chickdf$Chick)

# for each subDF we add w_incr column
# we will leave w_incr of 0'th day measurement as NA as there's no meaningful way to define it
chickdf_split_by_chick <- lapply(chickdf_split_by_chick, function(df){transform(df, w_incr = c(NA, diff(weight)))})

# combine all the subDFs back together
chickdf <- unsplit(chickdf_split_by_chick, chickdf$Chick)

# we define the criterion of chick's quality as its final weight (that is on day 21)
# for some reason not all measurements are completely present, so we will exclude the chicks
# that have not been measured on day 21 from consideration

# extract final measurements of all chicks
final_measurements <- subset(chickdf, Time == 21)

# sort them by weight and extract top 15 chicks
final_measurements_top <- final_measurements[head(order(final_measurements$weight, decreasing=TRUE), 15), ]

# extract all measurements of this top 15 chicks
chickdf_top <- subset(chickdf, Chick %in% final_measurements_top$Chick)

write.table(chickdf_top, "output_dataframe.txt", sep="\t")


# we define the quality of a diet as the mean final weight of the chicks receiving it
mean_final_weights <- aggregate(final_measurements$weight, by=list(final_measurements$Diet), mean)

print(sprintf("Diet #%d seems to be the best overall judging by the mean final weight of chick", 
              which.max(mean_final_weights$x)))

# the short-term quality of a diet is defined as the mean weight of the chicks receiving it on day 10
day_ten_measurements <- subset(chickdf, Time == 10)
mean_day_ten_weights <- aggregate(day_ten_measurements$weight, by=list(day_ten_measurements$Diet), mean)

print(sprintf("Diet #%d seems to be the best for short-term judging by the mean weight of chick on day 10",
      which.max(mean_day_ten_weights$x)))






