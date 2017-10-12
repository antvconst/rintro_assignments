# read dataset with appropriate modes of DS's columns
chickdf = read.table(
  "dataset.txt", 
  sep=',',
  colClasses = c(weight="numeric", Time="numeric", Chick="factor", Diet="factor"))

# make %Chick% factor ordered
chickdf$Chick <- factor(
  chickdf$Chick, 
  levels=unique(chickdf$Chick),
  ordered=TRUE)

# split DF into subDFs containing measurements of a single chick
chickdf_split_by_chick <- split(chickdf, chickdf$Chick)

# function computing and inserting the %w_incr% variable
# describing the difference between two consecutive measurements
# we will leave w_incr of 0'th day measurement as NA as there's no meaningful way to define it
add_w_incr <- function(df) {
  df$w_incr[df$Time > 0] <- diff(df$weight)
  return(df)
}

# apply %add_w_incr% to all of the subDFs
chickdf_split_by_chick <- lapply(chickdf_split_by_chick, add_w_incr)

# combine all the subDFs back together and move the %w_incr% column
chickdf <- unsplit(chickdf_split_by_chick, chickdf$Chick)[, c(1, 5, 2, 3, 4)]

# let's define the criterion of chick's quality as its final weight (that is on day 21)
# for some reason not all measurements are completely present, so we will exclude the chicks
# that have not been measured on day 21 from consideration

# extract final measurements of all chicks
final_measurements <- chickdf[chickdf$Time == 21,]

# sort them by weight and extract top 15 chicks
final_measurements_top <- head(
  final_measurements[order(final_measurements$weight, decreasing=TRUE),],
  15)

# extract all measurements of this top 15 chicks
chickdf_top <- chickdf[chickdf$Chick %in% final_measurements_top$Chick,]

write.table(chickdf_top, "output_dataframe.txt", sep="\t")



# let's define the quality of a diet as the mean final weight of the chicks receiving it
get_mean_weight <- function(df) {
  return(mean(df$weight))
}

# unname is used to avoid strange double printing of which.max result
mean_final_weights <- unname(lapply(split(final_measurements,
                                          final_measurements$Diet), 
                                    get_mean_weight))

print(sprintf("Diet #%d seems to be the best overall judging by the mean final weight of chick", 
              which.max(mean_final_weights)))

# the short-term quality of a diet is defined as the mean weight of the chicks receiving it on day 10
day_ten_measurements <- chickdf[chickdf$Time == 10,]
mean_day_ten_weights <- unname(lapply(split(day_ten_measurements, 
                                            day_ten_measurements$Diet), 
                                      get_mean_weight))
print(sprintf("Diet #%d seems to be the best for short-term judging by the mean weight of chick on day 10",
      which.max(mean_day_ten_weights)))






