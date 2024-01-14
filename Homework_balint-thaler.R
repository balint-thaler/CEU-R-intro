library(ggplot2)
library(data.table)
hotels <- readRDS(url('http://bit.ly/CEU-R-hotels-2018-merged'))
## How many hotels are from Austria? 
hotels[country == "Austria", .N]

## What is the rating of the most expensive hotel (based on the price per night)? 
hotels[avg_price_per_night == max(avg_price_per_night) & accommodation_type == "Hotel", rating]

## How many bookings are in 4-star hotels? 
hotels[stars == 4, sum(bookings)]

## Which country has the highest number of 5-star hotels? 
hotels[accommodation_type == "Hotel" & stars == 5, max(country)]

## Plot the number of bookings per country! 
ggplot(hotels, aes(x=country)) + geom_bar()

## Flip the coordinates and use the “classic dark-on-light theme”! 
ggplot(hotels, aes(y=country)) + geom_bar() + theme_light()

## Drop the Y axis title, and rename the X axis to “Number of hotels”! 
ggplot(hotels, aes(y=country)) + 
  geom_bar() + 
  theme_classic() +
  xlab("Number of hotels") + ylab("")

## Count the number of hotels per country! 
hotels[, .N, by = country]

## Order by alphabet! 
hotels[, .N, by = country][order(country)]

## Count the number of bookings per country, order by the number of bookings! 
hotels[, .(bookings = sum(bookings)), by = country][order(bookings)]


## Compute the average rating per number of stars! Use the weighted.mean function to account for 
## the number of ratings of the hotels, and experiment with the na.rm argument. Eliminate NAs. Order by stars. 
hotels[, .(rating = weighted.mean(x = rating, w = rating_count, na.rm = TRUE)), by = stars][order(stars)][!is.na(stars)]

## Plot this computed average rating per stars!  
rating_by_stars <- hotels[, .(rating = weighted.mean(x = rating, w = rating_count, na.rm = TRUE)), by = stars][order(stars)][!is.na(stars)]
ggplot(rating_by_stars, aes(x = stars, y = rating)) + geom_col()

## Make sure that each star category is printed on the X axis! 
ggplot(rating_by_stars, aes(x = as.factor(stars), y = rating)) + 
  geom_col() + 
  xlab("stars")

## Create a boxplot on ratings per stars! 
ggplot(hotels, aes(x = as.factor(stars), y = rating, group = stars)) + geom_boxplot() + xlab("stars")

## Create histograms on the nightly prices for each star category! 
## Check out the arguments and disable forcing the same Y axis range for the subplots.
ggplot(hotels, aes(x=avg_price_per_night)) + geom_histogram() + facet_wrap(~stars, scales="free")

       