library(tidyr)
setwd("/Users/jackpark/Desktop") #set your directory
wishpond <- read.csv("Paays July 10th.csv")
wishpond$finance_range <- wishpond$Whatis.The.Desired.Amount.You.Would.Want.To.Finance.
wishpond$vacation_budget <- wishpond$How.much.do.you.typically.spend.on.a.vacation.package.
wishpond$leisure_travel_type <- wishpond$What.does.your.dream.vacation.look.like...select.all.that.apply.
#there are   options in regards to what the finance range can be, so I can just replace
#each option with whatever is the average of its range and for $10,000+ I'll just set the value to 10000
ave.finance_range <- function(wishpond, finance_range){
  wishpond <- separate(wishpond, finance_range, c("low_finance_range", "high_finance_range"), sep = "-", remove = TRUE, convert = FALSE, extra = "warn", fill = "warn")
  #separates the finance_ranged values by the - in between them into two separate columns
  wishpond$low_finance_range <- gsub("\\$", "", wishpond$low_finance_range)
  wishpond$low_finance_range <- gsub("\\,", "", wishpond$low_finance_range)
  wishpond$low_finance_range <- gsub("\\+", "", wishpond$low_finance_range)
  wishpond$high_finance_range <- gsub("\\$", "", wishpond$high_finance_range)
  wishpond$high_finance_range <- gsub("\\,", "", wishpond$high_finance_range)
  #removes all symbols in the cells that prevent the values to be read as numbers
  wishpond$low_finance_range <- type.convert(wishpond$low_finance_range, na.strings = "NA", dec=".", numerals = c("no.loss"))
  wishpond$high_finance_range <- type.convert(wishpond$high_finance_range, na.strings = "NA", dec=".", numerals = c("no.loss"))
  #converts the type to numbers that can be used in calculations
  for (i in 1:nrow(wishpond)) {
    if(!is.na(wishpond$low_finance_range[i])){
      if(!is.na(wishpond$high_finance_range[i])){
        wishpond$ave_finance_range[i] <- with(wishpond, (low_finance_range[i]+high_finance_range[i])/2)
        #if both low and high finance_ranges aren't NA then the average is done by averaging the two
      }else{
        wishpond$ave_finance_range[i] <- wishpond$low_finance_range[i]
        #if only high is NA then that means that the value was $X+ so the average is equal to the low finance_range value
      }
    }else{
      wishpond$ave_finance_range[i] <- "NA"
      #if both are NA then the average is also NA
    }
  }
}
ave.vacation_budget <- function(wishpond, vacation_budget){
  wishpond <- separate(wishpond, vacation_budget, c("low_vacation_budget", "high_vacation_budget"), sep = "to", remove = TRUE, convert = FALSE, extra = "warn", fill = "warn")
  #separates the vacation_budgetd values by the - in between them into two separate columns
  wishpond$low_vacation_budget <- gsub("\\$", "", wishpond$low_vacation_budget)
  wishpond$low_vacation_budget <- gsub("\\,", "", wishpond$low_vacation_budget)
  wishpond$low_vacation_budget <- gsub("[<]", "", wishpond$low_vacation_budget)
  wishpond$low_vacation_budget <- gsub("\\+", "", wishpond$low_vacation_budget)
  wishpond$high_vacation_budget <- gsub("\\$", "", wishpond$high_vacation_budget)
  wishpond$high_vacation_budget <- gsub("\\,", "", wishpond$high_vacation_budget)
  #removes all symbols in the cells that prevent the values to be read as numbers
  wishpond$low_vacation_budget <- type.convert(wishpond$low_vacation_budget, na.strings = "NA", dec=".", numerals = c("no.loss"))
  wishpond$high_vacation_budget <- type.convert(wishpond$high_vacation_budget, na.strings = "NA", dec=".", numerals = c("no.loss"))
  #converts the type to numbers that can be used in calculations
  for (i in 1:nrow(wishpond)) {
    if(!is.na(wishpond$low_vacation_budget[i])){
      if(!is.na(wishpond$high_vacation_budget[i])){
        wishpond$ave_vacation_budget[i] <- with(wishpond, (low_vacation_budget[i]+high_vacation_budget[i])/2)
        #if both low and high vacation_budgets aren't NA then the average is done by averaging the two
      }else{
        wishpond$ave_vacation_budget[i] <- wishpond$low_vacation_budget[i]
        #if only high is NA then that means that the value was <$X or $X+ so the average is equal to the low vacation_budget value
      }
    }else{
      wishpond$ave_vacation_budget[i] <- "NA"
      #if both are NA then the average is also NA
    }
  }
}
wishpond$age_range <- wishpond$Please.Tell.Us.Your.Age
ave.age_range <- function(wishpond, age_range){
  wishpond <- separate(wishpond, age_range, c("low_age_range", "high_age_range"), sep = "-", remove = TRUE, convert = FALSE, extra = "warn", fill = "warn")
  #separates the age_ranged values by the - in between them into two separate columns
  wishpond$low_age_range <- gsub(" years or older", "", wishpond$low_age_range)
  wishpond$high_age_range <- gsub(" years old", "", wishpond$high_age_range)
  #removes all symbols in the cells that prevent the values to be read as numbers
  wishpond$low_age_range <- type.convert(wishpond$low_age_range, na.strings = "NA", dec=".", numerals = c("no.loss"))
  wishpond$high_age_range <- type.convert(wishpond$high_age_range, na.strings = "NA", dec=".", numerals = c("no.loss"))
  #converts the type to numbers that can be used in calculations
  for (i in 1:nrow(wishpond)) {
    if(!is.na(wishpond$low_age_range[i])){
      if(!is.na(wishpond$high_age_range[i])){
        wishpond$ave_age_range[i] <- with(wishpond, (low_age_range[i]+high_age_range[i])/2)
        #if both low and high age_ranges aren't NA then the average is done by averaging the two
      }else{
        wishpond$ave_age_range[i] <- wishpond$low_age_range[i]
        #if only high is NA then that means that the value was X+ so the average is equal to the low age_range value
      }
    }else{
      wishpond$ave_age_range[i] <- "NA"
      #if both are NA then the average is also NA
    }
  }
}
#Splitting up the leisure_travel_type into different columns that give either 1 or 0 based on if the user 
#selected that value, so 1 if they selected that option and 0 if not
wishpond$leisure_travel_type_Beach <- 0
wishpond$leisure_travel_type_Sightseeing_Touring <- 0
wishpond$leisure_travel_type_Trekking <- 0
wishpond$leisure_travel_type_Skiing_Golfing_Biking <- 0
wishpond$leisure_travel_type_Other <- 0
for(l in 1:nrow(wishpond)){
  if(grepl("Beach", wishpond$leisure_travel_type[l], fixed = TRUE)){
    wishpond$leisure_travel_type_Beach[l] <- 1
  }
  if(grepl("Sightseeing", wishpond$leisure_travel_type[l], fixed = TRUE)){
    wishpond$leisure_travel_type_Sightseeing_Touring[l] <- 1
  }
  if(grepl("Trekking", wishpond$leisure_travel_type[l], fixed = TRUE)){
    wishpond$leisure_travel_type_Trekking[l] <- 1
  }
  if(grepl("Golfing", wishpond$leisure_travel_type[l], fixed = TRUE)){
    wishpond$leisure_travel_type_Skiing_Golfing_Biking[l] <- 1
  }
  if(grepl("Other", wishpond$leisure_travel_type[l], fixed = TRUE)){
    wishpond$leisure_travel_type_Other[l] <- 1
  }
}
library(scales)
clean_data <- subset(wishpond, select = c("Email", "leisure_travel_type_Beach", "leisure_travel_type_Sightseeing_Touring", "leisure_travel_type_Trekking", "leisure_travel_type_Skiing_Golfing_Biking", "leisure_travel_type_Other", "ave_vacation_budget", "ave_finance_range", "ave_age_range"))
clean_data$ave_vacation_budget <- type.convert(clean_data$ave_vacation_budget, na.strings = "NA", dec=".", numerals = c("no.loss"))
clean_data$ave_finance_range <- type.convert(clean_data$ave_finance_range, na.strings = "NA", dec=".", numerals = c("no.loss"))
clean_data$ave_age_range <- type.convert(clean_data$ave_age_range, na.strings = "NA", dec=".", numerals = c("no.loss"))
clean_data <- subset(clean_data, !is.na(ave_vacation_budget))
clean_data <- subset(clean_data, !is.na(ave_finance_range))
clean_data <- subset(clean_data, !is.na(ave_age_range))
#limiting data to the pertinent columns and getting rid of rows with limited data
Leisure_Options <- c("Beach", "Sightseeing", "Trekking", "Skiing", "Other")
BudgetOptions <- c(1000, 1500, 3500, 7500, 10000)
data.split <- function(BudgetOptions, n){
  for(j in 1:n){
    bud <- BudgetOptions[j]
    y <- paste("budget", bud, sep = "", collapse = NULL)
    assign(x=y, value = subset(clean_data, ave_vacation_budget==bud))
    for(i in 1:5){
      z <- paste("budget", bud, "_", Leisure_Options[i], sep = "", collapse = NULL)
      b <- paste("leisure_travel_type_", Leisure_Options[i], sep ="", collapse = NULL)
      c <- subset(clean_data, ave_vacation_budget==bud)
      c <- c[i+1]
      assign(x=z, value = sum(c)/nrow(c))
    }
  }
}
budget5000plus <- subset(clean_data, ave_vacation_budget>=5000)
#split the data into each different price range to compare all of the ranges
for (k in 1:5) {
  z <- paste("budget5000plus_", Leisure_Options[k], sep = "", collapse = NULL)
  c <- budget5000plus[k+1]
  assign(x=z, value = sum(c)/nrow(c))
}
for (m in 1:5) {
  z <- paste("total_budget_", Leisure_Options[m], sep = "", collapse = NULL)
  c <- clean_data[m+1]
  assign(x=z, value = sum(c)/nrow(c))
}
#find the percentage chance that each option of leisure travel was selected for the budget groups and for the group as a whole
Groups <- c(rep("<$1,000", 5), rep("$1,000-$2,000", 5), rep("$2,000-$5,000", 5), rep("$5,000-$10,000", 5), rep("$10,000+", 5), rep("$5,000+", 5), rep("Everyone", 5))
Occurances <- c(nrow(budget1000), nrow(budget1500), nrow(budget3500), nrow(budget7500), nrow(budget10000), nrow(budget5000plus), nrow(clean_data))
Leisure_Activities <- rep(c("Beach", "Sightseeing and Touring", "Trekking", "Skiing, Golfing, Biking, etc...", "Other"), 7)
Percentage_Wanted <- c(budget1000_Beach, budget1000_Sightseeing, budget1000_Trekking, budget1000_Skiing, budget1000_Other, budget1500_Beach, budget1500_Sightseeing, budget1500_Trekking, budget1500_Skiing, budget1500_Other, budget3500_Beach, budget3500_Sightseeing, budget3500_Trekking, budget3500_Skiing, budget3500_Other, budget7500_Beach, budget7500_Sightseeing, budget7500_Trekking, budget7500_Skiing, budget7500_Other, budget10000_Beach, budget10000_Sightseeing, budget10000_Trekking, budget10000_Skiing, budget10000_Other, budget5000plus_Beach, budget5000plus_Sightseeing, budget5000plus_Trekking, budget5000plus_Skiing, budget5000plus_Other, total_budget_Beach, total_budget_Sightseeing, total_budget_Trekking, total_budget_Skiing, total_budget_Other)
budgetcomparison <- data.frame(Groups, Leisure_Activities, Percentage_Wanted)
Groups2 <- factor(Groups, levels = c("<$1,000", "$1,000-$2,000", "$2,000-$5,000", "$5,000-$10,000", "$5,000+", "$10,000+", "Everyone"))
library(ggplot2)
ggplot(budgetcomparison, aes(fill = Leisure_Activities, y = Percentage_Wanted, x = Groups2)) + geom_bar(position = "dodge", stat = "identity") + ggtitle("Budget Groups Preferred Leisure Activities") + labs(x = "Budget Groups", y = "Percentage of Users Preferred") + theme(legend.title = element_blank()) + scale_y_continuous(limits = c(0,1), expand = c(0,0), labels = percent)
Age_Options <- unique(clean_data$ave_age_range, incomparables = FALSE)
Age_Options <- sort(Age_Options, decreasing = FALSE)
freq1000 <- as.data.frame(table(budget1000$ave_age_range))
freq1500 <- as.data.frame(table(budget1500$ave_age_range))
freq3500 <- as.data.frame(table(budget3500$ave_age_range))
freq7500 <- as.data.frame(table(budget7500$ave_age_range))
freq10000 <- as.data.frame(table(budget10000$ave_age_range))
freq_total <- as.data.frame(table(clean_data$ave_age_range))
#will have to be changed with more data because there may no longer be frequencies of 0 for some
Frequencies <- c(freq1000$Freq[1], freq1000$Freq[2], freq1000$Freq[3], freq1000$Freq[4], freq1000$Freq[5], freq1000$Freq[6], freq1000$Freq[7], freq1500$Freq[1], freq1500$Freq[2], freq1500$Freq[3], freq1500$Freq[4], freq1500$Freq[5], freq1500$Freq[6], freq1500$Freq[7], freq3500$Freq[1], freq3500$Freq[2], freq3500$Freq[3], freq3500$Freq[4], freq3500$Freq[5], freq3500$Freq[6], freq3500$Freq[7], freq7500$Freq[1], freq7500$Freq[2], freq7500$Freq[3], freq7500$Freq[4], freq7500$Freq[5], freq7500$Freq[6], 0, freq10000$Freq[1], 0, freq10000$Freq[2], freq10000$Freq[3], freq10000$Freq[4], freq10000$Freq[5], 0, freq_total$Freq[1], freq_total$Freq[2], freq_total$Freq[3], freq_total$Freq[4], freq_total$Freq[5], freq_total$Freq[6], freq_total$Freq[7])
Budget <- c(rep("<$1,000", 7), rep("$1,000-$2,000", 7), rep("$2,000-$5,000", 7), rep("$5,000-$10,000", 7), rep("$10,000+", 7), rep("Everyone", 7))
Ages <- rep(Age_Options, 6)
AgeVaca <- data.frame(Ages, Budget, Frequencies)
AgeVaca$Budget2 <- factor(AgeVaca$Budget, levels = c("<$1,000", "$1,000-$2,000", "$2,000-$5,000", "$5,000-$10,000", "$10,000+", "Everyone"))
ggplot(AgeVaca, aes(fill = Ages, y = Frequencies, x = Budget2)) + geom_bar(position = "stack", stat = "identity") + ggtitle("Budget Groups by Age") + labs(x = "Budget Groups", y = "Number of Users")
Age_Possibilities <- c(21, 29.5, 39.5, 49.5, 59.5, 69.5, 75)
data.split <- function(Age_Possibilities, n){
  for(j in 1:7){
    age <- Age_Possibilities[j]
    y <- paste("age", age, sep = "", collapse = NULL)
    assign(x=y, value = subset(clean_data, ave_age_range==age))
    for(i in 1:5){
      z <- paste("age", age, "_", Leisure_Options[i], sep = "", collapse = NULL)
      b <- paste("leisure_travel_type_", Leisure_Options[i], sep ="", collapse = NULL)
      c <- subset(clean_data, ave_age_range==age)
      c <- c[i+1]
      assign(x=z, value = sum(c)/nrow(c)*100)
    }
  }
}
Age_Beach <- c(age21_Beach, age29.5_Beach, age39.5_Beach, age49.5_Beach, age59.5_Beach, age69.5_Beach, age75_Beach)
Age_Sightseeing <- c(age21_Sightseeing, age29.5_Sightseeing, age39.5_Sightseeing, age49.5_Sightseeing, age59.5_Sightseeing, age69.5_Sightseeing, age75_Sightseeing)
Age_Trekking <- c(age21_Trekking, age29.5_Trekking, age39.5_Trekking, age49.5_Trekking, age59.5_Trekking, age69.5_Trekking, age75_Trekking)
Age_Skiing <- c(age21_Skiing, age29.5_Skiing, age39.5_Skiing, age49.5_Skiing, age59.5_Skiing, age69.5_Skiing, age75_Skiing)
Age_Other <- c(age21_Other, age29.5_Other, age39.5_Other, age49.5_Other, age59.5_Other, age69.5_Other, age75_Other)
Age_Data <- data.frame(Age_Possibilities, Age_Beach, Age_Sightseeing, Age_Trekking, Age_Skiing, Age_Other)
plot(type = "l", x = Age_Data$Age_Possibilities, y = Age_Data$Age_Beach, main = "Leisure Types by Age", xlab = "Age", ylab = "Preferred Leisure Type (%)", xlim = c(18, 80), ylim = c(0, 100), col = "red", lwd = 2) 
lines(x = Age_Data$Age_Possibilities, y = Age_Data$Age_Sightseeing, type = "l", col = "yellow", lwd = 2)
lines(x = Age_Data$Age_Possibilities, y = Age_Data$Age_Trekking, type = "l", col = "green", lwd = 2)
lines(x = Age_Data$Age_Possibilities, y = Age_Data$Age_Skiing, type = "l", col = "blue", lwd = 2)
lines(x = Age_Data$Age_Possibilities, y = Age_Data$Age_Other, type = "l", col = "purple", lwd = 2)
legend("topright", legend = c("Beach", "Sightseeing and Touring", "Trekking", "Skiing, Golfing, Biking, etc...", "Other"), col = c("red", "yellow", "green", "blue", "purple"), lty = 1, cex = 0.6, title = "Leisure Types", text.font = 4, box.lty = 1, lwd = 2, bg = "light blue", box.col = "light blue")
#split data up by likelihoods
wishpond$likelihood <- wishpond$If.you.could.finance.your.vacation.over.12.months..how.likely.are.you.to.do.so.
data_2 <- subset(wishpond, likelihood != "")
#subsetted so that no one who didn't answer the question is included
data_2$unlikely <- 0
data_2$more_likely_not <- 0
data_2$likely <- 0
data_2$very_likely <- 0
for(p in 1:nrow(data_2)){
  if(grepl("Unlikely", data_2$likelihood[p], fixed = TRUE)){
    data_2$unlikely[p] <- 1
  }else{
    if(grepl("More likely not", data_2$likelihood[p], fixed = TRUE)){
      data_2$more_likely_not[p] <- 1
    }else{
      if(grepl("Likely", data_2$likelihood[p], fixed = TRUE)){
        data_2$likely[p] <- 1
      }else{
        if(grepl("Very likely", data_2$likelihood[p], fixed = TRUE)){
          data_2$very_likely[p] <- 1
        }
      }
    }
  }
}
chance_unlikely <- round(with(data_2, sum(unlikely)/nrow(data_2))*100, digits = 1)
chance_more_likely_not <- round(with(data_2, sum(more_likely_not)/nrow(data_2))*100, digits = 1)
chance_likely <- round(with(data_2, sum(likely)/nrow(data_2))*100, digits = 1)
chance_very_likely <- round(with(data_2, sum(very_likely)/nrow(data_2))*100, digits = 1)
slices <- c(chance_unlikely, chance_more_likely_not, chance_likely, chance_very_likely)
lbls <- c("Unlikely", "More Likely Not", "Likely", "Very Likely")
lbls <- paste(lbls, slices)
lbls <- paste(lbls, "%", sep = "")
pie(slices, labels = lbls, col = rainbow(length(lbls)), main = "How likely are you to finance a vacation?")
data_3 <- subset(data_2, unlikely!=1)
data_3 <- subset(data_3, more_likely_not!=1)
#subsetting for only those that answered likely or very likely
data_3$ave_vacation_budget <- type.convert(data_3$ave_vacation_budget, na.strings = "NA", dec=".", numerals = c("no.loss"))
data_3$ave_finance_range <- type.convert(data_3$ave_finance_range, na.strings = "NA", dec=".", numerals = c("no.loss"))
data_3$ave_age_range <- type.convert(data_3$ave_age_range, na.strings = "NA", dec=".", numerals = c("no.loss"))
data_3 <- subset(data_3, !is.na(ave_vacation_budget))
data_3 <- subset(data_3, !is.na(ave_finance_range))
data_3 <- subset(data_3, !is.na(ave_age_range))
data_4 <- subset(data_2, very_likely!=1)
data_4 <- subset(data_4, likely!=1)
data_4$ave_vacation_budget <- type.convert(data_4$ave_vacation_budget, na.strings = "NA", dec=".", numerals = c("no.loss"))
data_4$ave_finance_range <- type.convert(data_4$ave_finance_range, na.strings = "NA", dec=".", numerals = c("no.loss"))
data_4$ave_age_range <- type.convert(data_4$ave_age_range, na.strings = "NA", dec=".", numerals = c("no.loss"))
data_4 <- subset(data_4, !is.na(ave_vacation_budget))
data_4 <- subset(data_4, !is.na(ave_finance_range))
data_4 <- subset(data_4, !is.na(ave_age_range))
#getting rid of those who didn't answer all of the pertinent questions
likelies_ave_budget <- sum(data_3$ave_vacation_budget)/nrow(data_3)
likelies_ave_age <- sum(data_3$ave_age_range)/nrow(data_3)
likelies_ave_finance_range <- sum(data_3$ave_finance_range)/nrow(data_3)
unlikelies_ave_budget <- sum(data_4$ave_vacation_budget)/nrow(data_4)
unlikelies_ave_age <- sum(data_4$ave_age_range)/nrow(data_4)
unlikelies_ave_finance_range <- sum(data_4$ave_finance_range)/nrow(data_4)
likeliness_values <- c(likelies_ave_budget, likelies_ave_finance_range, unlikelies_ave_budget, unlikelies_ave_finance_range)
likeliness_names <- c(rep("Likely", 2), rep("Unlikely", 2))
likeliness_types <- rep(c("Budget", "Finance Range"), 2)
likeliness_comp <- data.frame(likeliness_names, likeliness_types, likeliness_values)
ggplot(data = likeliness_comp, aes(x = likeliness_names, y = likeliness_values, fill = likeliness_types)) + geom_bar(stat = "identity", position = "dodge") + xlab("") + ylab("Cost($)") + ggtitle("Likely vs Unlikely Budgets and Finance Ranges") + theme(legend.title = element_blank()) + annotate("text", label = "15% rise", x = 0.8, y = 2700) + annotate("text", label = "46% drop", x = 2.22, y = 1600) + geom_segment(aes(x = 1, xend = 1, y = likelies_ave_budget, yend = likelies_ave_finance_range), arrow = arrow(length = unit(0.3, "cm"))) + geom_segment(aes(x = 2, xend = 2, y = unlikelies_ave_budget, yend = unlikelies_ave_finance_range), arrow = arrow(length = unit(0.3, "cm")))
#likelies tend to have lower budgets, but much larger finance ranges than average
likeliness_ages_values <- c(likelies_ave_age, unlikelies_ave_age)
likeliness_ages_names <- c("Likely", "Unlikely")
likeliness_ages_types <- rep(c("Age"), 2)
likeliness_ages_comp <- data.frame(likeliness_ages_names, likeliness_ages_types, likeliness_ages_values)
ggplot(data = likeliness_ages_comp, aes(x = likeliness_ages_types, y = likeliness_ages_values, fill = likeliness_ages_names)) + geom_bar(stat = "identity", position = "dodge") + xlab("") + ylab("Age(Years)") + ggtitle("Likely vs Unlikely Age") + theme(legend.title = element_blank())
Leisure_Options <- c("Beach", "Sightseeing", "Trekking", "Skiing", "Other")
for(f in 1:5){
  z <- paste("likelies_", Leisure_Options[f], sep = "", collapse = NULL)
  assign(x=z, value = sum(data_3[f+25])/nrow(data_3))
}
for(g in 1:5){
  z <- paste("unlikelies_", Leisure_Options[g], sep = "", collapse = NULL)
  assign(x=z, value = sum(data_4[g+25])/nrow(data_4))
}
Leisure_List <- rep(c("Beach", "Sightseeing and Touring", "Trekking", "Skiing, Golfing, Biking, etc...", "Other"), 2)
Leisure_List <- factor(Leisure_List, levels = c("Beach", "Sightseeing and Touring", "Trekking", "Skiing, Golfing, Biking, etc...", "Other"))
Preferred_Percentages <- c(likelies_Beach, likelies_Sightseeing, likelies_Trekking, likelies_Skiing, likelies_Other, unlikelies_Beach, unlikelies_Sightseeing, unlikelies_Trekking, unlikelies_Skiing, unlikelies_Other)
Likeness <- rep(c("Likely", "Unlikely"), each = 5)
likeness_leisures <- data.frame(Likeness, Leisure_List, Preferred_Percentages)
ggplot(likeness_leisures, aes(fill = Likeness, y = Preferred_Percentages, x = Leisure_List)) + geom_bar(position = "dodge", stat = "identity") + ggtitle("Likely vs Unlikely Preferred Travels") + labs(x = "", y = "Population Seeking(%)") + theme(legend.title = element_blank()) + scale_y_continuous(expand = c(0,0), breaks = round(seq(min(likeness_leisures$Preferred_Percentages), max(likeness_leisures$Preferred_Percentages), by = 0.1), 1))
cities_canada <- read.csv("canadian_cities.csv")
cities_canada <- unique(cities_canada[,2:6])
plot(cities_canada$longitude, cities_canada$lattitude)
cities_data <- subset(wishpond, select = c("Email", "Geoip.City", "Geoip.State", "ave_finance_range", "ave_vacation_budget", "ave_age_range", "leisure_travel_type_Beach", "leisure_travel_type_Sightseeing_Touring", "leisure_travel_type_Trekking", "leisure_travel_type_Skiing_Golfing_Biking", "leisure_travel_type_Other", "likelihood"))
cities_data <- subset(cities_data, Geoip.City !="")
cities_data$lat <- 0
cities_data$long <- 0
cities_data$Geoip.City <- as.character(cities_data$Geoip.City)
cities_canada$City <- as.character(cities_canada$City)
cities_data$Geoip.State <- as.character(cities_data$Geoip.State)
for(q in 1:nrow(cities_data)){
  for (r in 1:nrow(cities_canada)) {
    if(identical(cities_data$Geoip.City[q], cities_canada$City[r])){
      cities_data$lat[q] <- cities_canada$lattitude[r]
      cities_data$long[q] <- cities_canada$longitude[r]
      q <- q+1
    }
  }
}
for (s in 1:nrow(cities_data)) {
  if(cities_data$lat[s]==0){
    if(cities_data$long[s]==0){
      if(identical(cities_data$Geoip.State[s], "Alberta")){
        cities_data$Geoip.City[s] <- "Edmonton"
      }
      if(identical(cities_data$Geoip.State[s], "British Columbia")){
        cities_data$Geoip.City[s] <- "North Vancouver"
      }
      if(identical(cities_data$Geoip.State[s], "Manitoba")){
        cities_data$Geoip.City[s] <- "Winnipeg"
      }
      if(identical(cities_data$Geoip.State[s], "New Brunswick")){
        cities_data$Geoip.City[s] <- "Fredericton"
      }
      if(identical(cities_data$Geoip.State[s], "Newfoundland and Labrador")){
        cities_data$Geoip.City[s] <- "St. John's"
      }
      if(identical(cities_data$Geoip.State[s], "Northwest Territories")){
        cities_data$Geoip.City[s] <- "Yellowknife"
      }
      if(identical(cities_data$Geoip.State[s], "Nova Scotia")){
        cities_data$Geoip.City[s] <- "Halifax"
      }
      if(identical(cities_data$Geoip.State[s], "Nunavut")){
        cities_data$Geoip.City[s] <- "Iqaliut"
      }
      if(identical(cities_data$Geoip.State[s], "Ontario")){
        cities_data$Geoip.City[s] <- "Toronto"
      }
      if(identical(cities_data$Geoip.State[s], "Prince Edward Island")){
        cities_data$Geoip.City[s] <- "Charlottetown"
      }
      if(identical(cities_data$Geoip.State[s], "Quebec")){
        cities_data$Geoip.City[s] <- "Montréal-Ouest"
      }
      if(identical(cities_data$Geoip.State[s], "Saskatchewan")){
        cities_data$Geoip.City[s] <- "Regina"
      }
      if(identical(cities_data$Geoip.State[s], "Yukon")){
        cities_data$Geoip.City[s] <- "Whitehorse"
      }
    }
  }
}
#taking all of those who are not in registered cities from the data fram and setting their city
#as the largest of whatever province they're from
for(q in 1:nrow(cities_data)){
  for (r in 1:nrow(cities_canada)) {
    if(identical(cities_data$Geoip.City[q], cities_canada$City[r])){
      cities_data$lat[q] <- cities_canada$lattitude[r]
      cities_data$long[q] <- cities_canada$longitude[r]
      q <- q+1
    }
  }
}
cities_data <- subset(cities_data, lat!=0)
cities_data <- subset(cities_data, Geoip.State!="Kingston") #get rid of our Jamaican friend
Province_Options <- unique(cities_data$Geoip.State, incomparables = FALSE)
library(devtools)
library(easyGgplot2)
ggplot2.scatterplot(data = cities_data, xName = "long", yName = "lat", groupName = "Geoip.State", size = length(Province_Options), backgroundColor = "white") + ggtitle("Location of Users") + labs(x = "Lattitude", y = "Longitude") + theme(legend.title = element_blank()) + ylim(42, 63) + xlim(-135, -50)
#let's do the same, but with only those who answered likely or very likely
cities_data <- subset(cities_data, likelihood!="Unlikely")
cities_data <- subset(cities_data, likelihood!="More likely not")
ggplot2.scatterplot(data = cities_data, xName = "long", yName = "lat", groupName = "Geoip.State", size = length(Province_Options), backgroundColor = "white", groupColors = c("red", "blue", "green", "pink", "grey", "purple", "orange", "turquoise", "maroon", "black")) + ggtitle("Location of Likely Users") + labs(x = "Lattitude", y = "Longitude") + theme(legend.title = element_blank()) + ylim(42, 63) + xlim(-135, -50)
#the biggest difference between the two is that the east coasters numbers seem to drop quite a bit while west coast stays largely the same
