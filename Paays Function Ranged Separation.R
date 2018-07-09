library(tidyr)
setwd("/Users/jackpark/Desktop") #set your directory
wishpond <- read.csv("wishpond_leads_june25 - wishpond_leads_june25.csv")
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
wishpond$leisure_travel_type_Sightseeing <- 0
wishpond$leisure_travel_type_Trekking <- 0
wishpond$leisure_travel_type_Touring <- 0
wishpond$leisure_travel_type_Other <- 0
for(l in 1:nrow(wishpond)){
  if(grepl("Beach", wishpond$leisure_travel_type[l], fixed = TRUE)){
    wishpond$leisure_travel_type_Beach[l] <- 1
  }
  if(grepl("Sightseeing", wishpond$leisure_travel_type[l], fixed = TRUE)){
    wishpond$leisure_travel_type_Sightseeing[l] <- 1
  }
  if(grepl("Trekking", wishpond$leisure_travel_type[l], fixed = TRUE)){
    wishpond$leisure_travel_type_Trekking[l] <- 1
  }
  if(grepl("Touring", wishpond$leisure_travel_type[l], fixed = TRUE)){
    wishpond$leisure_travel_type_Touring[l] <- 1
  }
  if(grepl("Other", wishpond$leisure_travel_type[l], fixed = TRUE)){
    wishpond$leisure_travel_type_Other[l] <- 1
  }
}
library(scales)
clean_data <- subset(wishpond, select = c("Email", "leisure_travel_type_Beach", "leisure_travel_type_Sightseeing", "leisure_travel_type_Trekking", "leisure_travel_type_Touring", "leisure_travel_type_Other", "ave_vacation_budget", "ave_finance_range", "ave_age_range"))
clean_data$ave_vacation_budget <- type.convert(clean_data$ave_vacation_budget, na.strings = "NA", dec=".", numerals = c("no.loss"))
clean_data$ave_finance_range <- type.convert(clean_data$ave_finance_range, na.strings = "NA", dec=".", numerals = c("no.loss"))
clean_data$ave_age_range <- type.convert(clean_data$ave_age_range, na.strings = "NA", dec=".", numerals = c("no.loss"))
clean_data <- subset(clean_data, !is.na(ave_vacation_budget))
clean_data <- subset(clean_data, !is.na(ave_finance_range))
clean_data <- subset(clean_data, !is.na(ave_age_range))
#limiting data to the pertinent columns and getting rid of rows with limited data
budget1000 <- subset(clean_data, ave_vacation_budget==1000)
budget1500 <- subset(clean_data, ave_vacation_budget==1500)
budget3500 <- subset(clean_data, ave_vacation_budget==3500)
budget7500 <- subset(clean_data, ave_vacation_budget==7500)
budget10000 <- subset(clean_data, ave_vacation_budget==10000)
budget5000plus <- subset(clean_data, ave_vacation_budget>=5000)
#split the data into each different price range to compare all of the ranges
budget1000_beach <- percent(sum(budget1000$leisure_travel_type_Beach)/nrow(budget1000))
budget1000_sightseeing <- percent(sum(budget1000$leisure_travel_type_Sightseeing)/nrow(budget1000))
budget1000_trekking <- percent(sum(budget1000$leisure_travel_type_Trekking)/nrow(budget1000))
budget1000_touring <- percent(sum(budget1000$leisure_travel_type_Touring)/nrow(budget1000))
budget1000_other <- percent(sum(budget1000$leisure_travel_type_Other)/nrow(budget1000))
budget1500_beach <- percent(sum(budget1500$leisure_travel_type_Beach)/nrow(budget1500))
budget1500_sightseeing <- percent(sum(budget1500$leisure_travel_type_Sightseeing)/nrow(budget1500))
budget1500_trekking <- percent(sum(budget1500$leisure_travel_type_Trekking)/nrow(budget1500))
budget1500_touring <- percent(sum(budget1500$leisure_travel_type_Touring)/nrow(budget1500))
budget1500_other <- percent(sum(budget1500$leisure_travel_type_Other)/nrow(budget1500))
budget3500_beach <- percent(sum(budget3500$leisure_travel_type_Beach)/nrow(budget3500))
budget3500_sightseeing <- percent(sum(budget3500$leisure_travel_type_Sightseeing)/nrow(budget3500))
budget3500_trekking <- percent(sum(budget3500$leisure_travel_type_Trekking)/nrow(budget3500))
budget3500_touring <- percent(sum(budget3500$leisure_travel_type_Touring)/nrow(budget3500))
budget3500_other <- percent(sum(budget3500$leisure_travel_type_Other)/nrow(budget3500))
budget7500_beach <- percent(sum(budget7500$leisure_travel_type_Beach)/nrow(budget7500))
budget7500_sightseeing <- percent(sum(budget7500$leisure_travel_type_Sightseeing)/nrow(budget7500))
budget7500_trekking <- percent(sum(budget7500$leisure_travel_type_Trekking)/nrow(budget7500))
budget7500_touring <- percent(sum(budget7500$leisure_travel_type_Touring)/nrow(budget7500))
budget7500_other <- percent(sum(budget7500$leisure_travel_type_Other)/nrow(budget7500))
budget10000_beach <- percent(sum(budget10000$leisure_travel_type_Beach)/nrow(budget10000))
budget10000_sightseeing <- percent(sum(budget10000$leisure_travel_type_Sightseeing)/nrow(budget10000))
budget10000_trekking <- percent(sum(budget10000$leisure_travel_type_Trekking)/nrow(budget10000))
budget10000_touring <- percent(sum(budget10000$leisure_travel_type_Touring)/nrow(budget10000))
budget10000_other <- percent(sum(budget10000$leisure_travel_type_Other)/nrow(budget10000))
budget5000plus_beach <- percent(sum(budget5000plus$leisure_travel_type_Beach)/nrow(budget5000plus))
budget5000plus_sightseeing <- percent(sum(budget5000plus$leisure_travel_type_Sightseeing)/nrow(budget5000plus))
budget5000plus_trekking <- percent(sum(budget5000plus$leisure_travel_type_Trekking)/nrow(budget5000plus))
budget5000plus_touring <- percent(sum(budget5000plus$leisure_travel_type_Touring)/nrow(budget5000plus))
budget5000plus_other <- percent(sum(budget5000plus$leisure_travel_type_Other)/nrow(budget5000plus))
total_budget_beach <- percent(sum(clean_data$leisure_travel_type_Beach)/nrow(clean_data))
total_budget_sightseeing <- percent(sum(clean_data$leisure_travel_type_Sightseeing)/nrow(clean_data))
total_budget_trekking <- percent(sum(clean_data$leisure_travel_type_Trekking)/nrow(clean_data))
total_budget_touring <- percent(sum(clean_data$leisure_travel_type_Touring)/nrow(clean_data))
total_budget_other <- percent(sum(clean_data$leisure_travel_type_Other)/nrow(clean_data))
#find the percentage chance that each option of leisure travel was selected for the budget groups and for the group as a whole
Groups <- c("<$1000","$1,000-$2,000", "$2,000-$5,000", "$5,000-$10,000", "$10,000+", "$5,000+", "Everyone")
Beach <- c(budget1000_beach, budget1500_beach, budget3500_beach, budget7500_beach, budget10000_beach, budget5000plus_beach, total_budget_beach)
Sightseeing <- c(budget1000_sightseeing, budget1500_sightseeing, budget3500_sightseeing, budget7500_sightseeing, budget10000_sightseeing, budget5000plus_sightseeing, total_budget_sightseeing)
Trekking <- c(budget1000_trekking, budget1500_trekking, budget3500_trekking, budget7500_trekking, budget10000_trekking, budget5000plus_trekking, total_budget_trekking)
Touring <- c(budget1000_touring, budget1500_touring, budget3500_touring, budget7500_touring, budget10000_touring, budget5000plus_touring, total_budget_touring)
Other <- c(budget1000_other, budget1500_other, budget3500_other, budget7500_other, budget10000_other, budget5000plus_other, total_budget_other)
Occurances <- c(nrow(budget1000), nrow(budget1500), nrow(budget3500), nrow(budget7500), nrow(budget10000), nrow(budget5000plus), nrow(clean_data))
budgetcomparison <- data.frame(Groups, Beach, Sightseeing, Trekking, Touring, Other, Occurances)
budgetcomparison
#as we can see from this table the beach, sightseeing and touring are by far the three most saught after leasure travel types
#as a general trend, sightseeing, trekking, touring and other all decrease with increasing budget
#we also see that there are less options selected the higher the budget and the higher budget users tend to gravitate more towards the beach