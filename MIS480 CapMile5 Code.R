setwd('/Users/tbrown/Desktop/StudySession')
library(openxlsx)
library(tidyverse)
pet_data <- read.xlsx("/Users/tbrown/Desktop/StudySession/PetFoodSampleDataset.xlsx")

#Basic Data Glimpse
summary(pet_data)
count(pet_data)
pet_data$pet_has_active_subscription <- as.numeric(pet_data$pet_has_active_subscription)

#Analyzing Data Through Plots
hist(pet_data$pet_has_active_subscription, main = 'Pet Subscription', xlab = 'No Subscription = 0, Active Subscription = 1', freq = TRUE)
tier <- table(pet_data$pet_food_tier)
barplot(tier)
hist(pet_data$pet_order_number, main = 'Number of Orders Per Pet', xlab = "Number of Orders")
hist(pet_data$pet_allergen_list, main = "Pet Allergies", xlab = 'No Food Allergies = 0, Food Allergies = 1')
health <-table(pet_data$pet_health_issue_list)
plot(health, type = 'h', main = 'Pet Health Issue Groups')
hist(pet_data$neutered, main = "Neuter Status", xlab = 'Not Neutered = 0, Neutered = 1')
gender <- table(pet_data$gender)
barplot(gender, main= 'Pet Gender')
pet_size <- table(pet_data$pet_breed_size)
barplot(pet_size, main = 'Pet Breed Size')
promo <- table(pet_data$signup_promo)
plot(promo, main = "Sign Up Promo")
dry_brands <- table(pet_data$dry_food_brand_pre_tails)
barplot(dry_brands, main = "Dry Pet Food Brands", las=2, cex.names = 0.5, ylim= c(0,40))
life_stage <- table(pet_data$pet_life_stage_at_order)
barplot(life_stage, main = "Pet Life Stage at Order")
hist(pet_data$total_web_sessions, main = "Total Web Sessions", xlab = 'Total Number of Web Sessions')
hist(pet_data$total_minutes_on_website, main = "Total Minutes on Website", xlab = "Total Minutes")
ticket_cat <- table(pet_data$customer_support_ticket_category)
barplot(ticket_cat, main = "Customer Support Ticket Category", las=2, cex.names = 0.67)

#Fit a CART for Pet Food Subscriptions
library(rpart)
library(rpart.plot)
pet_data$Pet_Subscription_Logic <- as.logical(pet_data$pet_has_active_subscription)
tree <- rpart(Pet_Subscription_Logic ~ pet_order_number+pet_food_tier+pet_breed_size+signup_promo+dry_food_brand_pre_tails+pet_life_stage_at_order+
    total_minutes_on_website+total_minutes_on_website+customer_support_ticket_category, data = pet_data, control = rpart.control(cp=.0001))
printcp(tree)
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
pruned_tree <- prune(tree, cp=best)
prp(pruned_tree, type = 2, faclen=0,extra='auto', varlen=0, box.palette = "auto",compress = TRUE, ycompress=TRUE, left = TRUE,roundint=F, digits=3)

#Edited CART-include web variables
tree_edit <- rpart(Pet_Subscription_Logic ~ signup_promo+total_web_sessions+total_minutes_on_website+customer_support_ticket_category, data = pet_data, control = rpart.control(cp=.01))
printcp(tree_edit)
best_edit <- tree_edit$cptable[which.min(tree_edit$cptable[,"xerror"]),"CP"]
pruned_tree_edit <- prune(tree_edit, cp=best_edit)
prp(pruned_tree_edit,
    faclen=0,extra='auto', type=4, varlen=0, box.palette = "auto",
    roundint=F, digits=3)

#Example Prediction
proportions <- table(pet_data$Pet_Subscription_Logic)/length(pet_data$Pet_Subscription_Logic)
percentages <- proportions*100
percentages