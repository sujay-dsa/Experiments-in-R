library(mongolite)
library(dlnm)
library(rpart)

# Reading JSON data from mongoDB
# Load data into your mongodb and execute mongod before running this code
post_insights <- mongo(collection = "fb_post_insights", db="local")

post_insights$count()

unique_post_ids <- post_insights$distinct(key = "fb_id")

temp <- post_insights$find(limit = 1)
temp1<- temp$fb_metrics

library (plyr)
library(dplyr)
temp21<- ldply (temp1, data.frame) # Each element of temp1 is a row in a data frame
temp22 <- unlist(temp21$values, recursive = FALSE,use.names = TRUE) # Bring nested data to the front
temp2<- matrix(unlist(temp1), nrow = 95)


# combine original data with the unravelled data.
metrics <-as.data.frame( cbind(as.vector(temp$fb_id), as.vector(temp$created_at), 
                 as.vector(temp$updated_at), as.vector(temp22), as.vector(temp2)))

post_insights$index(add = '{"fb_id":1}')

# Directly query mongodb to find elements within a range
post1 <- post_insights$find('{
                      "fb_id":{
                                "$gte": "222530618111374_402529463444821", 
                                "$lte": "222530618111374_406941906336910"
                            }}')





# create separate data frames and JSON Files for each id as we have nested data.
# Make sure your working directory is same as source file
unique_ids <- unique(post1$fb_id)
for(id in unique_ids[1:5]){
  jsonob <- toJSON(x = post1[post1$fb_id==id,])
sink(paste("insights_",id,".json"))
cat(jsonob)
sink()
}

library(tidyjson)
library(dplyr)
library(rjson)

# Read from the generated file.
json_file <- "insights_ 222530618111374_402529463444821 .json"
json_data <- (paste(readLines(json_file), collapse=""))
json <- toJSON(json_data)


json %>%  as.tbl_json %>% gather_keys %>% gather_array %>% 
  +   spread_values(
    post_stories=jstring("")
  )

#############################################
#     for ad insights
###############################################

ad_insights <- mongo(collection = "fb_ad_insights", db="local")
ad_insights$count()

ads <- ad_insights$find() #importing all the data into a data frame.
temp_ads <- ads # create a back up.
ads <- ads[1:10,]

#remove nested frames
colnames(ads$relevance_score) <-  paste("relevance_score",colnames(ads$relevance_score), sep = ".")
relevance_score <- ads$relevance_score
ads <- cbind(ads, relevance_score)


columns_to_remove <- c("cost_per_action_type","cost_per_unique_action_type",
                       "actions","unique_actions","website_ctr","action_values",
                       "account_name","campaign_name","adset_name","ad_name",
                       "fb_ad_account_id","relevance_score")

ads <- ads[,!colnames(ads) %in% columns_to_remove]
str(ads$relevance_score)

ads$cost_per_total_action <- as.numeric(ads$cost_per_total_action)
ads$impressions <- as.numeric(ads$impressions)
ads$app_store_clicks <- as.numeric(ads$app_store_clicks)
ads$canvas_avg_view_time <- as.numeric(ads$canvas_avg_view_time)
ads$cost_per_total_action <- as.numeric(ads$cost_per_total_action)
ads$deeplink_clicks <- as.numeric(ads$deeplink_clicks)
ads$newsfeed_impressions <- as.numeric(ads$newsfeed_impressions)

fb_content_ids <- unique(ads$fb_content_piece_id)

lapply(fb_content_ids, function(x){
  
  temp_csv <- ads[ads$fb_content_piece_id==x,]
  write.csv(x = temp_csv[order(temp_csv$created_at),],
            file = paste(x,".csv",sep = ""))
})





unique_relation_keys <- unique(post1$insights_relationship_key)
ads <- ads[ads$insights_relationship_key %in% unique_relation_keys,]

head(ads$cost_per_action_type)






#####################################################################
# read the converted json file
#######################################################################
# This file was visualized and cleaned up in tableau.
df <- read.csv("403382763359491_revised.csv")


column_names <- colnames(df)

str(df)
df$Created.At <- strptime(df$Created.At,"%m/%d/%Y %I:%M:%S %p")

summary(chicagoNMMAPS$time)

library(forecast)
library(usdm)
vifcor(df[,-c(7,8,9,10,11,56)])
vifcor(df[,c(1:6)])
vifstep(df[,c(1:6)])



#
#   Create the cross basis matrices
#
library(dlnm)
cb.post.impressions.unique <- crossbasis(df$ï..calc.post.impressions.unique , lag = 7,
                           argvar = list(df=5), arglag = list(fun="strata",breaks=1))


cb.post.impressions.unique.2 <- crossbasis(df$ï..calc.post.impressions.unique , lag = 7,
                                         argvar = list(fun="lin"), arglag = list(fun="strata",breaks=1))


cb.total.engagement.percentag <- crossbasis(df$calc.total.engagement.percentage , lag = 7,
                                         argvar = list(df=5), arglag = list(fun="strata",breaks=1))


cb.total.engagement.percentag.2 <- crossbasis(df$calc.total.engagement.percentage , lag = 7,
                                           argvar = list(fun="lin"), arglag = list(fun="strata",breaks=1))



summary(cb.totalLove)


#
#  Create the models
#
library(splines)

model1<- glm(Post.stories.share~cb.post.impressions.unique.2+cb.total.engagement.percentag.2
             ,family = quasipoisson(),
             data = df)

summary(model1)
pred1.post.impressions.unique <- crosspred(cb.post.impressions.unique.2, 
                                           model1,cumul=TRUE,at=0:5)

plot(pred1.post.impressions.unique, "slices",var=5,col=3)

pred1.post.impressions.unique$model.link
