library('RCurl')
library('XML')
library('jsonlite')

#zillow_xml <- getURL("http://www.zillow.com/webservice/GetZestimate.htm?zws-id=X1-ZWz1g5pwzifhmz_4234r&zpid=48749425")
zpid <- "48749425" # This is the property id. Zillow will fetch all details of the property id.
url_zestimate <- "http://www.zillow.com/webservice/GetZestimate.htm?zws-id=X1-ZWz1g5pwzifhmz_4234r&zpid="

# Pass the property id as part of the API Call
zestimate_xml <- getURL(paste(url_zestimate,zpid,sep =""))

# The API call returns an xml which needs to be parsed
parsed.zillow.xml <- xmlParse(zestimate_xml)
xml_data <- xmlToList(parsed.zillow.xml)

# Get the value of the property
xml_data$response$zestimate$amount$text

###############
#  this is for searching with address

library(utils)
address <- "2114 Bigelow Ave"
address <- URLencode(stringr::str_replace_all(address," ","+"))
citystatezip <- "Seattle, WA"
citystatezip <- URLencode(stringr::str_replace_all(citystatezip," ","+"))

paramString <- paste("&address=",address,"&citystatezip=",citystatezip, sep = "")
url_general_search <-"http://www.zillow.com/webservice/GetSearchResults.htm?zws-id=X1-ZWz1g5pwzifhmz_4234r"
general_search_xml <- getURL(paste(url_general_search,paramString,sep =""))

parsed.zillow.xml <- xmlParse(general_search_xml)
xml_data <- xmlToList(parsed.zillow.xml)
results <- xml_data$response$results$result


#create data frame with following
# address (street+city+state+zipcode), amount
property_details <- data.frame(matrix(data = NA, nrow = 1, ncol = 5))
colnames(property_details) <- c("Street","City","State","ZipCode","Amount")
property_details$Street <- results$address$street
property_details$City <- results$address$city
property_details$State <- results$address$state
property_details$ZipCode <- results$address$zipcode
property_details$Amount <- results$zestimate$amount$text
