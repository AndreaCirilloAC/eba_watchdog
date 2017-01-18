library(rio)
library(rvest)
library(dplyr)
acc_and_aud_code <- read_html("https://www.eba.europa.eu/regulation-and-policy/accounting-and-auditing")

acc_and_aud_code %>% 
  html_nodes(" #activities .Title") %>% 
  html_attr("href") -> link
acc_and_aud_code %>% 
  html_nodes("#activities .Title ") %>% 
  html_text() -> names

acc_and_aud_code %>% 
  html_nodes("#activities .Date ") %>% 
  html_text() -> status

acc_and_aud_db <- data.frame(names,status,link)
View(acc_and_aud_db)

# credit risk regulation and guidelines

credit_risk <- read_html('https://www.eba.europa.eu/regulation-and-policy/credit-risk/-/activity-list/tSWPlf98sl2V/more')
credit_risk %>% 
  html_nodes(" #activities .Title") %>% 
  html_attr("href") -> link

credit_risk %>% 
  html_nodes("#activities .Title") %>% 
  html_text() -> names

credit_risk_db <- data.frame(names,link)

#derive status and other info from the specific link ( avoiding issue if status or other attributes)
# are not showing for a given document

info_extractor <-  function(urls){
  page <- read_html(urls)
  page %>% 
    html_nodes(".Date") %>% 
    html_text()-> available_info
  
  if(length(available_info) == 2) {
    status       <- available_info[1]
    release_date <- available_info[2]
  
  }else{
    status <- available_info[1]
    page %>%
      html_node(".event-create-date")%>%
      html_text() -> release_date
  
  }
  return(data.frame(status, release_date))
}

info_extractor(as.character(credit_risk_db[1,2]))
