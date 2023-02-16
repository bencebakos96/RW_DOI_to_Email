#### Packages ####
library(easyPubMed)
library(tidyverse)

#### Redefining the fetch function ####

# because we are using DOIs, we don't need to retrieve 500 results, only 1 (retmax = 2)
# if it can't find the DOI it should not retry for 5 minutes, only 12 seconds
fetch_pubmed_data_shorter<- function(pubmed_id_list, 
                                     retstart = 0, 
                                     retmax = 2, 
                                     format = "xml", 
                                     encoding = "UTF8"){
  myIDlist <- pubmed_id_list
  if ((!is.list(myIDlist)) | is.na(myIDlist$WebEnv) | is.na(myIDlist$QueryKey) | 
      is.na(myIDlist$Count) | !is.integer(as.integer(retstart)) | 
      !is.integer(as.integer(retmax))) {
    message("There is an issue with the PubMed ID list you supplied. Please, call the function again and supply the result of a <get_pubmed_ids()> call as argument. Thank you.")
    return(NULL)
  }
  else {
    myWebEnv <- myIDlist$WebEnv
    myKey <- myIDlist$QueryKey
    myCount <- as.numeric(as.character(myIDlist$Count))
    myRetstart = as.integer(retstart)
    if (myRetstart < 0) {
      myRetstart = 0
    }
    myRetmax <- as.integer(retmax)
    if (myRetmax > 5000) {
      myRetmax = 5000
    }
    if (myRetmax < 1) {
      myRetmax = 1
    }
    if (format[1] %in% c("medline", "uilist", "abstract", 
                         "asn.1", "xml")) {
      myFormat <- format[1]
    }
    else {
      myFormat <- "xml"
    }
    typeMode <- switch(myFormat, asn.1 = c("null", "asn.1"), 
                       xml = c("null", "xml"), medline = c("medline", "text"), 
                       uilist = c("uilist", "text"), abstract = c("abstract", 
                                                                  "text"))
    efetch_url = paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?", 
                       "db=pubmed&WebEnv=", myWebEnv, "&query_key=", myKey, 
                       "&retstart=", myRetstart, "&retmax=", myRetmax, "&rettype=", 
                       typeMode[1], "&retmode=", typeMode[2], sep = "")
    api_key <- pubmed_id_list$APIkey
    if (!is.null(api_key)) {
      efetch_url <- paste(efetch_url, "&api_key=", api_key, 
                          sep = "")
    }
    out.data <- NULL
    try_num <- 1
    t_0 <- Sys.time()
    while (is.null(out.data)) {
      if (try_num > 1) 
        Sys.sleep(time = 0.5)
      t_1 <- Sys.time()
      if (as.numeric(difftime(t_1, t_0, units = "mins")) > 
          0.2) {
        message("Killing the request! Something is not working. Please, try again later")
        return(NULL)
      }
      out.data <- tryCatch({
        tmpConnect <- suppressWarnings(url(efetch_url, 
                                           open = "rb", encoding = "UTF8"))
        suppressWarnings(readLines(tmpConnect, warn = FALSE, 
                                   encoding = "UTF8"))
      }, error = function(e) {
        NULL
      }, finally = {
        try(suppressWarnings(close(tmpConnect)), silent = TRUE)
      })
      if (!is.null(out.data) && class(out.data) == "character" && 
          grepl("<ERROR>", substr(paste(utils::head(out.data, 
                                                    n = 100), collapse = ""), 1, 250))) {
        out.data <- NULL
      }
      try_num <- try_num + 1
    }
    if (is.null(out.data)) {
      message("Killing the request! Something is not working. Please, try again later")
      return(NULL)
    }
    if (encoding != "UTF8") 
      out.data <- base::iconv(out.data, from = "UTF8", 
                              to = encoding, sub = ".")
    if (format[1] == "xml") {
      out.data <- paste(out.data, collapse = "")
    }
    return(out.data)
  }
}

#### Database ####

RW_dois <- read_csv("data/RWDBDNLD11102022_filtered.csv")

# Converting the dataframe into a character vector
RW_dois <- RW_dois$OriginalPaperDOI

#### PubMed ####
# In this section we make requests to the PubMed database based on DOIs

# We create an empty dataframe - we will append new data to it every loop
data <- data_frame()

#We set k = 1 because we will start from the first element/DOI
k <- 1

for (i in k:length(RW_dois)) { # loop runs from the first element to the last
  my_query <- RW_dois[k] # we submit element k as the search term for PubMed
  my_entrez_id <- get_pubmed_ids(my_query) # we get the record's PubMed ID
  articles_xml <- tryCatch(fetch_pubmed_data_shorter(my_entrez_id, format = "xml"), error = function(e){}) # we get the data based on the PubMed ID
  data_new <- tryCatch(article_to_df(articles_to_list(articles_xml)),error = function(e){})
  # we get data in XML format and convert it to a dataframe
  # this line sometimes results in an error, halting the loop, but using tryCatch prevents that
  data <- rbind(data,data_new) # we add the new rows of data to our dataframe
  Sys.sleep(0.5) # we can make 3 requests/sec, this line limits us to 2/sec
  k <- k+1 # we increase k and start the loop again
}

#### Export ####

write_csv(data, "data/data.csv")

# A filtered version with only rows that contain e-mail addresses
RW_e_mails <- data %>% 
  drop_na(email)

write_csv(RW_e_mails, "data/RW_e_mails.csv")

# There still might be duplicate e-mail addresses
RW_e_mails_no_dups <- RW_e_mails %>% 
  distinct(email, .keep_all = TRUE)

write_csv(RW_e_mails_no_dups, "data/RW_e_mails_no_dups.csv")