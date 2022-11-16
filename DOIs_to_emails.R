#### Packages ####
library(easyPubMed)
library(tidyverse)

#### DOIs as links ####
# this section is unused at the moment as all dois in the RW database are usable as it is

#dummy_doi <- read_csv("data/dummy_doi.csv")
#dummy_doi <- str_split_fixed(dummy_doi$DOI, "/10.",2)[,2]
#dummy_doi <- paste0("10.",dummy_doi)

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
  articles_xml <- fetch_pubmed_data(my_entrez_id, format = "xml") # we get the data based on the PubMed ID
  data_new <- tryCatch(article_to_df(articles_to_list(articles_xml)),error = function(e){})
  # we get data in XML format and convert it to a dataframe
  # this line sometimes results in an error, halting the loop, but using tryCatch prevents that
  data <- rbind(data,data_new) # we add the new rows of data to our dataframe
  Sys.sleep(0.5) # we can make 3 requests/sec, this line limits us to 2/sec
  k <- k+1 # we increase k and start the loop again
}

#### Export ####