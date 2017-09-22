# Example Automation of Chikolongo Data 

############### Required Libraries and Data Sets #######################
#library(xlsx) #read in excel sheets ----> TO DO: rJava keeps failing - look into it! 
library(dplyr) #text data manipulation
library(tidytext) #parsing text
library(hunspell) #spell checker
data(stop_words) #useless words to eliminate from text mining

################ Good coding practices #################################
set.seed(5)

# Setting the working directory to source file location 
setwd("~/Helena_Imani/Automation")
# --> TO DO: look into not hardcoding the source file location

# Importing the written functions
source("Chikolongo_Automation_Functions.R")

############### Importing the Data File ################################ 

mydata <- read.table("Chikolongo_Direct_Beneficiary_Survey_Data.csv",
                     header=TRUE, sep=",")
# View(mydata)
# print(names(mydata))

############### Cleaning up the Data ###################################

cols_count = 1
extra_cols = c()
for (n in names(mydata)){
  # ---> TO DO: Think about how to vectorize this 
  if (substring(n,nchar(n)-4,nchar(n))=="extra"){
    # Collating all the "extra" column numbers 
    extra_cols = c(extra_cols,cols_count)
  }
  cols_count = cols_count+1 
}

# Deleting all the "extra" columns as they are empty
mydata = mydata[,-extra_cols]

################## Data Analysis #######################################

# Extracting all crop data (EXAMPLE; loop through all of the eventually)
crop_names = grep('crop', names(mydata), value=TRUE)
counter = 1 
for (i in crop_names){
  # print(i)
  col_data = mydata[,which(names(mydata)==i)]
  if(length(which(as.character(col_data)=='---'))!=0){
    #Replace --- with NAs
    col_data[which(as.character(col_data)=='---')]=NA
    #Then remove all the NAs as they are not needed
    col_data <- col_data[!is.na(col_data)]
  } else{}
  if ((sort(unique(col_data))[1] == 1 & tail(sort(unique(col_data)),n=1) == 2) | 
      (sort(unique(col_data))[1] == 1 & tail(sort(unique(col_data)),n=1) == 99) ){
    # Tests if answers consist of 1s and 2s only 
    print("Yes or No Question")
    y = length(which(col_data==1))
    n = length(which(col_data==2))
    print(paste(n, "(",signif(n/(y+n)*100,3), " % ) participants answered no"))
    print(paste(y, "(",signif(y/(y+n)*100,3), " % ) participants answered yes"))
  } else if (suppressWarnings(!is.na(as.numeric(substring(i,nchar(i)-1,nchar(i)))))){
    # Tests if there is a number at the end the question title (only up to double digits)
    print("Multiple Choice Question")
    option = as.numeric(substring(i,nchar(i)-1,nchar(i))) 
    #extracting the numeric value at the end of the question
    count=length(which(col_data==1)) #answers are either 1 or blank or ---
    print(paste(count,"(",signif(count/y*100,3),"% ) of those participants chose option",option))
    # --> TO DO: Could work on implementing a catch for the end of MC questions to gather data
    #            This would be much easier if questions are structured better
  } else if (sort(unique(is_letter(col_data)))[1] == TRUE){
    # Test if data is text; note, FALSE sorts before TRUE
    print("Quantitative Question")
    # Format text data in a data frame and parse it into a useful format
    text_df <- data_frame(line = 1:length(col_data),text=as.character(col_data))
    parsed_text_df <- text_df %>%
      unnest_tokens_(output='word',input='text') #tokenization
    # Count words in parsed texted
    count_text_df = count_fun(parsed_text_df)
    # Check and confirm the spelling of non-recognized words
    spell_check = hunspell_check(count_text_df$word) #T/F spell checker
    for (index in 1:length(spell_check)){
      TF = spell_check[index] #TRUE or FALSE for is it a word
      if (TF == FALSE){
        # If not a recognized word, ask for clarification
        true_word = rec_TF(count_text_df$word[index])
        # If different replace the old response with the new one and recount
        if (true_word != count_text_df$word[index]){
          parsed_text_df$word[(which(parsed_text_df$word == count_text_df$word[index]))]= true_word
        }
      }
    }
    # Get rid of fill words
    parsed_text_df <- parsed_text_df %>%
      anti_join(stop_words) 
    # Re-calculate the true word frequencies 
    count_text_df_corrected = count_fun(parsed_text_df)
    # --> TO DO: Implement a check for all words with end user
    # --> TO DO: If frequency of 2 counted words is the same confirm grouping them 
    for (index in 1:length(count_text_df_corrected$word)){
      print(paste(count_text_df_corrected$n[index],
                  "(",count_text_df_corrected$n[index]/sum(count_text_df_corrected$n),"% )",
                  "of participants answered with:",count_text_df_corrected$word[index]))
    }
  } else if ((sort(unique(col_data[grep('[0-9]+',col_data)]))[1]!=1) & 
           (tail(sort(unique(col_data[grep('[0-9]+',col_data)])),1)!=2) |
           length(sort(unique(col_data[grep('[0-9]+',col_data)])))>2 |
           length(sort(unique(col_data[grep('[0-9]+',col_data)])))==1){
    # Test if there are answers other than 1 or 2
    # Problem if 1 or 2 are the only single response answers given!! 
    # --> TO DO: better data structuring would solve the above issues 
    print("Single Response Question")
    vals = col_data[grep('[0-9]+',col_data)] #extracting numerics only 
    for (k in sort(unique(vals))){
      total = length(which(vals==as.numeric(k))) 
      print(paste(total,"participant(s) answered with:",k))
    }
  } else {
    print("This is currently not a recognized question format. Please update the code & or survey.")
  }
  counter=counter+1
}

# --> TO DO: Options for making figures, plain x-y graphs, tables and pie charts 

