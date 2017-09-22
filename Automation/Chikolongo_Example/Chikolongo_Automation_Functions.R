rec_TF <- function(word){
  # Recursive function for getting a TRUE or FALSE answer 
  # Either the word is accepted as correct (TRUE) or a correct spelling has to be provided (FALSE)
  answ = readline(prompt = paste(word,"is not a recognized word. Enter TRUE if it is or FALSE if you would like to correct it.    ->"))
  if (answ == TRUE) {
    print(paste(word, "has been accepted as a true word."))
    return(word)
  }
  else if (answ == FALSE) {
    new_resp = readline(prompt = paste("Please enter the corrected word for",word,"     -->"))
    print(paste(word, "has been corrected to", new_resp))
    return(new_resp)
  }
  else{
    rec_TF(word)}
}

is_letter <- function(x) grepl("[[:alpha:]]", x)
  # To check if variable x consists of letters 

count_fun <- function(dataframe){
  # Function to count the words in a dataframe
  count_df <- dataframe %>%
    count(word, sort = TRUE) #counts words in text 
  return(count_df)
}
