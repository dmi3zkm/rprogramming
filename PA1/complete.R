complete <- function(directory, id = 1:332){
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    allFiles <- list.files(directory, full.names = TRUE);
    
    ids <- vector(mode="integer");
    nobs <- vector(mode="integer");
    for(i in id){
        df <- read.csv(allFiles[i]);
        
        ids <- append(ids, i);
        nobs <- append(nobs, nrow(df[complete.cases(df),]));
    }
    
    data.frame(ids, nobs);
}