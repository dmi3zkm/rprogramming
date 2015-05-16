corr <- function(directory, threshold = 0){
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    
    allFiles <- list.files(directory, full.names = TRUE);
    
    df.complete <- complete(directory);
    
    df.complete <- subset(df.complete, nobs > threshold)
    
    cors <- vector(mode="numeric")
    for(i in df.complete$ids){
        table <- read.csv(allFiles[i]);
        cors <- append(cors, cor(table$sulfate, table$nitrate, use = "complete.obs"));
    }
    
    cors
}