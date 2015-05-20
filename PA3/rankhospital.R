rankhospital <- function(state, outcome, num = "best"){
    
    # Read outcome data
    # Check that state and outcome are valid
    # Return hospital name in that state with given rank 30-day death rate
    
    df.outcome <- read.csv("~/code/rprogramming/PA3/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses="character");
    
    valid_states <- attributes(factor(df.outcome$State))$levels;
    valid_outcome_names <- c("heart attack", "heart failure", "pneumonia");
    
    if(!(state %in% valid_states)){
        stop("invalid state");
    }
    
    if(!(outcome %in% valid_outcome_names)){
        stop("invalid outcome"); 
    }
    
    outcomes <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
                  "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
                  "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia");
    
    names(outcomes) <- valid_outcome_names;
    
    df.outcome <- subset(df.outcome, df.outcome$State == state, select = c("Hospital.Name", outcomes[outcome]));
    
    df.outcome[df.outcome == "Not Available"] <- NA;
    
    df.outcome <- na.omit(df.outcome);
    
    v.order <- vector();
    if(num == "best"){
        v.order <- order(df.outcome[, 2], df.outcome[, 1]);
        df.outcome <- df.outcome[v.order,];
        df.outcome$Rank <- 1:length(df.outcome$Hospital.Name);
        return(df.outcome[1, 1]);
        
    }else if(num == "worst"){
        v.order <- order(df.outcome[, 2], df.outcome[, 1], decreasing = TRUE);
        df.outcome <- df.outcome[v.order,];
        df.outcome$Rank <- length(df.outcome$Hospital.Name):1;
        return(df.outcome[length(nrow(df.outcome)), 1]);
    }
    
    if(is.numeric(num)){
        if(num > nrow(df.outcome) || num < 1){
            return(NA);
        }
        
        v.order <- order(df.outcome[, 2], df.outcome[, 1]);
        df.outcome <- df.outcome[v.order,];
        df.outcome$Rank <- 1:length(df.outcome$Hospital.Name); 
        
        return(df.outcome[num, 1]);
    }
}