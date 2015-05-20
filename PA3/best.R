best <- function(state, outcome){
    
    ## Read outcome datat
    ## Check that state and outcome are valid
    ## Return hospital name in thet state with lowest 30-day death rate
    
    df.outcome <- read.csv("~/code/rprogramming/PA3/rprog_data_ProgAssignment3-data//outcome-of-care-measures.csv", colClasses="character");
    
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
    
    df.outcome[which.min(apply(df.outcome, MARGIN = 1, min)), 1];
}