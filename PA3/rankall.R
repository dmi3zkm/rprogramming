rankall <- function(outcome, num = "best"){
    # Read outcome data
    # Check that state and outcome are valid
    # For each state, find the hospital of the given rank
    # Return a data frame with the hospital names and abbreviated state name
    
    df.outcome.init <- read.csv("~/code/rprogramming/PA3/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", 
                           colClasses="character");
    
    #df.outcome.init[df.outcome.init == "Not Available"] <- NA;
    #df.outcome.init <- na.omit(df.outcome.init);
    
    valid_states <- attributes(factor(df.outcome.init$State))$levels;
    valid_outcome_names <- c("heart attack", "heart failure", "pneumonia");
    
    if(!(outcome %in% valid_outcome_names)){
        stop("invalid outcome"); 
    }
    
    outcomes <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
                  "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
                  "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia");
    
    names(outcomes) <- valid_outcome_names;
    
    df.result <- data.frame(hospital=character(), state=character(), stringsAsFactors=FALSE);
    for(valid_state in valid_states){
        
        df.outcome <- subset(df.outcome.init, df.outcome.init$State == valid_state, select = c("Hospital.Name", outcomes[outcome]));
        
        df.outcome[df.outcome == "Not Available"] <- NA;
        df.outcome <- na.omit(df.outcome);
        
        df.outcome[,outcomes[outcome]] <- as.numeric(df.outcome[,outcomes[outcome]])
        
         if(num == "best"){
             
             v.order <- order(df.outcome[, 2], df.outcome[, 1]);
             df.outcome <- df.outcome[v.order,];
             df.result <- rbind(df.result, data.frame("hospital"= df.outcome[1, 1], "state" = valid_state));
             next;
                 
        }else if(num == "worst"){
            v.order <- order(df.outcome[, 2], df.outcome[, 1], decreasing = TRUE);
            df.outcome <- df.outcome[v.order,];
            df.result <- rbind(df.result, data.frame("hospital"= df.outcome[1, 1], "state" = valid_state));
            next;
        }
        
        if(is.numeric(num)){
            
            v.order <- order(df.outcome[, 2], df.outcome[, 1]);
            df.outcome <- df.outcome[v.order,];
            df.outcome$Rank <- 1:length(df.outcome$Hospital.Name);
            
            if(num > nrow(df.outcome)){
                df.result <- rbind(df.result, data.frame("hospital"= NA, "state" = valid_state));
                next;
            }
            
            df.result <- rbind(df.result, data.frame("hospital"= df.outcome[num, 1], "state" = valid_state));
            next;
        }
    }
    
    df.result;
}