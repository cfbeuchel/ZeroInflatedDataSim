batch_anova <- function(data, to.adjust, batch) {
  
  #=================================================================#
  # ANOVA - BATCH #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # check the batch effect 
  
  # loop through all parameters
  aov.results <- sapply(to.adjust, function(my.param) {
    
    # custom formula for each parameter
    my.formula <- as.formula(paste0(my.param, " ~ ", batch))
    
    # calculate anova
    my.aov <- anova(aov(my.formula, data = data))
    
    # clean output
    clean.aov <- as.data.table(broom::tidy(my.aov))
    single.aov.results <- clean.aov[term == (batch), p.value]
    
    # return p.vals
    return(single.aov.results)
  }) # end ANOVA sapply
  
  # summary ANOVA results
  aov.res <- data.table(parameter = names(aov.results),
                        p.values = aov.results)
  return(aov.res)
  # END: ANOVA - BATCH #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #=================================================================#
}