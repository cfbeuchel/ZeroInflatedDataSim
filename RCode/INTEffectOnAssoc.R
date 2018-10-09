# weird associations after INT

# !!!
# first get the combat.data from STEP1 for combat run 251 with the original scenarios
# !!!

# save a logical vector with the zero position when doing no batch adjustment 
# --> run this before transforming!
zero.index <- lapply(c(81, 85, 89, 93), function(i){
  my.y <- paste0("y.0infl.", i)
  combat.data[,  get(my.y) == 0]
})

fit_plots <- function(combat.data, pdf_name){
  # ComBated data
  pdf(pdf_name, width = 14)
  par(mfrow=c(1,2))
  for (i in c(81, 85, 89, 93)) {
    
    # very ugly ad hoc indexing
    my.zero.infl <- c(0,20,50,80)
    dummy.index <- c(81, 85, 89, 93)
    my.dummy.index <- which(i == dummy.index)
    
    # set indices
    my.y <- paste0("y.0infl.", i)
    my.x <- paste0("x.", i)
    my.y.tr <- paste0("y.0infl.", i, ".transformed")
    
    # fit lm
    lm <- lm(get(my.y) ~ get(my.x), data = combat.data)
    
    # coeffs for lm
    rmse <- round(sqrt(mean(resid(lm)^2)), 2)
    coefs <- coef(lm)
    b0 <- round(coefs[1], 2)
    b1 <- round(coefs[2],2)
    r2 <- round(summary(lm)$r.squared, 2)
    
    # Now build up the equation using constructs described in ?plotmath:
    eqn <- bquote(italic(beta) == .(b1) * "," ~~ 
                    r^2 == .(r2) * "," ~~ RMSE == .(rmse))
    
    
    lm.tr<- lm(get(my.y.tr) ~ get(my.x), data = combat.data)
    
    # coeffs for lm
    rmse.tr <- round(sqrt(mean(resid(lm.tr)^2)), 2)
    coefs.tr <- coef(lm.tr)
    b0.tr <- round(coefs.tr[1], 2)
    b1.tr <- round(coefs.tr[2],2)
    r2.tr <- round(summary(lm.tr)$r.squared, 2)
    
    # Now build up the equation using constructs described in ?plotmath:
    eqn.tr <- bquote(italic(beta) == .(b1.tr) * "," ~~ 
                       r^2 == .(r2.tr) * "," ~~ RMSE == .(rmse.tr))
    
    # set range
    my.range <- combat.data[, range(.SD), .SDcols = my.y.tr]
    my.range.diff <- c(0,1,1,1)
    my.range[1] <- my.range[1] - my.range.diff[my.dummy.index]
    
    # untransformed
    combat.data[ , plot(get(my.x), get(my.y),
                        main = paste0("Untransformed, ",
                                      my.zero.infl[my.dummy.index],
                                      "% zero-inflated data"),
                        sub = eqn,
                        xlab = "X",
                        ylab = paste0("Y, ", my.zero.infl[my.dummy.index],"% zero-inflated"),
                        ylim = my.range,
                        pch = 20,
                        col = ifelse(zero.index[[my.dummy.index]] == T,'brown4','grey40'))]
    combat.data[ , abline(lm, lwd = 3, col = "red")]
    
    # transformed
    combat.data[ , plot(get(my.x), get(my.y.tr), main = paste0("INT, ",
                                                               my.zero.infl[my.dummy.index],
                                                               "% zero-inflated data"),
                        sub = eqn.tr,
                        xlab = "X",
                        ylab = paste0("Y, ", my.zero.infl[my.dummy.index],"% zero-inflated"),
                        ylim = my.range,
                        pch = 20,
                        col = ifelse(zero.index[[my.dummy.index]] == T,'brown4','grey40'))]
    
    combat.data[ , abline(lm.tr, lwd = 3, col = "red")]
    
  }
  par(mfrow=c(1,1))
  dev.off()
}

# plot when INT before ComBat
fit_plots(combat.data = combat.data, pdf_name = "zero_inflated_fit_INT_pre_ComBat.pdf")
fit_plots(combat.data = combat.data, pdf_name = "zero_inflated_fit_INT_pre_no_ComBat.pdf")

# create the data for combat run 251 twice! NEEDS TO BE DONE MANUALLY
# with combat and transformation
fit_plots(combat.data = combat.data, pdf_name = "zero_inflated_fit_AFTER_ComBat.pdf")

# without ComBat but including the transformation (omit ComBat manually here -> see effect of INT on zeros)
fit_plots(combat.data = combat.data, pdf_name = "zero_inflated_fit_NO_ComBat.pdf")
