bsp =combat.data.raw[[81]]
hist(bsp$y.0infl.81)
plot(bsp$y.0infl.81 , bsp$y.81)
abline(0,1,col='red')
norm_plot2(bsp$y.0infl.81)


bsp[,summary(lm(y.0infl.81 ~ x.81))]


bsp[, y_transformed := orderNorm(y.81)$x.t]
bsp[, str(orderNorm(y.81))]

norm_plot2(bsp$y_transformed)

bsp[,summary(lm(y_transformed ~ x.81))]

bsp[,plot(y_transformed ~ y.81)]
abline(0,1,col='red')

bsp[,plot(scale(y_transformed, scale = F) ~ scale(y.81, scale = F))]
abline(0,1,col='red')


bsp[,summary(lm(y_transformed ~ y.81))]

bsp[,sd(y_transformed)]
bsp[,sd(y.81)]

require(broom)

bsp[,tidy(summary(lm(scale(y.81) ~ x.81)))]
bsp[,tidy(summary(lm(y.81 ~ x.81)))]


orisd = bsp[,sd(y.0infl.81)]
orisd 

bsp[,y_transformed_rescaled := sd(y.0infl.81)*y_transformed] ## TODO in Simulation
bsp[,tidy(summary(lm(y_transformed ~ x.81)))]


bsp[,tidy(summary(lm(y_transformed_rescaled  ~ x.81)))]


norm_plot2(bsp$y_transformed_rescaled)
