# set parameters for simulation, e.g. effect size, n...
set.seed(0815)
effekte = c(0.01,0.02, 0.05, 0.1, 0.3)
n = 5000
proz0 = c(20, 50, 80)
realisierungen = 1000
categart = c("quantile","bereiche")
categnum = c(2:10)

# create dt with all scenario combinations
szenarien = data.table(expand.grid(effekte=effekte, n=n, proz0=proz0, categart=categart, categnum=categnum))
szenarien[,num := 1:.N]
szenarien
# myzeile <- szenarien[1,] # for testing

# apply over each szenario
apply(szenarien[1:5,], 1, function(x){
  
  # for readability, assign parameters of each scenario
  effekt <- x[1]
  n <- x[2]
  proz0 <- x[3]
  categart <- x[4]
  categnum <- x[5]
  
  # create data for scenario
  df <- data.table(x = rnorm(n))
  df[, y := x * effekt + rnorm(n) * (1 - effekt) + 2]
  
  # fit lm
  linmod <- df[, summary(lm(y ~ x))]
  
  # add summary statistics
  res <- data.table(effekt = effekt,
                    n = n,
                    proz0 = proz0,
                    categart = categart,
                    categnum = categnum,
                    linmod.r2 = linmod$r.squared,
                    linmod.pval = linmod$coefficients["x","Pr(>|t|)"],
                    linmod.beta = linmod$coefficients["x","Estimate"])
  
  # create zero-inflated y-derived data
  df[, y.0infl := y]
  df[y < quantile(y.0infl, probs = proz0/100), y.0infl := 0]
  min.found = df[y.0infl > 0, min(y.0infl)]
  df[y.0infl > 0, y.0infl := y.0infl - 0.99*min.found]
  
  # check
  # df[,.N,y_0infl==0]
  # hist(df$y.0infl, breaks = 50)
  
  # sort by y (category)
  setorder(df, y)
  
  # create categories to calculate polr/ordinal over
  if(categart == "quantile") {
    df[y.0infl > 0, y.0infl.categ := cut(y.0infl, breaks = quantile(y.0infl, probs = seq(0, 1, 1/categnum)), include.lowest = T)]
    categs.created = df[, levels(y.0infl.categ)]
    df[y.0infl == 0, y.0infl.categ := factor(0, labels =  "[0]")]
    df[, y.0infl.categ := factor(y.0infl.categ, levels = c("[0]", categs_created))]
  } else if(categart == "bereiche"){
    
    # categories separating 
  } else {
    stop("Error")
  }
  
})

require(broom)

myzeile  = x
df = data.table(x = rnorm(myzeile$n))
df[,y := x*myzeile$effekte + rnorm(myzeile$n)*(1- myzeile$effekte) +2]

linmod = df[,summary(lm(y~x))]
myzeile$linmod_r2 = linmod$r.squared
myzeile$linmod_pval = linmod$coefficients["x","Pr(>|t|)"]
myzeile$linmod_beta = linmod$coefficients["x","Estimate"]

df[,y_0infl := y]
df[ y<quantile(y_0infl, probs = myzeile$proz0/100), y_0infl := 0]
min_found = df[ y_0infl > 0, min(y_0infl)]
df[ y_0infl > 0, y_0infl := y_0infl - 0.99*min_found]

df[,.N,y_0infl==0]

# hist(df$y_0infl, breaks = 50)

setorder(df, y)
if(myzeile$categart == "quantile") {
  df[y_0infl > 0, y_0infl_categ := cut(y_0infl, breaks = quantile(y_0infl, probs = seq(0,1,1/myzeile$categnum)), include.lowest = T)]
  categs_created = df[,levels(y_0infl_categ)]
  df[y_0infl == 0, y_0infl_categ := factor(0, labels =  "[0]")]
  df[, y_0infl_categ := factor(y_0infl_categ, levels = c("[0]", categs_created))]
  }


## dichotomisiert 0 vs not 0 als glm
