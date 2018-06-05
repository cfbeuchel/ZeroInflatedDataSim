withNas$linmod.pval.zero.inflated

withNas[, p.val.with.zeros :=withZeros$linmod.pval.zero.inflated]

withZeros$linmod.pval.zero.inflated


test.1 <- withNas
q_5("p.val.with.zeros","linmod.pval.zero.inflated" )
