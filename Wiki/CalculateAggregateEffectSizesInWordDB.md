This document provides a guide to calculate effect sizes and aggregate them across repeated measures from the Community-Augmented Meta-Analysis InWordDB, which addresses infants' ability to segment words from continuous, native speech. For details, see the companion website at [<http://inworddb.acristia.org>](http://inworddb.acristia.or).

*Make sure you have preprocessed the database first!*

Step 1: Estimate Effect Size
----------------------------

This is the most important bit. So let's go thorugh it step by step.

This is a repeated measure, akin to a pre-post contrast therefore, we use the *standardized mean gain* set of formulas whenever possible

``` {.r}
db$LTDif <- ifelse(db$formula2use == "means&SD", db$LTFam - db$LTNov, NA)
```

Compute pooled SD to use in Effect Size measures. Source: Lipsey & Wilson, page 44 - from Becker 1988

``` {.r}
db$PooledSD <- ifelse(db$formula2use == "means&SD", sqrt((db$SD.LTFam^2 + db$SD.LTNov^2)/2), 
    NA)
```

Compute Effect Size based on means and SD of dependent variables where possible. Source: Lipsey & Wilson, formula 3.14

``` {.r}
db$ES <- ifelse(db$formula2use == "means&SD", db$LTDif/db$PooledSD, NA)
```

This is an approximation of the same ES using exact t values We do NOT use formula 2/3 from Table B10 in Lipsey & Wilson because it is based on independent samples Instead, we follow Dunlap et al. 1996, page 171

``` {.r}
db$ES <- ifelse(db$formula2use == "tValue", db$exact.t * sqrt(2 * (1 - db$correlationFamNov.imputed)/db$Included), 
    as.numeric(as.character(db$ES)))
```

Since many of these samples are smaller than 20, we calculate Hedges' g, which corrects for small sample sizes and thus unbiases our effect sizes. Source: Morris 2010, p. 21

``` {.r}
db$ESg = db$ES * (1 - (3/(4 * db$Included - 5)))
```

We also need the Standard Error (SE) to know the variance of an effect size. We follow Lipsey & Wilson, formula 3.15; notice that others divide by N-1 in the last term

``` {.r}
db$ES.SE <- sqrt((2 * (1 - db$correlationFamNov.imputed)/db$Included) + (db$ES^2)/(2 * 
    db$Included))
db$ESg.SE <- sqrt((2 * (1 - db$correlationFamNov.imputed)/db$Included) + (db$ESg^2)/(2 * 
    db$Included))
```

We want each effect size to be weighted in regressions according to how *reliable* it is. So we compute the weights according to Lipsey & Wilson, formula 3.16

``` {.r}
db$ES.W = 1/(db$ES.SE^2)
db$ESg.W = 1/(db$ESg.SE^2)
```

Step 2: Aggregate Effect Sizes
------------------------------

First, we remove all records for which no effect sizes could be calculated. `db <- db[!is.na(db$ESg),]`

Next, we go through each unique infant group (defined as the reference concatenated with the infant group tested) and store a median ES for the independent ES calculations

``` {.r}
db.noRM <- NULL

for (eachunique in levels(factor(db$unique))) {
    nowdoing <- subset(db, unique == eachunique)
    nowdoing$ES <- median(nowdoing$ESg, na.rm = T)
    nowdoing$ES.SE <- median(nowdoing$ESg.SE, na.rm = T)
    nowdoing$ES.W <- median(nowdoing$ESg.W, na.rm = T)
    nowdoing$ESg <- median(nowdoing$ESg, na.rm = T)
    nowdoing$ESg.SE <- median(nowdoing$ESg.SE, na.rm = T)
    nowdoing$ESg.W <- median(nowdoing$ESg.W, na.rm = T)
    
    nowdoing$proportionFamPref <- median(nowdoing$proportionFamPref, na.rm = T)
    
    for (i in 1:dim(db)[2]) if (length(levels(factor(nowdoing[, i]))) > 1) 
        nowdoing[, i] <- NA
    db.noRM <- rbind(db.noRM, nowdoing[1, ])
}  # Closing FOR-loop
```

Finally, we exclude ES that are more than +-3SD from mean ES following standard meta-analytic practice.

``` {.r}
db.noRM$include <- ifelse(db.noRM$ESg > mean(db.noRM$ESg, na.rm = TRUE) + 3 * 
    sd(db.noRM$ESg, na.rm = TRUE) | db.noRM$ESg < mean(db.noRM$ESg, na.rm = TRUE) - 
    3 * sd(db.noRM$ESg, na.rm = TRUE), F, T)

db.noOutnoRM = db.noRM[db.noRM$include, ]
```

And with `db.noOutnoRM` we have the final sample where repeated measures are collapsed into one record. This way, we count the contribution from each unique infant only once.
