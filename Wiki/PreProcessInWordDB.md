This document provides a guide to reading in and preprocessing the raw Community-Augmented Meta-Analysis InWordDB, which addresses infants' ability to segment words from continuous, native speech. For details, see the companion website at [<http://inworddb.acristia.org>](http://inworddb.acristia.or).

Step 1: Read in the raw database
--------------------------------

The most recent database is always provided in the Files section under the name *InWordDB.csv*.

You can download and open it with the following command in *R*: `db<-read.csv("InWordDB.csv")`.

Now you can inspect the data in R, for example checking the first few column names using `head(names(db))` or asking how many entries there are with `length(row.names(db))`.

You can also check how many native languages are represented in the database.

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> 
Native Language
</th>
   <th style="text-align:right;"> 
Number
</th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 
African American English
</td>
   <td style="text-align:right;"> 
2
</td>
  </tr>
  <tr>
   <td style="text-align:left;"> 
American English
</td>
   <td style="text-align:right;"> 
154
</td>
  </tr>
  <tr>
   <td style="text-align:left;"> 
British English
</td>
   <td style="text-align:right;"> 
2
</td>
  </tr>
  <tr>
   <td style="text-align:left;"> 
Canadian English
</td>
   <td style="text-align:right;"> 
3
</td>
  </tr>
  <tr>
   <td style="text-align:left;"> 
Canadian French
</td>
   <td style="text-align:right;"> 
22
</td>
  </tr>
  <tr>
   <td style="text-align:left;"> 
Catalan
</td>
   <td style="text-align:right;"> 
2
</td>
  </tr>
  <tr>
   <td style="text-align:left;"> 
Catalan and Spanish
</td>
   <td style="text-align:right;"> 
2
</td>
  </tr>
  <tr>
   <td style="text-align:left;"> 
Dutch
</td>
   <td style="text-align:right;"> 
2
</td>
  </tr>
  <tr>
   <td style="text-align:left;"> 
German
</td>
   <td style="text-align:right;"> 
7
</td>
  </tr>
  <tr>
   <td style="text-align:left;"> 
Mandarin
</td>
   <td style="text-align:right;"> 
1
</td>
  </tr>
  <tr>
   <td style="text-align:left;"> 
Mandarin and English
</td>
   <td style="text-align:right;"> 
12
</td>
  </tr>
  <tr>
   <td style="text-align:left;"> 
Parisian French
</td>
   <td style="text-align:right;"> 
17
</td>
  </tr>
  <tr>
   <td style="text-align:left;"> 
Spanish
</td>
   <td style="text-align:right;"> 
2
</td>
  </tr>
  <tr>
   <td style="text-align:left;"> 
Turkish
</td>
   <td style="text-align:right;"> 
1
</td>
  </tr>
</tbody>
</table>


Step 2: Preprocess the database
-------------------------------

This step improves some aspects of the way the data are represented in the database.

First, we make a factor out of the coded moderator variable *Native Language*.

``` {.r}
db$NativeLanguage = factor(db$NativeLanguage)
```

Then, we fix *infant type*, bilinguals were not noted there (this variable was previously only used to keep track of typically developing infants).

``` {.r}
db$InfantType <- ifelse(db$NativeLanguage == "Mandarin and English", "bilingual", 
    as.character(db$InfantType))
db$InfantType <- ifelse(db$NativeLanguage == "Catalan and Spanish", "bilingual", 
    as.character(db$InfantType))
db$InfantType = factor(db$InfantType)
```

Now on to adding some info: proportion of babies included, a variable to code infant group (to track repeated measures)

``` {.r}
db$propIncl = db$Included/(db$Excluded + db$Included)
db$unique = paste(db$DOI, db$InfantGroup, sep = "_")
```

Cast everything that should be a number into a number (and not a string of characters)

``` {.r}
db$meanAge = as.numeric(as.character(db$meanAge))
db$exact.t = as.numeric(as.character(db$exact.t))
db$proportionFamPref = as.numeric(as.character(db$proportionFamPref))
db$LTFam = as.numeric(as.character(db$LTFam))
db$LTNov = as.numeric(as.character(db$LTNov))
```

Correlations of infant behavior across conditions are crucial for estimating effect size weights in within-subject designs. They inform us how systematic participants behaved. These correlations are almost never reported. We obtained correlations from the original authors of some database entries (when we contacted them for other information). We add missing correlations using a randomly selected value from the ones we have available.

We need a random seed for reproducibility. Now *impute* from the library *Hmisc* will always yield the same result, unless the seed is changed.

``` {.r}
set.seed(111)
db$correlationFamNov.imputed<-impute(db$correlationFamNov,fun="random")
```

Step 3: Add variable: Task Difficulty
-------------------------------------

To check whether older infants are exposed to more difficult experiments, we coded difficulty in the database, based on factors typically reported. The difficulty score is the sum of: linguistic difficulty (0 if the phonological form of the target word was identical across familiarization and test, 1 otherwise – e.g., ham-hamlet receives a score of 1); sentence alignment (0 if the target word was always aligned with a sentence edge, 1 otherwise); stress alignment (words that did not follow the predominant stress pattern for content words in the relevant language – English, German, and Dutch are largely stress-initial, Turkish stress-final – were given a score of 1, otherwise the score was 0); and indexical properties (0 if they were purposefully matched; 2 if they were purposefully changed between familiarization and test; and 1 if they were not controlled).

``` {.r}
ling_difficulty <- ifelse(db$Linguistic == "match", 0, 1)  #Does the target word match exactly in phonological form across familiarization and test 
align_difficulty <- ifelse(db$EdgeAlignSimple == 1, 0, 1)  #Is the target word always aligned with a sentence edge, in which case difficulty is 0
align_difficulty[is.na(db$EdgeAlignSimple)] <- 1  #When edge alignment was not available assume that the target was NOT aligned with a sentence edge in all cases
index_difficulty <- ifelse(db$Indexical == "match", 0, 2)  #In studies where indexical properties (pitch, voice, affect) are manipulated, the conditions having a precise match get zero, those with a mismatch get a 2
index_difficulty[is.na(db$Indexical)] <- 1  #All studies where indexical properties were not mentioned get a 1
index_difficulty[db$Indexical == "not.manipulated"] <- 1  #All studies where indexical properties were not controlled for get a 1

# Calculate total difficulty score.
db$difficulty <- ling_difficulty + align_difficulty + index_difficulty + db$WdMisAlignMajorStress
```
