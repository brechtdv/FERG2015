### UN/WPP POPULATION DATA
### 22/07/2017

### https://esa.un.org/unpd/wpp/Download/Standard/Interpolated/
### .. files are too large to read -> manually delete 'MEDIUM VARIANT'

## packages
options(java.parameters = "-Xmx4g")
library(XLConnect)
library(FERG2015)

## country names
names <- readWorksheetFromFile("country-names-20170722.xlsx", 1)
all(diff(names$ID.new) == 1)  ## correct order?

## population files
fileM <- "WPP2017_INT_F03_2_POPULATION_BY_AGE_ANNUAL_MALE.xlsx"
fileF <- "WPP2017_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE.xlsx"

### MALES -----------------------------------------------------------------#

## read worksheet
wbM <- loadWorkbook(fileM)
setMissingValue(wbM, "…")

x <- readWorksheet(wbM, "ESTIMATES", startRow = 17, startCol = 3)
str(x); names(x)

## initial cleanup
x[, 2:3] <- NULL

y <- x[, 1:2]
names(y) <- c("country", "year")
str(y)

x[, 1:2] <- NULL
x$X80. <- NULL
str(x); names(x)

## only year 2015
x <- subset(x, y$year == 2015); str(x)
y <- subset(y, y$year == 2015); str(y)

## create FERG age groups
popM <- x[1]  # $X0
popM$X1 <- rowSums(x[, 2: 5])
popM$X5 <- rowSums(x[, 6:10]) 
popM$X10 <- rowSums(x[, 11:15])
popM$X15 <- rowSums(x[, 16:20])
popM$X20 <- rowSums(x[, 21:25])
popM$X25 <- rowSums(x[, 26:30])
popM$X30 <- rowSums(x[, 31:35])
popM$X35 <- rowSums(x[, 36:40])
popM$X40 <- rowSums(x[, 41:45])
popM$X45 <- rowSums(x[, 46:50])
popM$X50 <- rowSums(x[, 51:55])
popM$X55 <- rowSums(x[, 56:60])
popM$X60 <- rowSums(x[, 61:65])
popM$X65 <- rowSums(x[, 66:70])
popM$X70 <- rowSums(x[, 71:75])
popM$X75 <- rowSums(x[, 76:80])
popM$X80 <- rowSums(x[, 81:85])
popM$X85. <- rowSums(x[, 86:101])

sum(popM); sum(x)

## clean-up
rm(wbM); gc()

### FEMALES ---------------------------------------------------------------#

## read worksheet
wbF <- loadWorkbook(fileF)
setMissingValue(wbF, "…")

x <- readWorksheet(wbF, "ESTIMATES", startRow = 17, startCol = 3)
str(x); names(x)

## initial cleanup
x[, 2:3] <- NULL

y <- x[, 1:2]
names(y) <- c("country", "year")
str(y)

x[, 1:2] <- NULL
x$X80. <- NULL
str(x); names(x)

## only year 2015
x <- subset(x, y$year == 2015); str(x)
y <- subset(y, y$year == 2015); str(y)

## create FERG age groups
popF <- x[1]  # $X0
popF$X1 <- rowSums(x[, 2: 5])
popF$X5 <- rowSums(x[, 6:10]) 
popF$X10 <- rowSums(x[, 11:15])
popF$X15 <- rowSums(x[, 16:20])
popF$X20 <- rowSums(x[, 21:25])
popF$X25 <- rowSums(x[, 26:30])
popF$X30 <- rowSums(x[, 31:35])
popF$X35 <- rowSums(x[, 36:40])
popF$X40 <- rowSums(x[, 41:45])
popF$X45 <- rowSums(x[, 46:50])
popF$X50 <- rowSums(x[, 51:55])
popF$X55 <- rowSums(x[, 56:60])
popF$X60 <- rowSums(x[, 61:65])
popF$X65 <- rowSums(x[, 66:70])
popF$X70 <- rowSums(x[, 71:75])
popF$X75 <- rowSums(x[, 76:80])
popF$X80 <- rowSums(x[, 81:85])
popF$X85. <- rowSums(x[, 86:101])

sum(popF); sum(x)

rm(wbF, x); gc()

### ------------------------------------------------------------------------#

## prepare population data array
pop_FERG <- array(dim = c(19, 2, 194))

## fill FERG population array
for (i in seq(194)) {
  id <- which(y$country == names$UN[i])
  if (!length(id) == 0) {
    pop_FERG[, 1, i] <- 1000 * unlist(popM[id, ], use.names = FALSE)
    pop_FERG[, 2, i] <- 1000 * unlist(popF[id, ], use.names = FALSE)

  } else {
    print(names[i, ])
  }
}

pop_FERG

### ------------------------------------------------------------------------#

## add small countries
names$FERG[names$UN == "NA"]

small <- readWorksheetFromFile("small-countries-20170722.xlsx", 1)
prop.table(c(sum(small$POP), sum(pop_FERG, na.rm = TRUE)))

## obtain mean population structures per subregion
sub <-
  apply(pop_FERG, 1:2, function(x) tapply(x, names$WHOsub, sum, na.rm = T))
str(sub)

sum(pop_FERG[, , names$WHOsub == "EUR A"], na.rm = T)
sum(sub["EUR A", , ])

sum(pop_FERG[, 1, names$WHOsub == "EUR A"], na.rm = T)
sum(sub["EUR A", , 1])

sum(pop_FERG[1, , names$WHOsub == "EUR A"], na.rm = T)
sum(sub["EUR A", 1, ])

## fill in imputed population structures for small countries
pop_FERG_imp <- pop_FERG
for (i in seq(nrow(small))) {
  ferg_id <- which(names$FERG == small$COUNTRY[i])
  i_sub <- names$WHOsub[ferg_id]
  sub_id <- which(dimnames(sub)[[1]] == i_sub)
  pop_FERG_imp[, , ferg_id] <- small$POP[i] * prop.table(sub[sub_id, , ])
}

sum(small$POP) / sum(pop_FERG_imp)


### ------------------------------------------------------------------------#

save(pop_FERG_imp, file = "pop_FERG_imp_2015.RData")


### ------------------------------------------------------------------------#

library(ggplot2)
pyramid <-
function(id) {
  df <- data.frame(population = c( pop_FERG_imp[, 1, id],
                                  -pop_FERG_imp[, 2, id]),
                   sex = rep(factor(c("male", "female"), c("male", "female")),
                             each = 19),
                   age = rep(factor(names(popF), names(popF)), times = 2))
  ggplot(df, aes(y = population, x = age, fill = sex)) +
    geom_col() +
    coord_flip() +
    ggtitle(sprintf(paste(names$FERG[id], "(%s)"),
                    formatC(sum(pop_FERG_imp[, , id]),
                            format = "f", big.mark = " ", digits = 0))) +
    theme_bw()
}

pdf("pop2015.pdf")
for (i in seq(194)) {
  subtitle <-
    ifelse(names$UN[i] == "NA",
           "CIA World Factbook, July 2016 estimate",
           "UN World Population Prospects 2017 Revision")
  print(pyramid(i) + labs(subtitle = subtitle))
}
graphics.off()
