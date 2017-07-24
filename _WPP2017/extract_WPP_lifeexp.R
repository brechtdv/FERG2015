### UN/WPP LIFE EXPECTANCY DATA
### 22/07/2017

### https://esa.un.org/unpd/wpp/Download/Standard/Interpolated/

## packages
options(java.parameters = "-Xmx4g")
library(XLConnect)

## country names
names <- readWorksheetFromFile("country-names-20170722.xlsx", 1)
all(diff(names$ID.new) == 1)  ## correct order?

## population files
fileM <- "WPP2017_MORT_F16_2_LIFE_EXPECTANCY_BY_AGE_MALE.xlsx"
fileF <- "WPP2017_MORT_F16_3_LIFE_EXPECTANCY_BY_AGE_FEMALE.xlsx"

### MALES -----------------------------------------------------------------#

## read worksheet
x <- readWorksheetFromFile(fileM, "ESTIMATES", startRow = 17, startCol = 3)
str(x); names(x)

## initial cleanup
x[, 2:3] <- NULL  # delete 'Notes', 'Country.code'

y <- x[, 1:2]
names(y) <- c("country", "period")
str(y)

x[, 1:2] <- NULL
str(x); names(x)

## only period 2010-2015
x <- subset(x, y$period == "2010-2015"); str(x)
y <- subset(y, y$period == "2010-2015"); str(y)

## create FERG age groups -> drop X100.
x$X100. <- NULL
lifeexpM <- x

### FEMALES ---------------------------------------------------------------#

## read worksheet
x <- readWorksheetFromFile(fileF, "ESTIMATES", startRow = 17, startCol = 3)
str(x); names(x)

## initial cleanup
x[, 2:3] <- NULL  # delete 'Notes', 'Country.code'

y <- x[, 1:2]
names(y) <- c("country", "period")
str(y)

x[, 1:2] <- NULL
str(x); names(x)

## only period 2010-2015
x <- subset(x, y$period == "2010-2015"); str(x)
y <- subset(y, y$period == "2010-2015"); str(y)

## create FERG age groups -> drop X100.
x$X100. <- NULL
lifeexpF <- x
str(lifeexpF)

### ------------------------------------------------------------------------#

## prepare population data array
LE_FERG <- vector("list", 194)
names(LE_FERG) <- names$FERG

## define ages
age <- as.numeric(gsub(fixed = T, ".", "", gsub("X.", "", names(lifeexpM))))

## fill FERG life expectancy array
for (i in seq(194)) {
  id <- which(y$country == names$UN[i])
  if (!length(id) == 0) {
    LE_FERG[[i]] <- cbind(age,
                          male = unlist(lifeexpM[id, ], use.names = FALSE),
                          female = unlist(lifeexpF[id, ], use.names = FALSE))
  } else {
    print(names[i, ])
  }
}

LE_FERG


### ------------------------------------------------------------------------#

## add small countries
small <- names$FERG[names$UN == "NA"]
small

## obtain LE per subregion
sub <-
  sapply(t(tapply(LE_FERG, names$WHOsub, unlist),
           matrix, nrow = 3*length(age)))
sub <- sapply(sub, rowMeans)

subM <- sub[22:42, ]
subF <- sub[43:63, ]

## fill in imputed LE structures for small countries
LE_FERG_imp <- LE_FERG
for (i in seq(small)) {
  ferg_id <- which(names$FERG == small[i])
  i_sub <- names$WHOsub[ferg_id]
  sub_id <- which(dimnames(sub)[[2]] == i_sub)
  LE_FERG_imp[[ferg_id]] <- cbind(age, subM[, sub_id], subF[, sub_id])
}

### ------------------------------------------------------------------------#

save(LE_FERG_imp, file = "LE_FERG_imp_2015.RData")


### ------------------------------------------------------------------------#

library(ggplot2)
LEplot <-
function(id) {
  df <- data.frame(lifeexp = c(LE_FERG_imp[[id]][, 2:3]),
                   sex = rep(factor(c("male", "female"), c("male", "female")),
                             each = 21),
                   age = rep(age, times = 2))
  ggplot(df, aes(y = lifeexp, x = age, color = sex)) +
    geom_point() +
    geom_line() +
    ggtitle(names$FERG[id]) +
    theme_bw() +
    scale_x_continuous("Exact age x (years)") +
    scale_y_continuous("Life expectancy") +
    labs(subtitle = "UN World Population Prospects 2017 Revision") +
    coord_cartesian(ylim = c(0, 85))
}

pdf("lifeexp2015.pdf", 6, 4)
for (i in seq(194)) {
  print(LEplot(i))
}
graphics.off()

