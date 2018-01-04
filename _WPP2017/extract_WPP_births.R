### UN/WPP BIRTHS DATA
### 02/01/2018

### https://esa.un.org/unpd/wpp/Download/Standard/Interpolated/

## required packages
library(bd)
library(readxl)
library(FERG2015)

## country names
names <- readxl("country-names-20170722.xlsx")
all(diff(names$ID.new) == 1)  ## correct order?

## population files
file <- "WPP2017_INT_F01_ANNUAL_DEMOGRAPHIC_INDICATORS.xlsx"

## read worksheet
x <- readxl(file, "ESTIMATES", range = cell_limits(c(17, 3), c(NA, NA)))
str(x); names(x)

## cleanup
y <- x[, c(1, 4, 15)]
names(y) <- c("country", "year", "births")
y <- subset(y, y$year == 2015)
str(y); head(y)

### ------------------------------------------------------------------------#

## prepare births vector
births_FERG <- numeric(194)

## fill FERG births vector
for (i in seq(194)) {
  id <- which(y$country == names$UN[i])
  if (!length(id) == 0) {
    births_FERG[i] <- 1000 * y[id, "births"]

  } else {
    print(names[i, ])
  }
}

births_FERG

### ------------------------------------------------------------------------#

## add small countries
names$FERG[names$UN == "NA"]

small <- readxl("small-countries-20180102.xlsx")
prop.table(c(sum(small$BIRTHS), sum(births_FERG, na.rm = TRUE)))

## fill in births for small countries
births_FERG_full <- births_FERG
for (i in seq(nrow(small))) {
  ferg_id <- which(names$FERG == small$COUNTRY[i])
  births_FERG_full[ferg_id] <- small$BIRTHS[i]
}

sum(small$BIRTHS) / sum(births_FERG_full)

cbind(names$FERG, round(births_by_country_2015))


### ------------------------------------------------------------------------#

births_by_country_2015 <- births_FERG_full
save(births_by_country_2015, file = "births_by_country_2015.RData")
