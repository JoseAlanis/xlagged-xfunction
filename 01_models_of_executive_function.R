
# set working dir
if(interactive()){
  path <- dirname(rstudioapi::getSourceEditorContext()$path)
} else {
  path <- normalizePath('./')
}
setwd(path)

# load packages
require(dplyr)
require(lavaan)
require(tidySEM)

# define helper functions
z_score <- function(x) {
  z <- (x - mean(x, na.rm = T))/sd(x, na.rm = T)
  z
}

# load data
data_base <- read.csv("06092022_Corona_base_inklEinzeldaten.csv",
                      header = T, dec = ",", sep = ";", encoding="ISO-8859-1")
data_MZP2 <- read.csv("06092022_Corona_MZP2_inklEinzeldaten.csv",
                      header = T, dec = ",", sep = ";", encoding="ISO-8859-1")


# re-arrange data accorting to the case number for easier visual
# inspection
data_base <- data_base %>%
  arrange(IC05_01) %>%
  select(IC05_01,
         # parents rate childrens EF
         MD04_01, MD04_02, MD04_03, MD04_04,
         MD04_05, MD04_06, MD04_07, MD04_08,

         # parents rate their own EF
         PB02_01, PB02_02, PB02_03, PB02_04,
         PB02_05, PB02_06, PB02_07, PB02_08,

         # time spent
         HS01)

data_MZP2 <- data_MZP2 %>%
  arrange(IC05_01) %>%
  select(IC05_01,
         # parents rate childrens EF
         MD04_01, MD04_02, MD04_03, MD04_04,
         MD04_05, MD04_06, MD04_07, MD04_08,

         # parents rate their own EF
         PB02_01, PB02_02, PB02_03, PB02_04,
         PB02_05, PB02_06, PB02_07, PB02_08,

         # time spent
         HS01)

data_mzp12 <- data_base %>%
  left_join(., data_MZP2, by = 'IC05_01', suffix = c(".1", ".2")) %>%
  mutate(IC05_01 = as.factor(IC05_01))

data_mzp12 <- data_mzp12 %>%
  mutate_if(is.numeric, z_score)

# model

###cross-lagged panel
Model_H1 <- '
# Messmodell

## Children
### Working memory
AGK1 =~ 1 * MD04_01.1 + 1 * MD04_04.1 + 1 * MD04_07.1
AGK2 =~ 1 * MD04_01.2 + 1 * MD04_04.2 + 1 * MD04_07.2

# AG
AGK2 ~~ AGK1


'

fit_Model_H1 <- sem(model = Model_H1, data = data_mzp12,
                    missing = "FIML", fixed.x = 'default',
                    estimator ="MLR",  std.lv = TRUE)
summary(fit_Model_H1, standardize = TRUE, rsquare = TRUE)
fitMeasures(fit_Model_H1, c("cfi", "rmsea", "srmr"))

#Graph
pfad_layout <- get_layout(
  NA, "AGK1", NA, NA, "AGK2", NA,
  "MD04_01.1", "MD04_04.1" , "MD04_07.1", "MD04_01.2", "MD04_04.2" , "MD04_07.2",
  rows = 2)

graph_sem(model = fit_Model_H1, layout = pfad_layout)


'

### Inhibition
IHK1 =~ 1 * MD04_03.1 + MD04_05.1 + MD04_06.1 + MD04_08.1
IHK2 =~ 1 * MD04_03.2 + MD04_05.2 + MD04_06.2 + MD04_08.2

## Parents
### Working memory
AGE1 =~ 1 * PB02_01.1 + PB02_02.1 + PB02_04.1 + PB02_07.1
AGE2 =~ 1 * PB02_01.2 + PB02_02.2 + PB02_04.2 + PB02_07.2

### Inhibition
IHE1 =~ 1 * PB02_03.1 + PB02_05.1 + PB02_06.1 + PB02_08.1
IHE2 =~ 1 * PB02_03.2 + PB02_05.2 + PB02_06.2 + PB02_08.2

## EF children
# EF_K1 =~ 1 * AGK1 + 1 * IHK1
# EF_K2 =~ 1 * AGK2 + 1 * IHK2

# EF_E1 =~ 1 * AGE1 + 1 * IHE1
# EF_E2 =~ 1 * AGE2 + 1 * IHE2

# Strukturmodell
# EFK2 ~ EFE1 + EFK1
# EFE2 ~ EFK1 + EFE1

# Kovarianzen
# EF_K1 ~~ EF_K2
# EF_E1 ~~ EF_E2

#Intecepts
MD04_01.1 ~ 0
MD04_04.1 ~ 0
MD04_07.1 ~ 0
MD04_03.1 ~ 0
MD04_05.1 ~ 0
MD04_06.1 ~ 0
MD04_08.1 ~ 0
MD04_01.2 ~ 0
MD04_04.2 ~ 0
MD04_07.2 ~ 0
MD04_03.2 ~ 0
MD04_05.2 ~ 0
MD04_06.2 ~ 0
MD04_08.2 ~ 0

PB02_01.1 ~ 0
PB02_02.1 ~ 0
PB02_04.1 ~ 0
PB02_07.1 ~ 0
PB02_03.1 ~ 0
PB02_05.1 ~ 0
PB02_06.1 ~ 0
PB02_08.1 ~ 0
PB02_01.2 ~ 0
PB02_02.2 ~ 0
PB02_04.2 ~ 0
PB02_07.2 ~ 0
PB02_03.2 ~ 0
PB02_05.2 ~ 0
PB02_06.2 ~ 0
PB02_08.2 ~ 0

# residuen
# PB02_03.1 ~~ 0 * PB02_03.1
# PB02_03.2 ~~ 0 * PB02_03.2

'