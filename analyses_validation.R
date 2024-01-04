
      ############################
      # title: "validation T-TK" #
      # author: "Tim Fütterer"   #
      # date: "10 10 2023"       #
      ############################


# clean environment
rm(list = ls())

# session info
sessionInfo()

# load packages
library(renv)
library(psych)

# renv init
renv::init()

# read data ----
# data is available here: https://doi.org/10.17605/OSF.IO/G86M7
data <- readRDS("data_t-tk_validation_anonymized.rds")

# overview R and packages ----
## R
citation()
version$version.string

## packages
citation("renv")
citation("psych")

pkgList <- c(
  "renv",
  "psych"
)

for (i in 1:length(pkgList)) {
  cat(
    paste0(
      i,
      ". ",
      pkgList[i],
      " [", "v",
      utils::packageVersion(pkgList[i]),
      "]\n"
    )
  )
}
rm(i, pkgList)

# validation -----
## descriptions (Table 5 in the paper)
psych::describe(data$tkpre)
psych::describe(data$SBTK)
psych::describe(data$SBTPK)
psych::describe(data$TPKASUM)
psych::describe(data$PK)

## Shapiro tests (prior to correlation)
shapiro.test(data$tkpre)
shapiro.test(data$SBTK)
shapiro.test(data$SBTPK)
shapiro.test(data$TPKASUM)
shapiro.test(data$PK)

## correlations (Table 7 in the paper)
# (1) T-TK AND SELF-REPORTED TK
stats::cor.test(data$tkpre, data$SBTK, method = "spearman", exact = FALSE)
#r = .52, p < .001

# (1) T-TK AND SELF-REPORTED TPK
stats::cor.test(data$tkpre, data$SBTPK, method = "spearman", exact = FALSE)
#r = .33, p < .001

# (1) T-TK AND TEST-BASED TPK
stats::cor.test(data$tkpre, data$TPKASUM, method = "spearman", exact = FALSE)
#r = .46, p < .001

# (1) T-TK AND TEST-BASED PK
stats::cor.test(data$tkpre, data$PK, method = "spearman", exact = FALSE)
#r = .18, p < .001

# (2) SELF-REPORTED TK AND SELF-REPORTED TPK
stats::cor.test(data$SBTK, data$SBTPK, method = "spearman", exact = FALSE)
#r = .52, p < .001

#(2)SELF-REPORTED TK AND TEST-BASED TPK
stats::cor.test(data$SBTK, data$TPKASUM, method = "spearman", exact = FALSE)
#r = .30, p < .001

#(2)SELF-REPORTED TK AND TEST-BASED PK
stats::cor.test(data$SBTK, data$PK, method = "spearman", exact = FALSE)
#r = .02, p < .001

#(3)SELF-REPORTED TPK AND TEST-BASED TPK
stats::cor.test(data$SBTPK, data$TPKASUM, method = "spearman", exact = FALSE)
#r = .27, p < .001
#(3)SELF-REPORTED TPK AND TEST-BASED TPK
stats::cor.test(data$SBTPK, data$PK, method = "spearman", exact = FALSE)
#r = .17, p < .001

#(4)TEST-BASED TPK AND TEST-BASED PK
stats::cor.test(data$TPKASUM, data$PK, method = "spearman", exact = FALSE)
#r = .23, p < .001