
#==========================================================
# PROJECT: CLINICAL TRIAL TABLE GENERATOR
# FILE   : create_synthetic_adsl.R
# PURPOSE: Create synthetic ADSL dataset
#==========================================================


# 1. SET RANDOM SEED

set.seed(123)


# 2. DEFINE NUMBER OF SUBJECTS

n <- 40


# 3. CREATE SYNTHETIC ADSL DATASET

adsl <- data.frame(
  
  STUDYID = "STUDY001",
  
  USUBJID = sprintf("SUBJ%03d", 1:n),
  
  TRT01A = rep(
    c("Tquine", "Placebo"),
    each = 20
  ),
  
  AGE = sample(
    25:75,
    n,
    replace = TRUE
  ),
  
  SEX = sample(
    c("Male", "Female"),
    n,
    replace = TRUE
  ),
  
  RACE = sample(
    c(
      "WHITE",
      "BLACK OR AFRICAN AMERICAN",
      "ASIAN"
    ),
    n,
    replace = TRUE
  ),
  
  ETHNIC = sample(
    c(
      "HISPANIC OR LATINO",
      "NOT HISPANIC OR LATINO"
    ),
    n,
    replace = TRUE
  ),
  
  HEIGHT = round(
    runif(n, 150, 190),
    1
  ),
  
  WEIGHT = round(
    runif(n, 45, 100),
    1
  )
  
)


# 4. DERIVE BMI

adsl$BMI <- round(
  adsl$WEIGHT / (adsl$HEIGHT / 100)^2,
  1
)


# 5. CREATE OUTPUT FOLDER IF IT DOES NOT EXIST

dir.create(
  "C:\\Users\\User\\Desktop\\Clinical-Table-Generator\\R\\data\\synthetic/",
  recursive = TRUE,
  showWarnings = FALSE
)


# 6. EXPORT DATASET AS CSV

write.csv(
  adsl,
  "C:\\Users\\User\\Desktop\\Clinical-Table-Generator\\R\\data\\synthetic\\adsl.csv",
  row.names = FALSE
)


# 7. CHECK THE GENERATED DATASET

cat("Synthetic ADSL dataset created successfully!\n")

cat("Number of subjects:", nrow(adsl), "\n")

cat("Number of variables:", ncol(adsl), "\n")

print(head(adsl))


#==========================================================
# END OF SCRIPT
#==========================================================
