# === Targets -------------------------------------------------------------
# Alec L. Robitaille



# Source ------------------------------------------------------------------
lapply(dir('R', '*.R', full.names = TRUE), source)



# Options -----------------------------------------------------------------
# Targets
tar_option_set(format = 'qs')

# Stan
options(mc.cores = 2,
        scipen = 999,
        digits = 2)


# Data --------------------------------------------------------------------
data(Howell1)
data(foxes)


# Targets: homework 2 -----------------------------------------------------
targets_h02 <- c(
  
  tar_target(
    d,
    Howell1 %>% 
      filter(age <= 13) %>%
      select(c(weight, age))
  ),
  
  tar_target(
    h02_mAW,
    brm(formula = weight ~ age, data = d, family = gaussian(),
               prior = c(set_prior("normal(5, 1)", class = "Intercept"),
                         set_prior("uniform(0, 10)", class = "b"), # uniform so it stays positive
                         set_prior("exponential(1)", class = "sigma")))
  ),
  
  tar_target(
    d2,
    Howell1 %>% 
      filter(age <= 13) %>%
      mutate(sex = as.factor(case_when(male == 0 ~ 1, 
                                       male == 1 ~ 2))) %>%
      select(c(weight, age, sex))
  ),
  
  tar_target(
    h02_mAWS,
    brm(formula = weight ~ age + sex, data = d2, family = gaussian(),
        prior = c(set_prior("normal(5, 1)", class = "Intercept"),
                  set_prior("uniform(0, 10)", class = "b"),
                  set_prior("exponential(1)", class = "sigma")))
  )
)


# Targets: homework 3 -----------------------------------------------------
targets_h03 <- c(
  
  tar_target(
    f,
    foxes %>% 
      select(c(avgfood, area)) %>%
      mutate(avgfood = standardize(avgfood), 
             area = standardize(area))
  ),
  
  tar_target(
    h03_q1,
    brm(formula = avgfood ~ area, data = f, family = gaussian(),
        prior = c(set_prior("normal(0, 0.5)", class = "Intercept"),
                  set_prior("normal(0, 0.5)", class = "b"), # uniform so it stays positive
                  set_prior("exponential(1)", class = "sigma")))
  )
  
  
)




# Quarto ------------------------------------------------------------------
targets_quarto <- c(
  tar_quarto(site, path = '.')
)



# Targets: all ------------------------------------------------------------
# Automatically grab all the "targets_*" lists above
lapply(grep('targets', ls(), value = TRUE), get)