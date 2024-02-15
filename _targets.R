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



# Targets: homework   -----------------------------------------------------
targets_homework <- c(
  
  tar_target(
    d,
    Howell1 %>% 
      filter(age <= 13) %>%
      select(c(weight, age))
  ),
  
  tar_target(
    h02_mAW_prior,
    brm(formula = weight ~ age, 
        data = d, 
        family = gaussian(),
        sample_prior = "only",
        prior = c(set_prior("normal(35, 2)", class = "Intercept"),
                  set_prior("uniform(0, 10)", class = "b"), # uniform so it stays positive
                  set_prior("exponential(1)", class = "sigma")),
        chains = 4,
        cores = 1,
        iter = 2000)),
  
  tar_target(
    h02_mAW,
    brm(formula = weight ~ age, 
    data = d, 
    family = gaussian(),
    prior = c(set_prior("normal(35, 2)", class = "Intercept"),
              set_prior("uniform(0, 10)", class = "b"), # uniform so it stays positive
              set_prior("exponential(1)", class = "sigma")),
    chains = 4,
    cores = 1,
    iter = 2000))
  
  
  
  
)


# Quarto ------------------------------------------------------------------
targets_quarto <- c(
  tar_quarto(site, path = '.')
)



# Targets: all ------------------------------------------------------------
# Automatically grab all the "targets_*" lists above
lapply(grep('targets', ls(), value = TRUE), get)