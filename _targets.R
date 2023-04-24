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

happiness <- sim_happiness(seed = 1977, N_years = 1000)

data(cherry_blossoms)

data(NWOGrants)

data(reedfrogs)
tar_source('input/prior-values.R')


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
    select(c(weight, avgfood, area, groupsize)) %>%
    mutate(avgfood = standardize(avgfood), 
           area = standardize(area),
           weight = standardize(weight),
           groupsize = standardize(groupsize))
  ),
  
  tar_target(
  h03_q1,
  brm(formula = avgfood ~ area, data = f, family = gaussian(),
      prior = c(set_prior("normal(0, 0.5)", class = "Intercept"),
                set_prior("normal(0, 0.5)", class = "b"),
                set_prior("exponential(1)", class = "sigma")))
  ),

  tar_target(
  h03_q2,
  brm(formula = weight ~ avgfood, data = f, family = gaussian(),
      prior = c(set_prior("normal(0, 0.5)", class = "Intercept"),
                set_prior("normal(0, 0.5)", class = "b"),
                set_prior("exponential(1)", class = "sigma")))
  ),

  tar_target(
  h03_q3,
  brm(formula = weight ~ avgfood + groupsize, data = f, family = gaussian(),
      prior = c(set_prior("normal(0, 0.5)", class = "Intercept"),
                set_prior("normal(0, 0.5)", class = "b"),
                set_prior("exponential(1)", class = "sigma")))
  ),

  tar_target(
  h03_q3b,
  brm(formula = groupsize ~ avgfood, data = f, family = gaussian(),
      prior = c(set_prior("normal(0, 0.5)", class = "Intercept"),
                set_prior("normal(0, 0.5)", class = "b"),
                set_prior("exponential(1)", class = "sigma")))
  )
)


# Targets: homework 04 ----------------------------------------------------
targets_h04 <- c(
  
  tar_target(
    happiness_data, 
    happiness %>%
      mutate(age = age > 17,
             A = (age - 18) / (65 - 18),
             mid = married + 1)
  ),
  
  tar_target(
    cherry,
    cherry_blossoms %>% 
      select(temp, doy) %>%
      drop_na() %>%
      mutate(temp_s = standardize(temp),
             doy_s = standardize(doy))
  ),
  
  tar_target(
    h04_q1a,
    brm(formula = happiness ~ mid + A, data = happiness_data, family = gaussian(),
        prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                  set_prior("normal(0, 2)", class = "b"),
                  set_prior("exponential(1)", class = "sigma")))
  ),
  
  tar_target(
    h04_q1b,
    brm(formula = happiness ~ A, data = happiness_data, family = gaussian(),
        prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                  set_prior("normal(0, 2)", class = "b"),
                  set_prior("exponential(1)", class = "sigma")))
  ),
  
  tar_target(
    h04_q2,
    brm(formula = weight ~ avgfood + groupsize + area, data = f, family = gaussian(),
        prior = c(set_prior("normal(0, 0.5)", class = "Intercept"),
                  set_prior("normal(0, 0.5)", class = "b"),
                  set_prior("exponential(1)", class = "sigma")))
  ),
  
  tar_target(
    h04_q3a,
    brm(formula = doy_s ~ temp_s, data = cherry, family = gaussian(),
        prior = c(set_prior("normal(0, 0.5)", class = "Intercept"),
                  set_prior("normal(0, 0.5)", class = "b"),
                  set_prior("exponential(1)", class = "sigma")),
        sample_prior = 'only')
  ),
  
  tar_target(
    h04_q3b,
    brm(formula = doy_s ~ temp_s, data = cherry, family = gaussian(),
        prior = c(set_prior("normal(0, 0.5)", class = "Intercept"),
                  set_prior("normal(0, 0.5)", class = "b"),
                  set_prior("exponential(1)", class = "sigma")))
  ),
  
  tar_target(
    h04_q3c,
    brm(formula = doy_s ~ s(temp_s), data = cherry, family = gaussian(),
        prior = c(set_prior("normal(0, 0.5)", class = "Intercept"),
                  set_prior("normal(0, 0.5)", class = "b"),
                  set_prior("exponential(1)", class = "sigma")))
  )
  
  
)



# Targets: homework 05 ----------------------------------------------------
targets_h05 <- c(
  
  tar_target(
    grant_data, 
    NWOGrants %>%
      mutate(awards = as.integer(awards),
             applications = as.integer(applications),
             discipline = as.factor(as.integer(discipline)),
             gender = as.factor(case_when(gender == 'f' ~ 1,
                                gender == 'm' ~ 2)))
  ),
  
  tar_target(
    h05_q1_prior,
    brm(formula = awards | trials(applications) ~ gender, data = grant_data, family = binomial(),
        prior = c(set_prior("normal(-1, 1)", class = "Intercept"),
                  set_prior("normal(-1, 1)", class = "b")),
        sample_prior = "only")
  ),
  
  tar_target(
    h05_q1,
    brm(formula = awards | trials(applications) ~ gender, data = grant_data, family = binomial(),
        prior = c(set_prior("normal(-1, 1)", class = "Intercept"), 
                  set_prior("normal(-1, 1)", class = "b")))
  ),
  
  tar_target(
    h05_q2,
    brm(formula = awards | trials(applications)  ~ gender + discipline + gender:discipline, data = grant_data, family = binomial(),
        prior = c(set_prior("normal(-1, 1)", class = "Intercept"),
                  set_prior("normal(-1, 1)", class = "b")))
  )
  
  
  
)


# Targets: homework 06 ----------------------------------------------------
targets_h06 <- c(
  
  tar_target(
    frogs,
    reedfrogs %>%
      mutate(tank = row_number())
  ),
  
  tar_eval(
    tar_target(
      model_name_sym,
      brm(formula = surv | trials(density)  ~ 1 + (1|tank), data = frogs, family = binomial(),
        prior = c(prior(normal(0, 1), class = "Intercept"), # alpha bar because only intercept (1) varies by tank (1|tank)
                  prior_string(paste0("exponential(", prior_level, ")"), class = "sd")), # sigma
        sample_prior = 'yes')),
    values = values_priors
    )

)



# Quarto ------------------------------------------------------------------
targets_quarto <- c(
  tar_quarto(site, path = '.')
)



# Targets: all ------------------------------------------------------------
# Automatically grab all the "targets_*" lists above
lapply(grep('targets', ls(), value = TRUE), get)