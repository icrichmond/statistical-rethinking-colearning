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
data(Oxboys)
data(foxes)
data(Dinosaurs)
data(NWOGrants)
data(reedfrogs)
data(Trolley)

marriage <- sim_happiness(seed = 1977, N_years = 1000) %>%
  filter(age > 17) %>%
  mutate(A = (age - 18)/(65-18))

lagged_obs <- Dinosaurs %>%
  filter(species == "Massospondylus carinatus") %>%
  mutate(time_diff = age - lag(age),
         sizelast = lag(mass)) %>%
  slice(-1)

trolley <- Trolley %>% 
  mutate(contact = as.factor(contact),
         action = as.factor(action),
         intention = as.factor(intention))


# Initial Values ----------------------------------------------------------


inits <- list(`Intercept[1]` = -2,
              `Intercept[2]` = -1,
              `Intercept[3]` = 0,
              `Intercept[4]` = 1,
              `Intercept[5]` = 2,
              `Intercept[6]` = 2.5)

inits_list <- list(inits, inits, inits, inits)


# Targets: homework   -----------------------------------------------------
targets_homework <- c(
  
  tar_target(
    d,
    Howell1 %>%
      filter(age <= 13) %>%
      select(c(weight, age))
  ),
  
  tar_target(
    ox,
    Oxboys %>%
      group_by(Subject) %>%
      mutate(growth = height - lag(height, default = first(height), order_by = Occasion)) %>%
      # remove first occasion because there is no growth
      filter(Occasion != 1)
  ),
  
  tar_target(
    fox,
    foxes %>%
      mutate(avgfood_s = scale(avgfood),
             area_s = scale(area),
             weight_s = scale(weight),
             groupsize_s = scale(groupsize))
  ),
  
  tar_target(
    r,
    reedfrogs %>%
      mutate(tank = 1:nrow(reedfrogs))
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
        iter = 2000)
  ),
  
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
        iter = 2000)
  ),
  
  tar_target(
    h02_mGO_prior,
    brm(formula = growth ~ 1,
        data = ox,
        family = gaussian(),
        sample_prior = "only",
        prior = c(prior(normal(1.6, 0.5), class = "Intercept", lb = 0),
                  prior(exponential(1), class = "sigma")),
        chains = 4,
        cores = 2,
        iter = 2000)
  ),
  
  tar_target(
    h02_mGO,
    brm(formula = growth ~ 1,
        data = ox,
        family = gaussian(),
        prior = c(prior(normal(1.6, 0.5), class = "Intercept", lb = 0),
                  prior(exponential(1), class = "sigma")),
        chains = 4,
        cores = 2,
        iter = 2000)
  ),
  
  tar_target(
    h03_q1_prior,
    brm(formula = avgfood_s ~ area_s,
        data = fox,
        sample_prior = "only",
        family = gaussian(),
        prior = c(prior(normal(0, 0.5), class = "Intercept"),
                  prior(normal(0, 0.5), class = "b"),
                  prior(exponential(1), class = "sigma")),
        chains = 4,
        cores = 2,
        iter = 2000)
  ),
  
  tar_target(
    h03_q1,
    brm(formula = avgfood_s ~ area_s,
        data = fox,
        family = gaussian(),
        prior = c(prior(normal(0, 0.5), class = "Intercept"),
                  prior(normal(0, 0.5), class = "b"),
                  prior(exponential(1), class = "sigma")),
        chains = 4,
        cores = 2,
        iter = 2000)
  ),
  
  tar_target(
    h03_q2_prior,
    brm(formula = weight_s ~ avgfood_s,
        sample_prior = "only",
        data = fox,
        family = gaussian(),
        prior = c(prior(normal(0, 0.5), class = "Intercept"),
                  prior(normal(0, 0.5), class = "b"),
                  prior(exponential(1), class = "sigma")),
        chains = 4,
        cores = 2,
        iter = 2000)
  ),
  
  tar_target(
    h03_q2,
    brm(formula = weight_s ~ avgfood_s,
        data = fox,
        family = gaussian(),
        prior = c(prior(normal(0, 0.5), class = "Intercept"),
                  prior(normal(0, 0.5), class = "b"),
                  prior(exponential(1), class = "sigma")),
        chains = 4,
        cores = 2,
        iter = 2000)
  ),
  
  
  tar_target(
    h03_q3_prior,
    brm(formula = weight_s ~ avgfood_s + groupsize_s,
        sample_prior = "only",
        data = fox,
        family = gaussian(),
        prior = c(prior(normal(0, 0.5), class = "Intercept"),
                  prior(normal(0, 0.5), class = "b"),
                  prior(exponential(1), class = "sigma")),
        chains = 4,
        cores = 2,
        iter = 2000)
  ),
  
  tar_target(
    h03_q3,
    brm(formula = weight_s ~ avgfood_s + groupsize_s,
        data = fox,
        family = gaussian(),
        prior = c(prior(normal(0, 0.5), class = "Intercept"),
                  prior(normal(0, 0.5), class = "b"),
                  prior(exponential(1), class = "sigma")),
        chains = 4,
        cores = 2,
        iter = 2000)
  ),
  
  tar_target(
    h04_q1a,
    brm(formula = happiness ~ A + (1|married),
        data = marriage,
        family = gaussian(),
        prior = c(prior(normal(0, 1), class = "Intercept"),
                  prior(normal(0, 2), class = "b"),
                  prior(exponential(1), class = "sigma")),
        chains = 4,
        cores = 2,
        iter = 2000)
  ),
  
  tar_target(
    h04_q1b,
    brm(formula = happiness ~ A,
        data = marriage,
        family = gaussian(),
        prior = c(prior(normal(0, 1), class = "Intercept"),
                  prior(normal(0, 2), class = "b"),
                  prior(exponential(1), class = "sigma")),
        chains = 4,
        cores = 2,
        iter = 2000)
  ),
  
  tar_target(
    h04_q2,
    brm(formula = weight_s ~ avgfood_s + groupsize_s + area_s,
        data = fox,
        family = gaussian(),
        prior = c(prior(normal(0, 0.5), class = "Intercept"),
                  prior(normal(0, 0.5), class = "b"),
                  prior(exponential(1), class = "sigma")),
        chains = 4,
        cores = 2,
        iter = 2000)
  ),
  
  tar_target(
    h04_q3a,
    brm(formula = mass ~ age,
        data = Dinosaurs %>% filter(species == "Massospondylus carinatus"),
        prior = c(prior(normal(90, 20), class = "Intercept"),
                  prior(normal(0, 5), class = "b"),
                  prior(exponential(1), class = "sigma")),
        chains = 4,
        cores = 2,
        iter = 2000)
  ),
  
  tar_target(
    h04_q3b,
    brm(formula = bf(mass ~ sizelast * exp(- exp(logR) * time_diff) +
                       sizeMax * (1 - exp(-exp(logR) * time_diff)),
                     logR ~ 1,
                     sizeMax ~ 1, nl = TRUE),
        data = lagged_obs,
        prior = c(prior(normal(245, 50), nlpar = "sizeMax", class = "b"),
                  prior(normal(0, 5), nlpar = "logR", class = "b"),
                  prior(exponential(1), class = "sigma")),
        chains = 4,
        cores = 2,
        iter = 2000)
  ),
  
  tar_target(
    h04_q3c,
    brm(formula = bf(mass ~ a * b ^ (c * age + d),
                     a + b + c + d ~ 1,
                     nl = TRUE),
        data = Dinosaurs %>% filter(species == "Massospondylus carinatus"),
        prior = c(
          prior(normal(0, 20), nlpar = 'a'),
          prior(normal(0, 20), nlpar = 'b'),
          prior(normal(0, 20), nlpar = 'c', lb = 0),
          prior(normal(0, 20), nlpar = 'd'),
          prior(exponential(1), sigma)
        ),
        chains = 4,
        cores = 2,
        iter = 2000)
  ),
  
  zar_brms(
    h05_q1,
    formula = awards | trials(applications) ~ gender,
    data = NWOGrants,
    family = 'binomial',
    prior = c(
      prior(normal(0, 1.5), class = "Intercept"),
      prior(normal(0, 0.5), class = "b"))
  ),
  
  zar_brms(
    h05_q2,
    formula = awards | trials(applications) ~ gender*discipline,
    data = NWOGrants,
    family = 'binomial',
    prior = c(
      prior(normal(0, 1.5), class = "Intercept"),
      prior(normal(0, 0.5), class = "b"))
  ),
  
  tar_target(
    h06_q1_a,
    brm(formula = surv | trials(density) ~ 1 + (1 | tank),
        family = 'binomial', 
        sample_prior = 'only',
        data = r,
        prior = c(prior(normal(0, 1.5), class = "Intercept"),
                  prior(exponential(1), class = "sd")))
  ),
  
  tar_target(
    h06_q1_b,
    brm(formula = surv | trials(density) ~ 1 + (1 | tank),
        family = 'binomial', 
        sample_prior = 'only',
        data = r,
        prior = c(prior(normal(0, 1.5), class = "Intercept"),
                  prior(exponential(10), class = "sd")))
  ),
  
  tar_target(
    h06_q1_c,
    brm(formula = surv | trials(density) ~ 1 + (1 | tank),
        family = 'binomial', 
        sample_prior = 'only',
        data = r,
        prior = c(prior(normal(0, 1.5), class = "Intercept"),
                  prior(exponential(0.1), class = "sd")))
  ),
  
  zar_brms(
    h06_q2, 
    formula = surv | trials(density) ~ 1 + pred*size + (1 | tank),
    family = 'binomial',
    data = r, 
    prior = c(prior(normal(0, 1.5), class = "Intercept"),
              prior(exponential(0.1), class = "sd"),
              prior(normal(0, 1), class = "b"))
  ),
  
  tar_target(
    h06_q3, 
    brm(formula = response ~ 1 + action + contact + intention +  (1 | id),
    family = 'cumulative',
    data = trolley, 
    prior = c(prior(normal(0, 1.5), class = "Intercept"),
              prior(exponential(1), class = "sd"),
              prior(normal(0, 1), class = "b")),
    inits = inits_list,
    chains = 4,
    cores = 2,
    iter = 2000)
  ),
  
  tar_target(
    h06_q3b, 
    brm(formula = response ~ 1 + action + contact + intention,
        family = 'cumulative',
        data = trolley, 
        prior = c(prior(normal(0, 1.5), class = "Intercept"),
                  prior(normal(0, 1), class = "b")),
        inits = inits_list,
        chains = 4,
        cores = 2,
        iter = 2000)
  )
  
)


# Quarto ------------------------------------------------------------------
targets_quarto <- c(
  tar_quarto(site, path = '.')
)



# Targets: all ------------------------------------------------------------
# Automatically grab all the "targets_*" lists above
lapply(grep('targets', ls(), value = TRUE), get)
