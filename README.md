
# Statistical Rethinking colearning 2022

This repository contains resources and information for a colearning
group meeting regularly to discuss lectures and homework assignments
from the [Statistical Rethinking
2022](https://github.com/rmcelreath/stat_rethinking_2022) course.

## Project structure

This repository is structured with a `homework/` folder for homework
solutions, and `notes/` folder for notes. For folks joining in the
colearning group, you are encouraged to make your own branch in this
repository and share your notes and/or homework solutions.

The `R/` folder can be used to store reusable functions useful across
homework solutions and your own model situations.

For example, the `dag_plot` function makes a DAG plot from a DAG:

``` r
library(ggplot2)
library(ggdag)
```

    ## 
    ## Attaching package: 'ggdag'

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

``` r
library(dagitty)

source('R/dag_plot.R')

dag <- dagify(
    Z ~ A + B,
    B ~ A,
    exposure = 'A',
    outcome = 'Z'
)

dag_plot(dag)
```

![](graphics/readme_dag-1.png)<!-- -->

## Resources

-   Lectures:
    <https://github.com/rmcelreath/stat_rethinking_2022#calendar--topical-outline>
-   Homework:
    <https://github.com/rmcelreath/stat_rethinking_2022/tree/main/homework>

Additional material using other packages or languages

-   Original R: <https://github.com/rmcelreath/rethinking/>
-   R + Tidyverse + ggplot2 + brms: <https://bookdown.org/content/4857/>
-   Python and PyMC3: Python/PyMC3
-   Julia and Turing: <https://github.com/StatisticalRethinkingJulia>
    and <https://github.com/StatisticalRethinkingJulia/TuringModels.jl>

See Richard’s comments about these here:
<https://github.com/rmcelreath/stat_rethinking_2022#original-r-flavor>

Also, Alec’s notes and solutions of the 2019 material:
<https://github.com/robitalec/statistical-rethinking> and
<https://www.statistical-rethinking.robitalec.ca/>

## Installation

Some packages and install notes. We’ll update these as we go!

### Rethinking

-   [`rethinking`](https://github.com/rmcelreath/rethinking#installation)

### Stan

-   [`cmdstanr`](https://mc-stan.org/cmdstanr/articles/cmdstanr.html)
-   [`RStan`](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started)
-   [`brms`](r/brms/#how-do-i-install-brms)

### Targets

-   [`targets`](https://github.com/ropensci/targets/#installation)
-   [`stantargets`](https://github.com/ropensci/stantargets/#installation)

### V8

(The V8 package is needed for dagitty)

-   [`V8`](https://github.com/jeroen/v8#installation)

## Thanks

Many thanks to Richard McElreath for a continued emphasis on teaching
Bayesian statistics and for providing this incredible resource of
lectures and homework assignments free for everyone.

Also thank you to the developers of R, Stan and innumerous R packages
that allow us to pursue this interest.

## Code of Conduct

Please note that this project is released with a [Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.
