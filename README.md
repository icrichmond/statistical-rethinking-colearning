# Statistical Rethinking Colearning 2023

**NOTE:** website format taken from [Alec Robitaille](https://github.com/robitalec/statistical-rethinking-colearning-2023)

-   <a href="#schedule" id="toc-schedule">Schedule</a>
    -   <a href="#lectures" id="toc-lectures">Lectures</a>
    -   <a href="#homework" id="toc-homework">Homework</a>
-   <a href="#participant-notes-and-homework-solutions"
    id="toc-participant-notes-and-homework-solutions">Participant notes and homework solutions</a>
-   <a href="#resources" id="toc-resources">Resources</a>
-   <a href="#installation" id="toc-installation">Installation</a>
-   <a href="#code-of-conduct" id="toc-code-of-conduct">Code of Conduct</a>

<!-- README.md is generated from README.Rmd. Please edit that file -->

------------------------------------------------------------------------

Third round of [Statistical Rethinking](https://github.com/rmcelreath/stat_rethinking_2024/tree/main) colearning, this time with 2024 [lectures](https://www.youtube.com/playlist?list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus) and [homework](https://github.com/rmcelreath/stat_rethinking_2024/tree/main/homework).

The first round of Statistical Rethinking colearning (2022) is available [here](https://github.com/robitalec/statistical-rethinking-colearning-2022).

The second round of Statistical Rethinking colearning (2023) is available [here](https://github.com/robitalec/statistical-rethinking-colearning-2023).

## Schedule

### Lectures

| Meeting date | Reading               | Lectures                                                                                                                                                                                                                                                                                                                                                                                                                                                |
|:--------|:--------|:------------------------------------------------------|
| 25 January   | Chapters 1, 2 and 3   | \[1\] \<[Science Before Statistics](https://www.youtube.com/watch?v=FdnMWdICdRs&list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus&index=1)\> \<[Slides](https://speakerdeck.com/rmcelreath/statistical-rethinking-2023-lecture-01)\> <br> \[2\] \<[Garden of Forking Data](https://www.youtube.com/watch?v=R1vcdhPBlXA&list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus&index=2)\> \<[Slides](https://speakerdeck.com/rmcelreath/statistical-rethinking-2023-lecture-02)\> |
| 08 February  | Chapter 4             | \[3\] \<[Geocentric Models](https://www.youtube.com/watch?v=tNOu-SEacNU&list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus&index=3)\> \<[Slides](https://speakerdeck.com/rmcelreath/statistical-rethinking-2023-lecture-03)\> <br> \[4\] \<[Categories and Curves](https://www.youtube.com/watch?v=F0N4b7K_iYQ&list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus&index=4)\> \<[Slides](https://speakerdeck.com/rmcelreath/statistical-rethinking-2023-lecture-04)\>          |
| 22 February  | Chapters 5 and 6      | \[5\] \<[Elemental Confounds](https://www.youtube.com/watch?v=mBEA7PKDmiY&list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus&index=5)\> \<[Slides](https://speakerdeck.com/rmcelreath/statistical-rethinking-2023-lecture-05)\> <br> \[6\] \<[Good and Bad Controls](https://www.youtube.com/watch?v=uanZZLlzKHw&list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus&index=6)\> \<[Slides](https://speakerdeck.com/rmcelreath/statistical-rethinking-2023-lecture-06)\>        |
| 07 March     | Chapters 7 and 8      | \[7\] \<[Overfitting](https://www.youtube.com/watch?v=1VgYIsANQck&list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus&index=7)\> \<[Slides](https://speakerdeck.com/rmcelreath/statistical-rethinking-2023-lecture-07)\> <br> \[8\] \<[MCMC](https://www.youtube.com/watch?v=rZk2FqX2XnY&list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus&index=8)\> \<[Slides](https://speakerdeck.com/rmcelreath/statistical-rethinking-2023-lecture-08)\>                                 |
| 21 March     | Chapters 9, 10 and 11 | \[9\] \<[Modeling Events](https://www.youtube.com/watch?v=Zi6N3GLUJmw&list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus&index=9)\> \<[Slides](https://speakerdeck.com/rmcelreath/statistical-rethinking-2023-lecture-09)\> <br> \[10\] \<[Counts and Confounds](https://www.youtube.com/watch?v=jokxu18egu0&list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus&index=10)\> \<[Slides](https://speakerdeck.com/rmcelreath/statistical-rethinking-2023-lecture-10)\>           |
| 04 April     | Chapters 11 and 12    | \[11\] \<[Ordered Categories](https://www.youtube.com/watch?v=VVQaIkom5D0&list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus&index=11)\> \<[Slides](https://speakerdeck.com/rmcelreath/statistical-rethinking-2023-lecture-11)\> <br> \[12\] \<[Multilevel Models](https://www.youtube.com/watch?v=iwVqiiXYeC4&list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus&index=12)\> \<[Slides](https://speakerdeck.com/rmcelreath/statistical-rethinking-2023-lecture-12)\>         |
| 18 April     | Chapter 13            | \[13\] \<[Multilevel Adventures](https://www.youtube.com/watch?v=sgqMkZeslxA&list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus&index=13)\> \<[Slides](https://speakerdeck.com/rmcelreath/statistical-rethinking-2023-lecture-13)\> <br> \[14\] \<[Correlated Features](https://www.youtube.com/watch?v=Es44-Bp1aKo&list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus&index=14)\> \<[Slides](https://speakerdeck.com/rmcelreath/statistical-rethinking-2023-lecture-14)\>    |
| 2 May        | Chapter 14            | \[15\] \<[Social Networks](https://www.youtube.com/watch?v=hnYhJzYAQ60&list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus&index=15)\> \<[Slides](https://speakerdeck.com/rmcelreath/statistical-rethinking-2023-lecture-15)\> <br> \[16\] \<[Gaussian Processes](https://www.youtube.com/watch?v=Y2ZLt4iOrXU&list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus&index=16)\> \<[Slides](https://speakerdeck.com/rmcelreath/statistical-rethinking-2023-lecture-16)\>           |
| 16 May       | Chapter 15            | \[17\] Measurement Error <br> \[18\] Missing Data                                                                                                                                                                                                                                                                                                                                                                                                       |
| 30 May       | Chapters 16 and 17    | \[19\] Beyond GLMs: State-space Models, ODEs <br> \[20\] Horoscopes                                                                                                                                                                                                                                                                                                                                                                                     |

### Homework

| Meeting date | Homework                                                                                       | Solutions                                                                                               |
|:-------------|:---------------------------|:-----------------------------|
| 1 February   | [Homework 1](https://github.com/rmcelreath/stat_rethinking_2024/blob/main/homework/week01.pdf) | [Solutions](https://github.com/rmcelreath/stat_rethinking_2024/blob/main/homework/week01_solutions.pdf) |
| 15 February  | [Homework 2](https://github.com/rmcelreath/stat_rethinking_2024/blob/main/homework/week02.pdf) | [Solutions](https://github.com/rmcelreath/stat_rethinking_2024/blob/main/homework/week02_solutions.pdf)                                                                                               |
| 29 February  | [Homework 3](https://github.com/rmcelreath/stat_rethinking_2024/blob/main/homework/week03.pdf) | [Solutions](https://github.com/rmcelreath/stat_rethinking_2024/blob/main/homework/week03_solutions.pdf)                                                                                               |
| 14 March     | [Homework 4](https://github.com/rmcelreath/stat_rethinking_2024/blob/main/homework/week04.pdf) | [Solutions](https://github.com/rmcelreath/stat_rethinking_2024/blob/main/homework/week04_solutions.pdf)                                                                                               |
| 28 March     | [Homework 5](https://github.com/rmcelreath/stat_rethinking_2024/blob/main/homework/week05.pdf) | [Solutions](https://github.com/rmcelreath/stat_rethinking_2024/blob/main/homework/week05_solutions.pdf)                                                                                               |
| 11 April     | [Homework 6](https://github.com/rmcelreath/stat_rethinking_2024/blob/main/homework/week06.pdf) | [Solutions](https://github.com/rmcelreath/stat_rethinking_2024/blob/main/homework/week06_solutions.pdf)                                                                                               |
| 25 April     | [Homework 7](https://github.com/rmcelreath/stat_rethinking_2024/blob/main/homework/week07.pdf) | [Solutions](https://github.com/rmcelreath/stat_rethinking_2024/blob/main/homework/week07_solutions.pdf)                                                                                               |                                                                                     | Solutions                                                                                               |
| 9 May        | [Homework 8](https://github.com/rmcelreath/stat_rethinking_2024/blob/main/homework/week08.pdf) | [Solutions](https://github.com/rmcelreath/stat_rethinking_2024/blob/main/homework/week08_solutions.pdf)                                                                                               |
| 23 May       | [Homework 9](https://github.com/rmcelreath/stat_rethinking_2024/blob/main/homework/week09.pdf) | [Solutions](https://github.com/rmcelreath/stat_rethinking_2024/blob/main/homework/week09_solutions.pdf)                                                                                               |
| 30 May       | [Homework 10](https://github.com/rmcelreath/stat_rethinking_2024/blob/main/homework/week010.pdf) | [Solutions](https://github.com/rmcelreath/stat_rethinking_2024/blob/main/homework/week10_solutions.pdf)                                                                                               |

## Participant notes and homework solutions

-   [Alec](https://github.com/robitalec/statistical-rethinking-colearning-2023)
-   [Bella](https://github.com/icrichmond/statistical-rethinking-colearning) (this repo)
-   

## Resources

Additional material using other packages or languages

-   Original R: <https://github.com/rmcelreath/rethinking/>
-   R + Tidyverse + ggplot2 + brms: <https://bookdown.org/content/4857/>
-   Python and PyMC3: Python/PyMC3
-   Julia and Turing: <https://github.com/StatisticalRethinkingJulia> and <https://github.com/StatisticalRethinkingJulia/TuringModels.jl>

See Richard's comments about these here: <https://github.com/rmcelreath/stat_rethinking_2023#coding>

2022 colearning:

-   Lectures: <https://github.com/rmcelreath/stat_rethinking_2022#calendar--topical-outline>
-   Homework: <https://github.com/rmcelreath/stat_rethinking_2022/tree/main/homework>

Also, Alec's notes and solutions of the 2019 material: <https://github.com/robitalec/statistical-rethinking> and <https://www.statistical-rethinking.robitalec.ca/>

## Installation

Package specific install directions. We'll update these as we go!

Rethinking

-   [`rethinking`](https://github.com/rmcelreath/rethinking#installation)

Stan

-   [`cmdstanr`](https://mc-stan.org/cmdstanr/articles/cmdstanr.html)
-   [`RStan`](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started)
-   [`brms`](r/brms/#how-do-i-install-brms)

Targets

-   [`targets`](https://github.com/ropensci/targets/#installation)
-   [`stantargets`](https://github.com/ropensci/stantargets/#installation)

V8, needed for the `dagitty` package

-   [`V8`](https://github.com/jeroen/v8#installation)

## Code of Conduct

Please note that this project is released with a [Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.
