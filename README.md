
<!-- README.md is generated from README.Rmd. Please edit that file -->

# val

<!-- badges: start -->
<!-- badges: end -->

The goal of `val` is to create a unit testing culture for data cleaning
tasks using R. Coming from a statistics background, the idea of
defensive programming is fascinating to me. A comprehensive set of unit
tests can demonstrate the developer’s ability to understand the demand
from the end user’s perspective before even starting any actual
implementation. The analytics community hasn’t fully adopted this
software engineering practice for various reasons.

> When Phelps was a teenager, Bowman would tell him to watch the
> “videotape” before he went to sleep and when he woke up. The
> “videotape” wasn’t real but a mental visualization of the perfect
> race. Phelps would imagine diving off the blocks and, in slow motion,
> swimming flawlessly. He would visualize the strokes, the pool, and the
> finish. He would lie in bed with his eyes shut and watch the entire
> completion down to the smallest details.

In most analytics projects, no matter how many sources the team has to
pull or how many steps are involved in performing the transformation,
the end goal is to have “cleaned” and “ready-to-go” data that everyone
should use. However, most of the time, team members won’t spend the time
to visualize the detailed schema of their cleaned table mentally. At
best, they would make sure that the column names are aligned. But what
about attributes such as data types, allowable values, parsing formats,
uniqueness, mandatory, etc.? Currently, no generic built-in object in R
allows developers to specify the schema. Meanwhile, no workflow helps
data engineers to validate their data in a tidy manner.

At first, I thought about writing schema in JSON and YAML and parsing
them into the R environment. However, when Joe Cheng shared his stories
of the early ideas of developing the Shiny package in [Rconf
2022](https://www.rstudio.com/conference/2022/keynotes/past-future-shiny/),
the Shiny team expected R users to easily create an interactive web
application in R without needing to know HTML, JavaScript, and CSS.
Therefore, I hope val could enable R users to specify table schema
without knowing JSON, YAML, SQL, and designing a database. Users would
create schema objects from `val` and work towards the detailed schema
until their data has passed the validation.

## Installation

You can install the released version of val from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("val")
```
