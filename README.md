This repository contains code to build cosponsorship networks from bills passed in of the [Portuguese Parliament](http://www.parlamento.pt/) .

- [interactive demo](http://f.briatte.org/parlviz/assembleia)
- [static plots](http://f.briatte.org/parlviz/assembleia/plots.html)
- [more countries](https://github.com/briatte/parlnet)

# HOWTO

Replicate by running `make.r` in R.

The `data.r` script downloads information on bills and sponsors. Unfortunately, the scraper is _highly_ error-prone, as explained at the top of the script: multiple re-runs are required to successfully collect all bills. All photos should download fine.

See also Ricardo Lafuente's [Python scraper for bills](https://github.com/rlafuente/scraper-iniciativas), which also runs slowly but is certainly more efficient overall, as well as his [listing of Portuguese MPs](https://github.com/centraldedados/parlamento-deputados), which is similar to the one collected by this repository.

The `build.r` script then assembles the edge lists and plots the networks, with the help of a few routines coded into `functions.r`. Adjust the `plot`, `gexf` and `mode` parameters to skip the plots or to change the node placement algorithm.

# DATA

## Bills

- `legislature` -- legislature number (Roman number, VI-XII)
- `session` -- legislature session
- `ref` -- bill local identifier (number/legislature)
- `title` -- title
- `bid` -- bill unique identifier
- `authors` -- party affiliation(s) of the individual sponsor(s)
- `sponsors` -- individual sponsor(s), as numeric identifiers
- `n_au` -- number of sponsors on the bill

## Sponsors

The sponsors data have multiple rows for each sponsor (one per legislature in which the sponsor sat).

- `url` -- unique numeric identifier, from the profile URL
- `legislature` -- legislature of activity
- `name` -- short name
- `fullname` -- full name (Portuguese names are looooong)
- `sex` -- gender (F/M), imputed from status ("deputado/a") and first names
- `born` -- year of birth
- `party` -- political party, abbreviated
- `constituency` -- constituency
- `photo` -- URL to the online photo of the sponsor
- `nyears` -- seniority (time in office since legislature 6), in years

Constituencies are standardized to Wikipedia Português handles (except for "Portuguese outside Europe"), and genders have been checked to ensure that male and female names do not overlap.
