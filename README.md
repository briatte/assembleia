This repository contains code to build cosponsorship networks from bills passed in of the [Portuguese Parliament](http://www.parlamento.pt/) .

- [interactive demo](http://briatte.org/assembleia)
- [static plots](http://briatte.org/assembleia/plots.html)

# HOWTO

Replicate by running `make.r` in R.

The `data.r` script downloads information on bills and sponsors. Unfortunately, the scraper is _highly_ error-prone, as explained at the top of the script. All photos should download fine, however.

The `build.r` script then assembles the edge lists and plots the networks, with the help of a few routines coded into `functions.r`. Adjust the `plot`, `gexf` and `mode` parameters to skip the plots or to change the node placement algorithm.

# DATA

## Bills

- `legislature` -- legislature number
- `session` -- legislature session
- `ref` -- bill local identifier (number/legislature)
- `title` -- title
- `bid` -- bill unique identifier
- `authors` -- party sponsors
- `sponsors` -- individual sponsors, using unique identifiers

## Sponsors

The sponsors data have multiple entries for each sponsor (one per legislature in which the sponsor sat).

- `url` -- unique numeric identifier, from the profile URL
- `legislature` -- legislature of activity
- `name` -- short name
- `fullname` -- full name (Portuguese names are looooong)
- `sex` -- gender (F/M), imputed from status ("deputado/a") and first names
- `born` -- year of birth
- `party` -- political party, abbreviated
- `constituency` -- constituency
- `photo` -- a dummy to indicate whether the sponsor has a photo or not
- `nyears` -- seniority (time in office since legislature 6), in years

Notes -- constituencies are standardized to Wikipedia English handles (except for "Portuguese outside Europe"), and genders have been checked to ensure that male and female names do not overlap.
