# hi Portugal

source("load.r")
source("functions.r")
source("parties.r")

# folders

dir.create("data"  , showWarnings = FALSE)
dir.create("plots" , showWarnings = FALSE)

if (file.exists("photos.zip"))
  unzip("photos.zip")

dir.create("photos", showWarnings = FALSE)

if (file.exists("raw.zip"))
  unzip("raw.zip")

dir.create("raw"            , showWarnings = FALSE)
dir.create("raw/bills"      , showWarnings = FALSE)
dir.create("raw/committees" , showWarnings = FALSE)
dir.create("raw/indexes"    , showWarnings = FALSE)
dir.create("raw/mps"        , showWarnings = FALSE)

# parameters

plot = TRUE
gexf = TRUE
mode = "fruchtermanreingold"
meta = c(
  "cty" = "Portugal",
  "lang" = "pt", # Wikipedia language for chamber and constituencies
  "ch" = "Assembleia da República",
  "type" = "Unicameral",
  "ipu" = 2257,
  "seats" = 230
)

# build routine

source("data.r")  # scrape bills and sponsors
source("build.r") # assemble the networks
source("comm.r")  # add committee co-membership

save(list = ls(pattern = "^(co)?(net|edges|bills)_pt\\d{4}$"),
     file = "data/net_pt.rda")

# have a nice day
