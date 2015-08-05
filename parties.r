# party colors

colors = c(
  "BE" = "#B2182B",      # -- dark red
  "PEV" = "#4DAF4A",     # -- green
  "PCP" = "#E41A1C",     # -- red
  "PS" = "#F781BF",      # -- pink
  "PSD" = "#FF7F00",     # -- orange
  "CDS-PP" = "#80B1D3",  # -- light blue
  "PSN" = "#FFFF33"      # -- yellow
)

# party names

groups = c(
  "BE" = "Bloco de Esquerda",
  "PEV" = "Partido Ecologista – Os Verdes",
  "PCP" = "Partido Comunista Português",
  "PS" = "Partido Socialista",
  "PSD" = "Partido Social Democrata",
  "CDS-PP" = "Centro Democrático e Social – Partido Popular",
  "PSN" = "Partido da Solidariedade Nacional"
)

# ParlGov Left/Right scores

scores = c(
  "BE" = 1.6,
  "PEV" = 1.7,
  "PCP" = 2.2,
  "PS" = 4.0,
  "PSD" = 6.3,
  "CDS-PP" = 8.0,
  "PSN" = Inf # missing
)

stopifnot(names(colors) == names(groups))
stopifnot(names(colors) == names(scores))

