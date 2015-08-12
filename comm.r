# committee compositions available only for l. 10-12

# due to bad server-side parameters, the scraper will fail to get the inquiry
# committees from l. 11-12 and will also fail to get a few more subcommittees

r = "http://www.parlamento.pt/sites/COM/"
c = data_frame()

for (i in c("XII", "XI", "X")) {

  for (j in c(ifelse(i == "X", "ComissoesInquerito", "Comiss%c3%b5esdeInquerito"),
              "ComissoesPermanentes", "ComissoesEventuais")) {

    f = paste0("raw/indexes/committees-", i, "-", j, ".html")

    if (!file.exists(f))
      try(download.file(paste0(r, i, "LEG/Paginas/", j, ".aspx"), f,
                        mode = "wb", quiet = TRUE), silent = TRUE)

    if (!file.info(f)$size) {

      cat("Failed to download committee index", f, "\n")
      file.remove(f)
      next

    }

    h = html(f) %>%
      html_nodes("tr.ARTabResultadosLinhaPar a") %>%
      html_attr("href")

    cat("Legislature", i, j, ":", length(h), "committee(s)\n")

    for (k in h) {

      f = paste0("raw/committees/committee-", i, "-", basename(k), ".html")

      if (!file.exists(f))
        try(download.file(paste0("http://www.parlamento.pt", k,
                                 "/Apresentacao/Paginas/Composicao.aspx"), f,
                          mode = "wb", quiet = TRUE), silent = TRUE)

      if (!file.info(f)$size) {

        cat("Failed to download committee", k, "\n")
        file.remove(f)
        next

      }

      h = html(f) %>%
        html_nodes(xpath = "//a[contains(@href, 'Biografia.aspx')]") %>%
        html_attr("href")

      if (length(h) > 0)
        c = rbind(c, data_frame(
          legislature = i,
          type = j,
          name = basename(k),
          n_members = length(h),
          members = gsub("\\D", "", h) %>% paste0(collapse = ";")
        ))

    }

  }

}

write.csv(c, "data/committees.csv", row.names = FALSE)

# unique identifier
c$id = paste0(c$legislature, c$name)

cat("Building co-membership matrix...\n")

comm = data_frame(u = paste0(c$legislature, c$name))
sponsors = unlist(strsplit(c$members, ";")) %>% unique

# add sponsor columns
for (i in sponsors)
  comm[, i ] = 0

for (i in colnames(comm)[ -1 ])
  comm[ , i ] = as.numeric(sapply(c$members, strsplit, ";") %>%
                             sapply(function(x) i %in% x))

# sanity check
stopifnot(rowSums(comm[, -1 ]) == c$n_members)

# convert legislature numbers to years
c$legislature = substr(legislatures[ c$legislature ], 1, 4)

# assign co-memberships to networks for l. 10-12
for (i in ls(pattern = "^net_")[ 5:7 ]) {

  n = get(i)

  sp = network.vertex.names(n)
  names(sp) = gsub("\\D", "", n %v% "url")

  missing = !names(sp) %in% colnames(comm)
  if (sum(missing) > 0) {

    cat(i, ": adding", sum(missing), "MP(s) with no membership(s)\n")
    for (j in names(sp)[ missing ]) {
      comm[, j ] = 0
    }

  }

  m = comm[ comm$u %in% c$id[ c$legislature == gsub("\\D", "", i) ], names(sp) ]

  cat(i, ":", network.size(n), "nodes", nrow(m), "committees", ncol(m), "MPs")

  M = m

  m = t(as.matrix(m)) # sponsors in rows, committees in columns
  m = m %*% t(m) # adjacency matrix

  stopifnot(ncol(m) == network.size(n))
  colnames(m) = sp[ colnames(m) ]
  rownames(m) = sp[ rownames(m) ]

  e = data_frame(i = n %e% "source", j = n %e% "target")
  e$committee = NA

  for (j in 1:nrow(e))
    e$committee[ j ] = m[ e$i[ j ], e$j[ j ] ]

  cat(" co-memberships:",
      str_pad(paste0(range(e$committee), collapse = "-"), 6, "right"),
      sum(e$committee == 0), "null,",
      sum(e$committee == 1), "single,",
      sum(e$committee > 1), "> 1\n")

  nn = network(e[, 1:2], directed = FALSE)
  nn %e% "committee" = e$committee

  print(table(nn %e% "committee", exclude = NULL))
  stopifnot(!is.na(nn %e% "committee"))

  n %e% "committee" = e$committee
  assign(i, n)

  nn %n% "committees" = as.table(rowSums(M))
  assign(paste0("co", i), nn)

}
