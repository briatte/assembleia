dir.create("raw/mps"     , showWarnings = FALSE)
dir.create("raw/bills"   , showWarnings = FALSE)
dir.create("raw/indexes" , showWarnings = FALSE)

sponsors = "data/sponsors.csv"
bills = "data/bills.csv"

# loop over all legislatures and pages

# note: due to the weirdness of ASP forms and to the overall sluggishness of
# the website, it is much safer to manually download the first page of bills
# of each legislature; setting j to start at 0 will attempt to download it
# automatically, but this is highly likely to fail

# it is also highly unlikely that the loop below will actually run as a loop:
# you will probably need to run the script several times, by removing the
# legislatures that you managed to download from the argument to the for()
# loop; this is how ridiculously dysfunctional this script is

# last, sponsor biographies are not available for legislatures 1-5, so there
# is no real use to download the bills for that period; the biographies of the
# sponsors who sat in these legislatures and in a later one do not even refer
# to their bills during that period, so just forget about it

for (i in c("XII", "XI", "X", "IX", "VIII", "VII", "VI")) {

  index = paste0("data/bills-index-", i, ".csv")

  if (!file.exists(index)) {

    u = "http://www.parlamento.pt/ActividadeParlamentar/Paginas/IniciativasLegislativas.aspx"
    p = GET(u)

    ev = html(p) %>%
      html_node(xpath = "//input[@name='__EVENTVALIDATION']") %>%
      html_attr("value")

    vs = html(p) %>%
      html_node(xpath = "//input[@name='__VIEWSTATE']") %>%
      html_attr("value")

    rd = html(p) %>%
      html_node(xpath = "//input[@name='__REQUESTDIGEST']") %>%
      html_attr("value")

    cat("Scraping using master index:",
        "EV", substr(ev, 1, 10),
        "VS", substr(vs, 1, 10),
        "RD", substr(rd, 1, 10),
        "\n")

    j = 1
    l = 2

    while (j < l) {

      j = j + 1
      cat("Scraping bills index for legislature", i, #"session", k,
          "page", sprintf("%2.0f", j))

      et = ifelse(j == 1, "null",
                  "ctl00$ctl43$g_889e27d8_462c_47cc_afea_c4a07765d8c7$ctl00$gvResults")

      #et = "ctl00$ctl43$g_889e27d8_462c_47cc_afea_c4a07765d8c7$ctl00$gvResults"

      pinfo = list("__EVENTTARGET" = et,
                   "__EVENTARGUMENT" = paste0("Page$", j),
                   "ctl00$ctl43$g_889e27d8_462c_47cc_afea_c4a07765d8c7$ctl00$ddlLeg" = i,
                   "ctl00$ctl43$g_889e27d8_462c_47cc_afea_c4a07765d8c7$ctl00$ddlTipoIniciativa" =
                     "J",
                   # "ctl00$ctl43$g_889e27d8_462c_47cc_afea_c4a07765d8c7$ctl00$ddlSL" = "",
                   "__VIEWSTATE" = vs,
                   "__EVENTVALIDATION" = ev,
                   "__VIEWSTATEGENERATOR" = "694F9F5E",
                   "__LASTFOCUS" = "",
                   "__REQUESTDIGEST" = rd)

      d = POST(u, body = pinfo, cookies = p$cookies)

      if (html(d) %>% html_node("title") %>% html_text %>% str_trim == "Erro") {

        cat(": failed\n")
        next

      }

      writeLines(content(d, "text"),
                 paste0("raw/indexes/bills-", i, # "-session-", k,
                        "-page-", sprintf("%02.0f", j), ".html"))

      ev = html(d) %>%
        html_node(xpath = "//input[@name='__EVENTVALIDATION']") %>%
        html_attr("value")

      vs = html(d) %>%
        html_node(xpath = "//input[@name='__VIEWSTATE']") %>%
        html_attr("value")

      rd = html(d) %>%
        html_node(xpath = "//input[@name='__REQUESTDIGEST']") %>%
        html_attr("value")

      cat(":",
          "EV", substr(ev, 1, 10),
          "VS", substr(vs, 1, 10),
          "RD", substr(rd, 1, 10),
          "\n")

      l = html(d) %>%
        html_nodes(xpath = "//a[starts-with(@href, 'javascript:__do')]") %>%
        html_attr("href")

      l = gsub("(.*)Page\\$(\\d+|Last|First)(.*)", "\\2", l) %>% unique
      l = l[ grepl("\\d", l) ] %>% as.integer %>% max

    }

    # build an index out of all bills from the legislature, so that it can be
    # conditionally ignored by the script is rerun

    b = data_frame()
    p = list.files("raw/indexes", pattern = paste0("bills-", i, "-"),
                   full.names = TRUE)

    for (j in p) {

      t = html(j) %>%
        html_nodes("#ctl00_ctl43_g_889e27d8_462c_47cc_afea_c4a07765d8c7_ctl00_gvResults tr")

      # session
      l = sapply(t, html_node, xpath = "td[3]")

      # bill ref
      n = sapply(t, html_node, xpath = "td[2]/span")

      # bill title
      d = sapply(t, html_node, xpath = "td[4]/a")

      # authors (parties)
      a = sapply(t, html_node, xpath = "td[5]")

      # find valid rows
      k = which(!sapply(n, is.null))

      xx = d[ k ] %>% sapply(html_attr, "href") %>% gsub("\\D", "", .)
      stopifnot(!is.na(xx))

      # subset all lists
      b = rbind(b, data_frame(
        legislature = i,
        session = l[ k ] %>% sapply(html_text) %>% as.integer,
        ref = n[ k ] %>% sapply(html_text),
        title = d[ k ] %>% sapply(html_text),
        bid = d[ k ] %>% sapply(html_attr, "href") %>% gsub("\\D", "", .),
        authors = a[ k ] %>% sapply(html_text) %>% str_trim
      ))

    }

    b = mutate(b, num = gsub("\\D", "", ref) %>% as.integer) %>%
      arrange(session, num) %>%
      filter(nchar(title) > 3) %>% # remove a few faulty rows
      select(-num)

    write.csv(b, index, row.names = FALSE)

  }

  b = read.csv(index, stringsAsFactors = FALSE)

  cat("Legislature", sprintf("%4s", i), ":",
      sprintf("%4.0f", nrow(b)), "bills\n")

  # download the bills

  for (j in b$bid) {

      f = paste0("raw/bills/bill-", j, ".html")

      if (!file.exists(f))
        try(download.file(paste0("http://www.parlamento.pt/",
                                 "ActividadeParlamentar/Paginas/",
                                 "DetalheIniciativa.aspx?BID=", j), f,
                          mode = "wb", quiet = TRUE), silent = TRUE)

      if (!file.info(f)$size) {

        cat("Failed to download bill", j, "\n")
        file.remove(f)

      }

  }

}

# parse bills and sponsors

if(!file.exists(bills)) {

  b = list.files("data", "^bills-index", full.names = TRUE) %>%
    lapply(read.csv, stringsAsFactors = FALSE) %>%
    bind_rows

  cat("Parsing", nrow(b), "bills...\n")

  p = list.files("raw/bills", pattern = "^bill-", full.names = TRUE)
  stopifnot(length(p) == nrow(b)) # sanity check

  b$sponsors = NA

  w = txtProgressBar(0, length(p), style = 3)

  for(i in p) {

    setTxtProgressBar(w, which(p == i))

    h = html(i) %>%
      html_nodes("#ctl00_ctl43_g_11b3c0cd_3bce_44ea_a8db_08db0682f787_ctl00_pnlAutoresD a") %>%
      html_attr("href")

    # add to bills
    b$sponsors[ b$bid == gsub("\\D", "", i) ] = gsub("\\D", "", h) %>%
      paste0(collapse = ";")

  }

  b$n_au = 1 + str_count(b$sponsors, ";")

  write.csv(b[ b$sponsors != "", ], bills, row.names = FALSE)

}

b = read.csv(bills, stringsAsFactors = FALSE)

# download sponsors

if(!file.exists(sponsors)) {

  a = strsplit(b$sponsors, ";") %>% unlist %>% unique
  a = paste0("/DeputadoGP/Paginas/Biografia.aspx?BID=", a)

  cat("Downloading", length(a), "sponsors...\n")

  # rerun to fix network errors

  xpath <- function(x) {
    paste0("//span[ contains(@id, '_", x, "_') and contains(@id, 'lblText') ]")
  }

  xpath_table <- function(x) {
    paste0("//table[contains(@id, '_gvTabLegs')]/tr/td[", x, "]")
  }

  s = data_frame()

  w = txtProgressBar(0, length(a), style = 3)

  for (i in a) {

    setTxtProgressBar(w, which(a == i))

    f = gsub("/DeputadoGP/Paginas/Biografia.aspx\\?BID=", "raw/mps/mp-", i) %>%
      paste0(., ".html")

    if (!file.exists(f))
      try(download.file(paste0("http://www.parlamento.pt/", i), f,
                        mode = "wb", quiet = TRUE), silent = TRUE)

    if (!file.info(f)$size) {

      cat("Failed MP", f)
      file.remove(f)

    } else {

      h = html(f)

      s = rbind(s, data_frame(
        url = gsub("\\D", "", i),
        legislature = html_nodes(h, xpath = 1 %>% xpath_table) %>%
          html_text %>%
          str_clean,
        name = html_node(h, ".NomeDeputado span") %>%
          html_text,
        fullname = html_nodes(h, xpath = "ucNome" %>% xpath) %>%
          html_text,
        sex = html_nodes(h, xpath = "ucCargosDesempenha" %>% xpath) %>%
          html_text %>% paste0(collapse = ", "),
        born = html_nodes(h, xpath = "ucDOB" %>% xpath) %>%
          html_text %>%
          substr(1, 4) %>%
          ifelse(!length(.), NA, .),
        party = html_nodes(h, xpath = 5 %>% xpath_table) %>%
          html_text,
        constituency = html_nodes(h, xpath = 4 %>% xpath_table) %>%
          html_text,
        photo = html_node(h, ".tdFotoBio img") %>%
          html_attr("src")
      ))

    }

  }

  # clean legislatures
  s$legislature = gsub("(\\w+)\\s(.*)", "\\1", s$legislature)

  # fix abroad constituency
  s$constituency[ s$constituency == "Fora da Europa" ] = "Portuguese outside Europe"

  # impute sex from mandate
  s$sex[ grepl("Deputado", s$sex) ] = "M"
  s$sex[ grepl("Deputada", s$sex) ] = "F"

  # impute sex from names
  s$sex[ !s$sex %in% c("F", "M") ] = NA
  s$sex[ is.na(s$sex) ] = sapply(s$fullname[ is.na(s$sex) ], function(x) {
    strsplit(x, " ") %>%
      unlist %>% .[1]
  })

  # list checked manually
  s$sex[ grepl("(a|bel|ês|ite|quel|men)$", s$sex) ] = "F"
  s$sex[ grepl("(o|é|ís|úl|nuel|los|quim|lipe|gos|vid|ael|tor|dre|rge|ose|guel|ui|ente|tur|ben|dor|ter|que|ior|aul|sses|al|son|Gil|briel|inis|elim|oel|ires|aime|rtim|arte)$", s$sex) ] = "M"
  table(s$sex, exclude = NULL)

  # compute seniority since legislature 6
  s$nyears = sapply(s$legislature, function(x) {
    x = legislatures[ x ]
    seq(substr(x, 1, 4) %>% as.integer,
        substr(x, 6, 9) %>% as.integer) %>%
      length
  }) %>% as.integer

  s = mutate(s, order = legislatures[ legislature ]) %>%
    arrange(name, order) %>%
    group_by(name) %>%
    mutate(nyears = cumsum(nyears) - nyears) %>%
    select(-order)

  write.csv(s, sponsors, row.names = FALSE)

}

s = read.csv(sponsors, stringsAsFactors = FALSE)

# download sponsor photos (rerun to fix network errors)

p = s$photo %>% unique

cat("\nDownloading photos for", length(p), "sponsors...\n")

w = txtProgressBar(0, length(p), style = 3)

for(i in p) {

  setTxtProgressBar(w, which(p == i))

  f = gsub("http://app.parlamento.pt/webutils/getimage.aspx\\?id=",
           "photos/", i) %>%
    gsub("&type=deputado", ".jpg", .)

  if(!file.exists(f))
    try(download.file(i, f, mode = "wb", quiet = TRUE),
        silent = TRUE)

  if(!file.info(f)$size) {

    s$photo[ s$photo == i ] = 0
    file.remove(f)

  } else {

    s$photo[ s$photo == i ] = 1

  }

}

cat("\n", n_distinct(s$url[ s$photo == "0" ]), "photos failed to download.\n")
