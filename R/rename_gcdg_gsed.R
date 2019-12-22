#' Rename items from gcdg into gsed lexicon
#'
#' Function `rename_gcdg_gsed()` translates item names in the
#' gcdg lexicon to item names in the gsed lexicon.
#' @param x A character vector containing item names in the gcdg lexicon
#' @param copy A logical indicating whether any unmatches names should
#' be copied (`copy = TRUE`) or set to an empty string.
#' @return A character vector of length `length(x)` with gcdg
#' item names replaced by gsed item name.
#' @details
#' The gsed-naming convention is as follows. Position 1-3 codes the
#' instrument, position 4-5 codes the domain, position 6 codes
#' direct/caregiver/message, positions 7-9 is a item sequence number.
#'
#' The function currently support ASQ-I (aqi), Barrera-Moncade (bar),
#' Batelle (bat), Bayley I (by1), Bayley II (by2), Bayley III (by3),
#' Dutch Development Instrument (ddi), Denver (den), Griffith (gri),
#' MacArthur (mac), WHO milestones (mds), Mullen (mul), pegboard (peg),
#' South African Griffith (sgr), Stanford Binet (sbi), Tepsi (tep),
#' Vineland (vin).
#'
#' In cases where the domain of the items isn't clear (vin, bar),
#' the domain is coded as 'xx'.
#' @references
#' <https://docs.google.com/spreadsheets/d/1zLsSW9CzqshL8ubb7K5R9987jF4YGDVAW_NBw1hR2aQ/edit#gid=0>
#' @author Iris Eekhout, Stef van Buuren
#' @examples
#' from <- c(
#'   "ag28", "gh2_19", "a14ps4", "b1m157", "mil6",
#'   "bm19", "a16fm4", "n22", "ag9", "gh6_5"
#' )
#' to <- rename_gcdg_gsed(from, copy = FALSE)
#' to
#' @export
rename_gcdg_gsed <- function(x, copy = TRUE) {
  aqi <- function(x) {
    domo <- gsub("a|[0-9]", "", x)
    domn <- NA
    domn <- ifelse(domo == "com", "cm", domn)
    domn <- ifelse(domo == "f", "fm", domn)
    domn <- ifelse(domo == "pbs", "px", domn)
    domn <- ifelse(domo == "g", "gm", domn)
    domn <- ifelse(domo == "ps", "sl", domn)
    nr <- gsub("[a-z]", "", x)
    nr <- str_pad(nr, 3, pad = "0")
    rep <- "c"
    instr <- "aqi"
    cbind(as.character(x), paste(instr, domn, rep, nr, sep = ""))
  }

  bar <- function(x) {
    domn <- "xx"
    nr <- gsub("[a-z]", "", x)
    nr <- str_pad(nr, 3, pad = "0")
    repi <- gsub("bm|[0-9]", "", x)
    rep <- "x"
    rep <- ifelse(grepl("a", repi), "d", rep)
    rep <- ifelse(grepl("b", repi), "c", rep)
    instr <- "bar"
    cbind(as.character(x), paste(instr, domn, rep, nr, sep = ""))
  }

  bat <- function(x) {
    domo <- substr(x, 4, 4)
    domn <- rep("", length(x))
    domn <- ifelse(domo == "c", "cg", domn)
    domn <- ifelse(domo == "1", "cm", domn)
    domn <- ifelse(domo == "a", "ad", domn)
    domn <- ifelse(domo == "m", "mo", domn)
    domn <- ifelse(domo == "s", "sl", domn)
    nr <- str_pad(unlist(lapply(strsplit(x, "_z"), `[[`, 2)),
      3,
      pad = "0"
    )
    rep <- "d"
    instr <- "bat"
    cbind(as.character(x), paste(instr, domn, rep, nr, sep = ""))
  }

  by1 <- function(x) {
    domo <- gsub("b|[0-9]", "", x)
    domn <- NA
    domn <- ifelse(domo == "m", "md", domn)
    domn <- ifelse(domo == "p", "pd", domn)
    nr <- gsub("b1p|b1m", "", x)
    nr <- str_pad(nr, 3, pad = "0")
    rep <- "d"
    instr <- "by1"
    cbind(as.character(x), paste(instr, domn, rep, nr, sep = ""))
  }

  by2 <- function(x) {
    domo <- gsub("b|[0-9]", "", x)
    domn <- NA
    domn <- ifelse(domo == "m", "md", domn)
    domn <- ifelse(domo == "p" | domo == "g", "pd", domn)
    # 3 items with g instead of p with same label b2g102 b2g103 b2g109
    nr <- gsub("b2p|b2m|b2g", "", x)
    nr <- str_pad(nr, 3, pad = "0")
    rep <- "d"
    instr <- "by2"
    cbind(as.character(x), paste(instr, domn, rep, nr, sep = ""))
  }

  by3 <- function(x) {
    domo <- gsub("b|[0-9]", "", x)
    domn <- NA
    domn <- ifelse(domo == "c", "cg", domn)
    domn <- ifelse(domo == "f", "fm", domn)
    domn <- ifelse(domo == "e", "ex", domn)
    domn <- ifelse(domo == "r", "re", domn)
    domn <- ifelse(domo == "g", "gm", domn)
    nr <- gsub("b3|[a-z]", "", x)
    nr <- str_pad(nr, 3, pad = "0")
    rep <- "d"
    instr <- "by3"
    cbind(as.character(x), paste(instr, domn, rep, nr, sep = ""))
  }

  ddi <- function(x) {
    fm <- c(
      1, 7, 8, 9, 13, 14, 19, 20, 21, 27, 32, 33, 38,
      39, 44, 45, 51, 52, 53, 54
    )
    cm <- c(
      2, 6, 10, 25, 31, 30, 37, 40, 47, 55, 56, 16, 36,
      41, 48
    )
    gm <- c(
      3, 4, 11, 15, 5, 12, 18, 17, 14, 22, 23, 24, 26,
      28, 29, 34, 35, 42, 50, 43, 49, 57, 46
    )
    domn <- rep("xx", length(x))
    nr <- gsub("n|v", "", x)
    domn <- ifelse(grepl("n", x) & nr %in% fm, "fm", domn)
    domn <- ifelse(grepl("n", x) & nr %in% cm, "cm", domn)
    domn <- ifelse(grepl("n", x) & nr %in% gm, "gm", domn)
    domn <- ifelse(grepl("v", x) & nr < 30, "fm", domn)
    domn <- ifelse(grepl("v", x) & nr > 30 & nr < 52, "cm", domn)
    domn <- ifelse(grepl("v", x) & nr > 66, "gm", domn)
    nr <- ifelse(grepl("n1", x), 1, nr)
    nr <- ifelse(grepl("n2", x), 29, nr)
    nr <- ifelse(grepl("n3", x), 52, nr)
    nr <- ifelse(grepl("n4", x), 53, nr)
    nr <- ifelse(grepl("n5", x), 56, nr)
    nr <- ifelse(grepl("n6", x), 30, nr)
    nr <- ifelse(grepl("n7", x), 2, nr)
    nr <- ifelse(grepl("n8", x), 3, nr)
    nr <- ifelse(grepl("n9", x), 4, nr)
    nr <- ifelse(grepl("n10", x), 31, nr)
    nr <- ifelse(grepl("n11", x), 54, nr)
    nr <- ifelse(grepl("n12", x), 57, nr)
    nr <- ifelse(grepl("n13", x), 5, nr)
    nr <- ifelse(grepl("n14", x), 6, nr)
    nr <- ifelse(grepl("n15", x), 55, nr)
    nr <- ifelse(grepl("n16", x), 116, nr)
    nr <- ifelse(grepl("n17", x), 59, nr)
    nr <- ifelse(grepl("n18", x), 58, nr)
    nr <- ifelse(grepl("n19", x), 7, nr)
    nr <- ifelse(grepl("n20", x), 8, nr)
    nr <- ifelse(grepl("n21", x), 9, nr)
    nr <- ifelse(grepl("n22", x), 60, nr)
    nr <- ifelse(grepl("n23", x), 61, nr)
    nr <- ifelse(grepl("n24", x), 62, nr)
    nr <- ifelse(grepl("n25", x), 33, nr)
    nr <- ifelse(grepl("n26", x), 63, nr)
    nr <- ifelse(grepl("n27", x), 10, nr)
    nr <- ifelse(grepl("n28", x), 64, nr)
    nr <- ifelse(grepl("n29", x), 65, nr)
    nr <- ifelse(grepl("n30", x), 36, nr)
    nr <- ifelse(grepl("n31", x), 34, nr)
    nr <- ifelse(grepl("n32", x), 11, nr)
    nr <- ifelse(grepl("n33", x), 12, nr)
    nr <- ifelse(grepl("n34", x), 66, nr)
    nr <- ifelse(grepl("n35", x), 67, nr)
    nr <- ifelse(grepl("n36", x), 136, nr)
    nr <- ifelse(grepl("n37", x), 37, nr)
    nr <- ifelse(grepl("n38", x), 13, nr)
    nr <- ifelse(grepl("n39", x), 14, nr)
    nr <- ifelse(grepl("n40", x), 39, nr)
    nr <- ifelse(grepl("n41", x), 141, nr)
    nr <- ifelse(grepl("n42", x), 68, nr)
    nr <- ifelse(grepl("n43", x), 69, nr)
    nr <- ifelse(grepl("n44", x), 15, nr)
    nr <- ifelse(grepl("n45", x), 16, nr)
    nr <- ifelse(grepl("n46", x), 146, nr)
    nr <- ifelse(grepl("n47", x), 41, nr)
    nr <- ifelse(grepl("n48", x), 148, nr)
    nr <- ifelse(grepl("n49", x), 70, nr)
    nr <- ifelse(grepl("n50", x), 168, nr)
    nr <- ifelse(grepl("n51", x), 17, nr)
    nr <- ifelse(grepl("n52", x), 18, nr)
    nr <- ifelse(grepl("n53", x), 19, nr)
    nr <- ifelse(grepl("n54", x), 154, nr)
    nr <- ifelse(grepl("n55", x), 43, nr)
    nr <- ifelse(grepl("n56", x), 44, nr)
    nr <- ifelse(grepl("n57", x), 71, nr)
    nr <- ifelse(grepl("v20", x), 20, nr)
    nr <- ifelse(grepl("v21", x), 21, nr)
    nr <- ifelse(grepl("v22", x), 22, nr)
    nr <- ifelse(grepl("v23", x), 23, nr)
    nr <- ifelse(grepl("v24", x), 24, nr)
    nr <- ifelse(grepl("v25", x), 25, nr)
    nr <- ifelse(grepl("v26", x), 27, nr)
    nr <- ifelse(grepl("v27", x), 26, nr)
    nr <- ifelse(grepl("v31", x), 32, nr)
    nr <- ifelse(grepl("v32", x), 132, nr)
    nr <- ifelse(grepl("v35", x), 35, nr)
    nr <- ifelse(grepl("v38", x), 38, nr)
    nr <- ifelse(grepl("v40", x), 40, nr)
    nr <- ifelse(grepl("v42", x), 42, nr)
    nr <- ifelse(grepl("v45", x), 45, nr)
    nr <- ifelse(grepl("v46", x), 46, nr)
    nr <- ifelse(grepl("v47", x), 47, nr)
    nr <- ifelse(grepl("v48", x), 48, nr)
    nr <- ifelse(grepl("v49", x), 49, nr)
    nr <- ifelse(grepl("v50", x), 50, nr)
    nr <- ifelse(grepl("v72", x), 72, nr)
    nr <- ifelse(grepl("v73", x), 268, nr)
    nr <- ifelse(grepl("v74", x), 73, nr)
    nr <- ifelse(grepl("v75", x), 74, nr)
    mitem <- c(
      4, 9, 12, 29:38, 60, 64, 65, 66, 67,
      14, 16, 19, 25, 39:43, 45:48, 50, 51, 73
    )
    rep <- ifelse(nr %in% mitem, "m", "d")
    nr <- str_pad(nr, 3, pad = "0")
    instr <- "ddi"
    cbind(as.character(x), paste(instr, domn, rep, nr, sep = ""))
  }

  den <- function(x) {
    domo <- gsub("d|[0-9]", "", x)
    domn <- NA
    domn <- ifelse(domo == "l", "lg", domn)
    domn <- ifelse(domo == "f", "fm", domn)
    domn <- ifelse(domo == "g", "gm", domn)
    domn <- ifelse(domo == "p", "sl", domn)
    nr <- gsub("[a-z]", "", x)
    nr <- str_pad(nr, 3, pad = "0")
    rep <- "d"
    instr <- "den"
    cbind(as.character(x), paste(instr, domn, rep, nr, sep = ""))
  }

  gri <- function(x) {
    domo <- gsub("g|[0-9]|_", "", x)
    domn <- NA
    domn <- ifelse(domo == "c", "cg", domn)
    domn <- ifelse(domo == "e", "eh", domn)
    domn <- ifelse(domo == "h", "hs", domn)
    domn <- ifelse(domo == "p", "re", domn)
    domn <- ifelse(domo == "", "gm", domn)
    nr <- gsub("g|[a-z]", "", x)
    nr <- ifelse(nchar(nr) > 3, gsub("_", "", nr), nr)
    nr <- gsub("_", "0", nr)
    nr <- str_pad(nr, 3, pad = "0")
    rep <- "d"
    instr <- "gri"
    cbind(as.character(x), paste(instr, domn, rep, nr, sep = ""))
  }

  mac <- function(x) {
    domn <- "gm"
    nr <- gsub("mg", "", x)
    nr <- str_pad(nr, 3, pad = "0")
    nr <- ifelse(nr == "04a", "041", nr)
    nr <- ifelse(nr == "04b", "042", nr)
    rep <- "d"
    instr <- "mac"
    cbind(as.character(x), paste(instr, domn, rep, nr, sep = ""))
  }

  mds <- function(x) {
    domn <- "gm"
    nr <- gsub("mil", "", x)
    nr <- str_pad(nr, 3, pad = "0")
    rep <- "d"
    instr <- "mds"
    cbind(as.character(x), paste(instr, domn, rep, nr, sep = ""))
  }

  mul <- function(x) {
    domo <- substr(x, 4, 4)
    domn <- NA
    domn <- ifelse(domo == "v", "cg", domn)
    domn <- ifelse(domo == "f", "fm", domn)
    domn <- ifelse(domo == "r", "re", domn)
    domn <- ifelse(domo == "g", "gm", domn)
    domn <- ifelse(domo == "e", "ex", domn)
    domn <- ifelse(domo == "s", "se", domn)
    nr <- gsub("[a-z]", "", x)
    tr <- substr(x, nchar(x), nchar(x))
    ad <- rep("0", length(x))
    ad <- ifelse(tr == "a", "0", ad)
    ad <- ifelse(tr == "b", "1", ad)
    ad <- ifelse(tr == "c", "2", ad)
    ad <- ifelse(tr == "d", "3", ad)
    ad <- ifelse(tr == "e", "4", ad)
    ad <- ifelse(tr == "f", "5", ad)
    nr <- paste0(nr, ad)
    nr <- str_pad(nr, 3, pad = "0")
    rep <- "d"
    instr <- "mul"
    cbind(as.character(x), paste(instr, domn, rep, nr, sep = ""))
  }

  peg <- function(x) {
    domn <- "fm"
    nr <- gsub("peg", "", x)
    nr <- str_pad(nr, 3, pad = "0")
    rep <- "d"
    instr <- "peg"
    cbind(as.character(x), paste(instr, domn, rep, nr, sep = ""))
  }

  sgr <- function(x) {
    domo <- gsub("sag|[0-9]", "", x)
    domn <- NA
    domn <- ifelse(domo == "c", "cg", domn)
    domn <- ifelse(domo == "e", "eh", domn)
    domn <- ifelse(domo == "h" | domo == "hs", "hs", domn)
    domn <- ifelse(domo == "p", "fm", domn)
    domn <- ifelse(domo == "l", "gm", domn)
    domn <- ifelse(domo == "ps" | domo == "s", "re", domn)
    nr <- gsub("[a-z]", "", x)
    nr <- str_pad(nr, 3, pad = "0")
    rep <- "d"
    instr <- "sgr"
    cbind(as.character(x), paste(instr, domn, rep, nr, sep = ""))
  }

  sbi <- function(x) {
    domo <- gsub("sb|[0-9]", "", x)
    domn <- NA
    domn <- ifelse(domo == "w", "wm", domn)
    domn <- ifelse(domo == "v", "vs", domn)
    domn <- ifelse(domo == "f", "fr", domn)
    domn <- ifelse(domo == "s", "sl", domn)
    nr <- gsub("[a-z]", "", x)
    nr <- ifelse(nchar(nr) == 1, paste0(nr, "0"), nr)
    nr <- str_pad(nr, 3, pad = "0")
    rep <- "d"
    instr <- "sbi"
    cbind(as.character(x), paste(instr, domn, rep, nr, sep = ""))
  }

  tep <- function(x) {
    nr <- gsub("j|b|c|__[0-9]", "", x)
    domn <- NA
    domn <- ifelse(nr <= 16, "co", domn)
    domn <- ifelse(nr > 16 & nr <= 40, "lg", domn)
    domn <- ifelse(nr > 40, "mo", domn)
    nr <- gsub("[a-z]|__", "", x)
    nr <- str_pad(nr, 3, pad = "0")
    rep <- "d"
    instr <- "tep"
    cbind(as.character(x), paste(instr, domn, rep, nr, sep = ""))
  }

  vin <- function(x) {
    nr <- gsub("sa2v", "", x)
    domn <- "xx"
    nr <- ifelse(grepl("playswith", x), "31", nr)
    nr <- ifelse(grepl("crayondraw", x), "32", nr)
    nr <- ifelse(grepl("avoidda", x), "33", nr)
    nr <- ifelse(grepl("buttons", x), "34", nr)
    nr <- ifelse(grepl("compete", x), "35", nr)
    nr <- ifelse(grepl("downstar", x), "36", nr)
    nr <- ifelse(grepl("dryhand", x), "37", nr)
    nr <- ifelse(grepl("eatfood", x), "38", nr)
    nr <- ifelse(grepl("getwater", x), "39", nr)
    nr <- ifelse(grepl("helpshou", x), "40", nr)
    nr <- ifelse(grepl("narrates", x), "41", nr)
    nr <- ifelse(grepl("onjacket", x), "42", nr)
    nr <- ifelse(grepl("performs", x), "43", nr)
    nr <- ifelse(grepl("scissors", x), "44", nr)
    nr <- ifelse(grepl("selftoil", x), "45", nr)
    nr <- ifelse(grepl("takeoff", x), "46", nr)
    nr <- ifelse(grepl("washface", x), "47", nr)
    nr <- ifelse(grepl("washhands", x), "48", nr)
    nr <- ifelse(grepl("playself", x), "49", nr)
    nr <- ifelse(grepl("ytoilet", x), "50", nr)
    nr <- ifelse(grepl("dressself", x), "51", nr)
    nr <- str_pad(nr, 3, pad = "0")
    rep <- "c"
    instr <- "vin"
    cbind(as.character(x), paste(instr, domn, rep, nr, sep = ""))
  }

  convert <- function(x, y, n, pat, func) {
    idx <- substr(x, 1, n) %in% pat
    if (!any(idx)) {
      return(y)
    }
    y[idx] <- func(x[idx])[, 2]
    y
  }

  y <- x
  if (!copy) y <- rep("", length(y))

  y <- convert(x, y, 2, c("ac", "af", "ap", "ag"), aqi)
  y <- convert(x, y, 2, c("bm"), bar)
  y <- convert(x, y, 3, c("bat"), bat)
  y <- convert(x, y, 2, c("b1"), by1)
  y <- convert(x, y, 2, c("b2"), by2)
  y <- convert(x, y, 2, c("b3"), by3)
  y <- convert(x, y, 1, c("n", "v"), ddi)
  y <- convert(x, y, 1, c("d"), den)
  y <- convert(x, y, 1, c("g"), gri)
  y <- convert(x, y, 2, c("mg"), mac)
  y <- convert(x, y, 3, c("mil"), mds)
  y <- convert(x, y, 3, c("mul"), mul)
  y <- convert(x, y, 3, c("peg"), peg)
  y <- convert(x, y, 3, c("sag"), sgr)
  y <- convert(x, y, 2, c("sb"), sbi)
  y <- convert(x, y, 1, c("j"), tep)
  y <- convert(x, y, 4, c("sa2v", "savi", "savn"), vin)

  y
}
