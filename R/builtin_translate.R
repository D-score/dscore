#' A table to translate between different lexicons (naming schema)
#'
#' The built-in variable `builtin_translate` contains a table for
#' translating among sets of item names into each other.
#'
#' @docType data
#' @format A `data.frame` with variables:
#'
#' | Name       | Label                          |
#' | ---------- | ------------------------------ |
#' | `phase1`   | Item names, Phase 1 data       |
#' | `phase2`   | Item names, Phase 2 data       |
#' | `gsed`     | gsed lexion                    |
#' | `gsed2`    | gto/gpa lexicon for LF/SF      |
#' | `gsed3`    | gl1/gs1 lexicon for LF/SF      |
#' | `gsed4`    | lf/sf lexicon for LF/SF        |
#' | `short1`   | Short item name, phase 1 order |
#' | `short2`   | Short item name, phase 2 order |
#' | `instrument` | Instrument code              |
#' | `seq_phase1` | Phase 1 order                |
#' | `seq_phase2` | Phase 2 order                |
#' | `label`    | Item label (English)           |
#'
#' @details
#' The `builtin_translate` is created by script
#' `data-raw/R/save_builtin_translate.R`.
#'
#' Updates:
#'  - July 2025 - Tranferred from gsedread package
#' @author Compiled by Stef van Buuren
#' @keywords datasets
"builtin_translate"
