## ensure global parameters exist (from global.R)
stopifnot(exists(c("paths")))


## -----------------------------------------------------------------------------
message(brk(), "  finished running mapsForShiny.R [", Sys.time(), "]", "\n", brk())
