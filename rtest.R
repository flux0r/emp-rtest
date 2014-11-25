RTest <- list()

RTest$partition <- function (V.Size, N) {
        vs <- sort(V.Size, decreasing=TRUE)
        r <- list()
        N_ <- 1
        selection <- c(TRUE, rep(FALSE, N - 1))
        while (N_ <= N) {
                # Chop off the first N_ elements and then cycle.
                r[[N_]] <- vs[N_:length(vs)][selection]
                N_ <- N_ + 1
        }
        r
}

RTest$count <- function (V.Ints) {
        r <- as.data.frame(table(V.Ints))
        r[r$Freq > 1, ]
}

# Days after today will have negative values, as will quarter numbers. So
# tomorrow will have a value -1 for days and also -1 for quarter number. The
# function also assumes the ``V.dates'' argument is already a vector of Date
# objects, not a vector of character strings.
RTest$dates <- function (V.dates) {
        now <- Sys.Date()
        qlength <- 91
        days <- as.integer(now - V.dates)
        quarter <- floor(days/91)
        data.frame(dt=V.dates, days=days, quarter=quarter)
}

# If nothing matches, the output will still have an entry in the list, but both
# vectors will have one element that is the empty string.
RTest$parse <- function (V.text) {
        # Dates can be separated by a slash or a hyphen-minus. The month and
        # day part can have 1 or two characters and the year part can have
        # either 2 or 4 characters.
        regexp.dt <- "\\d{1,2}(/|-)\\d{1,2}?(/|-)(\\d{4}|\\d{2})"
        # The lookarounds are to make sure we don't match numbers that are part
        # of a date.
        regexp.num <- "(?<!(-|/|\\d))(\\$?(\\d|,)+)(?!(-|/))"
        i <- 1
        r <- list()
        while (i <= length(V.text)) {
                txt <- V.text[i]
                r.entry <- list()
                m.dt <- gregexpr(regexp.dt, txt, perl=TRUE)[[1]]
                cat(m.dt, '\n')
                r.entry$date.candidates <- mapply(function (s, n) {
                                substr(txt, s, s + n - 1)
                        },
                        m.dt,
                        attr(m.dt, 'match.length'))
                m.num <- gregexpr(regexp.num, txt, perl=TRUE)[[1]]
                cat(m.num, '\n')
                r.entry$number.candidates <- mapply(function (s, n) {
                                substr(V.text[i], s, s + n - 1)
                        },
                        m.num,
                        attr(m.num, 'match.length'))
                # If the date begins with a zero, we'll have a false positive.
                ix <- r.entry$number.candidates != '0'
                r.entry$number.candidates <- r.entry$number.candidates[ix]
                cat(r.entry$date.candidates, '\n')
                cat(r.entry$number.candidates, '\n')
                r[[i]] <- r.entry
                i <- i + 1
        }
        r
}
