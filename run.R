source('rtest.R')

runner <- list()

xs <- c(38992, 837, 888, 92883, 83, 48)
n <- 3
runner$partition <- list(
        input=list(V.Size=xs, N=n),
        output=RTest$partition(xs, n)
)

xs <- c(1, 4, 4, 2, 2, 4, 3, 2, 7, 7, 7, 6, 6)
runner$count <- list(input=xs, output=RTest$count(xs))

xs <- as.Date(c("2014-01-23", "2014-10-11", "2014-12-17", "2009-10-23"))
runner$dates <- list(input=xs, output=RTest$dates(xs))

xs <- c(
        "Listed on 1/05/2009 for 180000 and sold for $150,250 on 3/1/2009",
        "Listed on 1-05-2009 for 180000 and sold for $150,250 on 3/1/09",
        "Listed 01-05-2009 for 180000 and sold for $150,250 on 3/01/09",
        "blah blah blah"
)
runner$parse <- list(input=xs, output=RTest$parse(xs))
