
test_let <- function() {
  d <- data.frame(
    Sepal_Length = c(5.8, 5.7),
    Sepal_Width = c(4.0, 4.4),
    Species = 'setosa',
    rank = c(1, 2)
  )

  mapping = list(RankColumn = 'rank', GroupColumn = 'Species')
  let(alias = mapping,
      expr = {
        # Notice code here can be written in terms of known or concrete
        # names "RankColumn" and "GroupColumn", but executes as if we
        # had written mapping specified columns "rank" and "Species".

        # restart ranks at zero.
        dres <- d
        dres$RankColumn <- dres$RankColumn - 1
      })
  RUnit::checkEquals(dres$rank, c(0,1))

  invisible(NULL)
}
