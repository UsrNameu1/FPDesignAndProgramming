import laziness._

val as = Stream(1, 2, 3, 2)
as.takeWhileByFold( i => i < 3 ).toList
as.headOptionByFold
