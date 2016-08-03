import laziness._

val as = Stream(1, 2, 3, 2)
as.takeWhileByFold( i => i < 3 ).toList
as.headOptionByFold

Stream.constant(1).take(10).toList
Stream.from(1).take(9).toList
Stream.fibs.take(7).toList
Stream.fibsByUnfold.take(7).toList
Stream.fromByUnfold(2).take(2).toList
Stream.constantByUnfold(3).take(4).toList
Stream.onesByUnfold.take(3).toList
