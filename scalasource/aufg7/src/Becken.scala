class Becken(val temperatur: List[Double], val besucherzahlen: Array[Int]):
  def durchschnittlicheBesucherzahl(): Double =
    var summe = 0.0
    if besucherzahlen.isEmpty then 0.0
    else
      for zahl <- besucherzahlen do summe += zahl
      summe / besucherzahlen.length
  def durchschnittlicheTemperatur(): Double =
    var summe = 0.0
    if temperatur.isEmpty then 0.0
    else
      for temp <- temperatur do summe += temp
      summe / temperatur.length
  def anzahlBesucher(): Int =
    var summe = 0
    for zahl <- besucherzahlen do summe += zahl
    summe
  def maxBesucher(): Int =
    var max = 0
    if !besucherzahlen.isEmpty then for zahl <- besucherzahlen do if zahl > max then max = zahl
    max
  def minBesucher(): Int =
    var min = Int.MaxValue
    if besucherzahlen.isEmpty then 0
    else for zahl <- besucherzahlen if zahl < min do min = zahl
    min
  def maxTemperatur(): Double =
    var max = 0.0
    if temperatur.isEmpty then 0.0
    else for temp <- temperatur do if temp > max then max = temp
    max
  def minTemperatur(): Double =
    var min = 100.0
    if temperatur.isEmpty then 0.0
    else for temp <- temperatur do if temp < min then min = temp
    min
end Becken
