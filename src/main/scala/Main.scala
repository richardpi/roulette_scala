import scala.math

object Main extends App {
  //iterations start from 0, so e.g. 3 is max subsequent gamble 4 times
  val maxIterations = 6
  val initialBet = 5
  val betChoice = Casino.RouletteOptions.BLACK

  Casino.play(initialBet, maxIterations, betChoice)
}

object Casino {

  def play(initialBet: Int, maxIterations: Int, betChoice:RouletteOptions.EnumVal):Unit = {
    //algorithm increasing bet 2^n * bet; 0...n
    def betIncrease(power: Int): Int = math.pow(2, power).toInt * initialBet

    val moneyRequired = minMoneyRequired(maxIterations, betIncrease)

    val maxBets = maxIterations + 1
    println(s"you need £$moneyRequired per game session " +
      s"to make maximum $maxBets subsequential bets if you start with £$initialBet")

    var totalMoney = 0
    val rouletteStats = new RouletteStats

    val gameSessions = 1000
    for (i <- 1 to gameSessions) {
      totalMoney += doGamble(betChoice, maxIterations, betIncrease, rouletteStats)
    }

    println(s"after playing $gameSessions game sessions, you have £$totalMoney")

    //some stats to ensure random is as realistic as possible
    println("all games total black: " + rouletteStats.totalBlack + " (" + rouletteStats.getPercent(rouletteStats.totalBlack) + "%)")
    println("all games total red: " + rouletteStats.totalRed + " (" + rouletteStats.getPercent(rouletteStats.totalRed) + "%)")
    println("all games total green: " + rouletteStats.totalGreen + " (" + rouletteStats.getPercent(rouletteStats.totalGreen) + "%)")
  }

  def doGamble(betChoice:RouletteOptions.EnumVal, maxIterations: Int, f: Int => Int, stats: RouletteStats = null): Int = {

    def gamble(iteration: Int, money: Int): Int = {
      val betAmount = f(iteration)

      val currentAmountMoney = money - betAmount
      val rouletteMoney = Roulette.play(betAmount, betChoice, stats)

      //if you lost(0), play again, unless you played maxIterations
      if (0 == rouletteMoney && iteration < maxIterations) {
        gamble(iteration+1, currentAmountMoney)
      } else {
        return currentAmountMoney + rouletteMoney
      }
    }

    gamble(0, 0)
  }

  object Roulette {
    val roulette = RouletteOptions.black_18_and_red_18
    //val roulette = RouletteOptions.black_18_and_red_18_and_1_green
    //val roulette = RouletteOptions.black_18_and_red_18_and_2_green

    def play(bet: Int, betChoice: RouletteOptions.EnumVal, stats: RouletteStats = null): Int = {
      val idx:Int = scala.util.Random.nextInt(roulette.length)
      val random:RouletteOptions.EnumVal = (roulette(idx))

      stats match {
        case stats:RouletteStats => stats.add(random)
        case _ =>
      }

      if (random == betChoice) bet*2
      else 0
    }
  }

  class RouletteStats {
    var totalBlack = 0
    var totalRed = 0
    var totalGreen = 0

    def getTotal = totalBlack + totalRed + totalGreen

    def add(elem: RouletteOptions.EnumVal) = {

      elem match {
        case RouletteOptions.BLACK => totalBlack += 1
        case RouletteOptions.RED => totalRed += 1
        case RouletteOptions.GREEN => totalGreen += 1
      }

    }

    def getPercent(number: Int) = "%.2f".format(number.toDouble / getTotal.toDouble * 100)
  }

  object RouletteOptions extends Enumeration {

    def black_18_and_red_18 = {
      generateRoulette()
    }

    def black_18_and_red_18_and_1_green = {
      generateRoulette(1)
    }

    def black_18_and_red_18_and_2_green = {
      generateRoulette(2)
    }

    def generateRoulette(green: Int = 0) = {
      val defaultRouletteSize = 36
      val rouletteOptions = new Array[RouletteOptions.EnumVal](defaultRouletteSize + green)

      for(i <- 0 until rouletteOptions.length - green) {
        if (i % 2 == 0) rouletteOptions(i) = RouletteOptions.BLACK
        else rouletteOptions(i) = RouletteOptions.RED
      }

      for (j <- defaultRouletteSize to defaultRouletteSize + green - 1) {
        rouletteOptions(j) = RouletteOptions.GREEN
      }

      rouletteOptions
    }

    sealed trait EnumVal
    case object RED extends EnumVal
    case object BLACK extends EnumVal
    case object GREEN extends EnumVal
  }

  def minMoneyRequired(maxIterations: Int, f: Int => Int): Int = {
    def betAmount(i: Int): Int = {
      if (i <= maxIterations) f(i) + betAmount(i+1)
      else 0
    }

    betAmount(0)
  }
}
