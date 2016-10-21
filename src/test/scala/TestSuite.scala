import org.scalatest.FunSuite

class TestSuite extends FunSuite {

  import math.sqrt

  def round(x: Double, d: Int = 2) = (scala.math.pow(10,d) * x).toInt / scala.math.pow(10,d)
  // Post-processing
  def mean(x: List[Double]) = x.sum / x.size
  def sd(x: List[Double]) = {
    val xbar = mean(x)
    val n = x.size
    val sqDiff = x.map{xi => (xi-xbar)*(xi-xbar)}
    sqrt( sqDiff.sum / (n-1) )
  }

  test("aft") {
    val XTV = scala.io.Source.fromFile("src/test/resources/tongue.dat").getLines.map(x=>x.split(",").toList.map(_.toDouble)).toList
    val X = XTV.map(xtv => List(1.0,xtv(0)))
    val t = XTV.map(xtv => xtv(1))
    val v = XTV.map(xtv => xtv(2).toInt)

    import MCMC.all._
    import AFT._
    Rand.reSeed(1L)

    val B = 10000
    val burn = 5000

    val prior = new AFT.Prior(m = List(0,0), s2 = List(10,10), csb = List(1,1),
                              a = 2.0, b = 1.0, css = 1)
    val aft = new AFT(t,X,v,prior)

    val (weib,wdic) = timer{aft.sample(B,burn)}
    weib.take(2).foreach{println}
    println(Console.GREEN+"weib: "+weib.map(_.sig).sum/B+Console.RESET)
    println(Console.GREEN+"weib: "+sd(weib.map(_.sig))+Console.RESET)
    println(Console.GREEN+"weib: "+weib.map(_.beta(0)).sum/B+Console.RESET)
    println(Console.GREEN+"weib: "+weib.map(_.beta(1)).sum/B+Console.RESET)
    println(Console.GREEN+"weib: "+sd(weib.map(_.beta(0)))+Console.RESET)
    println(Console.GREEN+"weib: "+sd(weib.map(_.beta(1)))+Console.RESET)
    println(Console.GREEN+"weib DIC: "+wdic+Console.RESET)

    val (llog,lldic) = timer{aft.sample(B,burn,distr="LogLogistic")}
    println(Console.GREEN+"llog: "+llog.map(_.sig).sum/B+Console.RESET)
    println(Console.GREEN+"llog: "+sd(llog.map(_.sig))+Console.RESET)
    println(Console.GREEN+"llog: "+llog.map(_.beta(0)).sum/B+Console.RESET)
    println(Console.GREEN+"llog: "+llog.map(_.beta(1)).sum/B+Console.RESET)
    println(Console.GREEN+"llog DIC: "+lldic+Console.RESET)

    val (lnorm,lndic) = timer{aft.sample(B,burn,distr="LogNormal")}
    println(Console.GREEN+"lnorm: "+lnorm.map(_.sig).sum/B+Console.RESET)
    println(Console.GREEN+"lnorm: "+sd(lnorm.map(_.sig))+Console.RESET)
    println(Console.GREEN+"lnorm: "+lnorm.map(_.beta(0)).sum/B+Console.RESET)
    println(Console.GREEN+"lnorm: "+lnorm.map(_.beta(1)).sum/B+Console.RESET)
    println(Console.GREEN+"lnorm DIC: "+lndic+Console.RESET)
  }

}
