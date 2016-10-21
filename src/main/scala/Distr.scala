package Survival

object Distr {
  import org.apache.commons.math3.distribution.{NormalDistribution}
  import math.{log,exp,Pi}

  private val N01 = new NormalDistribution(0,1)

  trait ErrorDistribution {
    def logpdf(z: Double): Double
    def cdf(z: Double): Double
  }

  object Normal extends ErrorDistribution {
    def logpdf(z: Double) = -(log(2.0*Pi) + z*z)/2.0
    def cdf(z: Double) = N01.cumulativeProbability(z)
  }

  object Logistic extends ErrorDistribution {
    def logpdf(z: Double) = -z -2.0*log(1.0+exp(-z))
    def cdf(z: Double) = 1/(exp(-z) + 1)
  }

  object ExtremeValue extends ErrorDistribution {
    // Wikipedia
    //def logpdf(z: Double) = -z-exp(-z)
    //def cdf(z: Double) = exp(-exp(-z))
    // R and Stats community
    def logpdf(z: Double) = z - exp(z)
    def cdf(z: Double) = 1-exp(-exp(z))
  }

  trait TimeDistribution {
    val errDistr: ErrorDistribution
    def logpdf(t: Double, mu: Double, s: Double) = {
      assert(t > 0 && s > 0, "t,s should be > 0")
      errDistr.logpdf((log(t)-mu)/s) - log(t*s)
    }
    def logSurv(t: Double, mu: Double, s: Double) = {
      assert(t > 0 && s > 0, "t,s should be > 0")
      log(1-errDistr.cdf((log(t)-mu)/s))
    }
  }

  object LogNormal extends TimeDistribution { val errDistr = Normal }
  object LogLogistic extends TimeDistribution { val errDistr = Logistic }
  object Weibull extends TimeDistribution { val errDistr = ExtremeValue }
}
