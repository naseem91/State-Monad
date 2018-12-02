object PriceCalculatorWithMonad {

  import Stubs._
  import State._

  case class PriceState(productId: String, stateCode: String, price: Double)

  def findBasePrice(ps: PriceState): Double = {
    val basePrice = findTheBasePrice(ps.productId)
    basePrice
  }

  def applyStateSpecificDiscount(ps: PriceState): Double = {
    val discount = findStateSpecificDiscount(ps.productId, ps.stateCode)
    ps.price - discount
  }

  def applyProductSpecificDiscount(ps: PriceState): Double = {
    val discount = findProductSpecificDiscount(ps.productId)
    ps.price - discount
  }

  def applyTax(ps: PriceState): Double = {
    val tax = calculateTax(ps.productId, ps.price)
    ps.price + tax
  }

  def calculatePrice(productId: String, stateCode: String): Double = {
    def modifyPriceState(f: PriceState => Double) = modify[PriceState](s => s.copy(price = f(s)))

    val stateMonad = for {
      a <- modifyPriceState(findBasePrice)
      b <- modifyPriceState(applyStateSpecificDiscount)
      c <- modifyPriceState(applyProductSpecificDiscount)
      d <- modifyPriceState(applyTax)
    } yield (a,b,c,d)

    val initialPriceState = PriceState(productId, stateCode, 0.0)
    val finalPriceState = stateMonad.apply(initialPriceState)._1
    val finalPrice = finalPriceState.price
    finalPrice
  }

  def main(args: Array[String]): Unit = {
   println(calculatePrice("id1","state1"))
  }
}

