import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class NormalizedAppSpec extends AnyFlatSpec with should.Matchers {
  behavior of "getNormalizedData"

  it should "return normalized data for data from file data.txt in the resources directory" in {
    val path = getClass.getResource("/data.txt").getPath
    val expected = List("0,-0.8181818181818181", "1,-0.6161616161616161", "2,-1.0", "3,1.0")

    val actual = NormalizationApp.getNormalizedData(path)

    actual.toList should contain theSameElementsAs(expected)
  }
}
