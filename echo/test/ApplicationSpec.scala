import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

import play.api.test._
import play.api.test.Helpers._

/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 * For more information, consult the wiki.
 */
@RunWith(classOf[JUnitRunner])
class ApplicationSpec extends Specification {

  "Application" should {

    "send 404 on a bad request" in new WithApplication{
      route(FakeRequest(GET, "/boum")) must beNone
    }

    "echo nothing when msg parameter is missing" in new WithApplication{
      val home = route(FakeRequest(GET, "/")
        .withHeaders(("Content-Type", "application/x-www-form-urlencoded"))).get

      status(home) must equalTo(OK)
      contentType(home) must beSome.which(_ == "text/plain")
      contentAsString(home).length must beEqualTo(0)
    }

    "echo given message" in new WithApplication{
      val home = route(
        FakeRequest(POST, "/")
        .withHeaders(("Content-Type", "application/x-www-form-urlencoded"))
        .withFormUrlEncodedBody(
          ("msg", "hello"))).get

      status(home) must equalTo(OK)
      contentType(home) must beSome.which(_ == "text/plain")
      contentAsString(home) must contain ("hello")
    }
  }
}
