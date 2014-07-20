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

    "render empty list of videos" in new WithApplication{
      val home = route(FakeRequest(GET, "/")).get

      status(home) must equalTo(OK)
      contentType(home) must beSome.which(_ == "text/plain")
      contentAsString(home).length must beEqualTo(0)
    }

    "add video to list" in new WithApplication {
      val name = "hello"
      val url = "hello.video.example.com"
      val requestUrl = s"/?name=${name}&url=${url}&duration=134345"
      val page = route(FakeRequest(POST, requestUrl)).get

      status(page) must equalTo(OK)
      contentType(page) must beSome.which(_ == "text/plain")
      contentAsString(page) must contain("Video added.")

      val home = route(FakeRequest(GET, "/")).get
      status(home) must equalTo(OK)
      contentType(home) must beSome.which(_ == "text/plain")
      contentAsString(home) must contain(s"${name} : ${url}")
    }

    "return bad request when missing paramter" in new WithApplication {
      val name = "hello"
      val url = "hello.video.example.com"
      val page = route(FakeRequest(POST, s"/?name=${name}&url=${url}.com")).get

      status(page) must equalTo(BAD_REQUEST)
      contentType(page) must beSome.which(_ == "text/plain")
      contentAsString(page) must contain("duration")
    }

    "return bad request when malformed paramter" in new WithApplication {
      val name = "hello"
      val url = "hello.video.example.com"
      val page = route(FakeRequest(POST, s"/?name=${name}&url=${url}.com&duration=asdf")).get

      status(page) must equalTo(BAD_REQUEST)
      contentType(page) must beSome.which(_ == "text/plain")
      contentAsString(page) must contain("duration")
    }
  }
}
