package s1

import org.scalatra.test.scalatest._

class SmallWebServerTests extends ScalatraFunSuite {

  addServlet(classOf[SmallWebServer], "/*")

  test("GET / on SmallWebServer should return status 200") {
    get("/") {
      status should equal (200)
    }
  }

}
