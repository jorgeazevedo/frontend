package test

import controllers.HealthCheck
import org.scalatest.{BeforeAndAfterAll, DoNotDiscover, FlatSpec, Matchers}
import play.api.test.Helpers._
import play.api.test.Helpers
import org.scalatest.concurrent.ScalaFutures

@DoNotDiscover class CdnHealthCheckTest
  extends FlatSpec
  with Matchers
  with ConfiguredTestSuite
  with ScalaFutures
  with BeforeAndAfterAll
  with WithTestWsClient {

  "CDN health check" should "mimic the instance health check" in {
    val testPort: Int = port
    val controller = new HealthCheck(wsClient) { override val port = testPort }

    // Cache internal healthCheck results before to test endpoints
    whenReady(controller.runChecks) { _ =>
      status(controller.healthCheck()(TestRequest("/_healthcheck"))) should be (200)
      status(controller.healthCheck()(TestRequest("/_cdn_healthcheck"))) should be(200)
    }
  }
}
