package controllers.admin

import com.gu.googleauth.{Actions, GoogleAuthConfig, UserIdentity}
import play.api.mvc.Security.AuthenticatedBuilder
import play.api.mvc.{Action, Call, Controller}
import model.NoCache
import play.api.Environment

object AuthActions extends Actions {

  override def authConfig: GoogleAuthConfig = conf.GoogleAuth.getConfigOrDie

  val loginTarget: Call = routes.OAuthLoginAdminController.login()

  object AuthActionTest extends AuthenticatedBuilder(r =>
    UserIdentity.fromRequest(r), r => sendForAuth(r)
  )
}

class AdminIndexController (implicit env: Environment) extends Controller {

  def index() = Action { Redirect("/admin") }

  def admin() = Action { implicit request =>
    NoCache(Ok(views.html.admin()))
  }
}
