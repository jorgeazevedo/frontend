package model

import com.gu.contentapi.client.model.v1.{Content => ApiContent, Element => ApiElement, Tag => ApiTag, _}
import com.gu.contentapi.client.utils.CapiModelEnrichment.RichJodaDateTime
import common.Edition
import org.joda.time.DateTime
import org.scalatest.{FlatSpec, Matchers}

class ContentTest extends FlatSpec with Matchers {
  "Trail" should "be populated properly" in {
    val imageElement = ApiElement(
      "test-picture",
      "main",
      ElementType.Image,
      Some(0),
      List(Asset(
        AssetType.Image,
        Some("image/jpeg"),
        Some("http://www.foo.com/bar"),
        Some(AssetFields(caption = Some("caption"), width = Some(55))))))

    val elements = List(
      imageElement,
      ApiElement(
        "test-audio",
        "main",
        ElementType.Audio,
        Some(0),
        Nil)
    )

    val content = ApiContent(id = "foo/2012/jan/07/bar",
      sectionId = None,
      sectionName = None,
      webPublicationDate = Some(DateTime.now().toCapiDateTime),
      webTitle = "Some article",
      webUrl = "http://www.guardian.co.uk/foo/2012/jan/07/bar",
      apiUrl = "http://content.guardianapis.com/foo/2012/jan/07/bar",
      //mediaAssets = media,
      tags = List(tag("type/article")),
      elements = Some(elements)
    )

    val trail: Trail = Content(content)

    trail.linkText should be("Some article")
    trail.url should be("/foo/2012/jan/07/bar")
    trail.mainPicture.flatMap(_.largestImage.flatMap(_.url)) should be (Some("http://www.foo.com/bar"))
  }

  "Tags" should "understand tag types" in {

    val theKeywords = Seq(Tag(tag("/keyword1", "keyword")), Tag(tag("/keyword2", "keyword")))
    val theSeries = Seq(Tag(tag("/series", "series")))
    val theContributors = Seq(Tag(tag("/contributor", "contributor")))
    val theTones = Seq(Tag(tag("/tone", "tone")))
    val theBlogs = Seq(Tag(tag("/blog", "blog")))
    val theTypes = Seq(Tag(tag("/type", "type")))

    val tags = new Tags {
      override val tags = theBlogs ++ theTones ++ theContributors ++ theSeries ++ theKeywords ++ theTypes
      override def isSponsored(maybeEdition: Option[Edition]): Boolean = false
      override val isFoundationSupported: Boolean = false
      override val isAdvertisementFeature: Boolean = false
    }

    tags.keywords should be(theKeywords)

    tags.contributors should be(theContributors)

    tags.blogs should be(theBlogs)

    tags.tones should be(theTones)

    tags.series should be(theSeries)

    tags.types should be(theTypes)
  }

  "Content" should "understand that in body pictures are not main pictures" in {

    val testContent = content("article", List(image("test-image-0","body", "body picture 1", 50, 0),
                                              image("test-image-1","body", "body picture 2", 50, 0),
                                              image("test-image-2","main", "main picture 1", 50, 0)))

    testContent.mainPicture.flatMap(_.largestImage.flatMap(_.caption)) should be(Some("main picture 1"))
  }

  it should "detect if content is commentable" in{
    val noFields = article.copy(fields = None)
    Content(noFields).isCommentable should be(false)

    val notCommentable= article.copy(fields = Some(ContentFields(commentable = Some(false))))
    Content(notCommentable).isCommentable should be(false)

    val commentable = article.copy(fields = Some(ContentFields(commentable = Some(true))))
    commentable.fields.flatMap(_.commentable) should be(Some("true"))
    Content(commentable).isCommentable should be(true)

  }

  it should "detect if content is closed for comments" in{
    val noFields = article.copy(fields = None)
    Content(noFields).isClosedForComments should be(true)

    val future = Some(new DateTime().plusDays(3).toCapiDateTime)
    val openComments= article.copy(fields = Some(ContentFields(commentCloseDate = future)))
    Content(openComments).isClosedForComments should be(false)

    val past = Some(new DateTime().minus(3).toCapiDateTime)
    val closedComments = article.copy(fields = Some(ContentFields(commentCloseDate = past)))
    Content(closedComments).isClosedForComments should be(true)
  }

  it should "realise that it should not show ads" in {
    val sensitive = article.copy(fields =  Some(ContentFields(shouldHideAdverts = Some(true))))

    Content(article).shouldHideAdverts should be(false)
    Content(sensitive).shouldHideAdverts should be(true)
  }

  it should "detect if article requires membershipAccess" in {

    conf.switches.Switches.MembersAreaSwitch.switchOn()

    val membershipArticle = ApiContent(id = "membership/2015/jan/01/foo",
      sectionId = None,
      sectionName = None,
      webPublicationDate = Some(new DateTime().toCapiDateTime),
      webTitle = "Some article",
      webUrl = "http://www.guardian.co.uk/membership/2015/jan/01/foo",
      apiUrl = "http://content.guardianapis.com/membership/2015/jan/01/foo",
      tags = List(tag("type/article")),
      fields = Some(ContentFields(membershipAccess = Some(MembershipTier.MembersOnly))),
      elements = None
    )

    Content(membershipArticle).requiresMembershipAccess should be(true)

    val noAccess = article.copy(fields = None)
    Content(noAccess).requiresMembershipAccess should be(false)

    val outsideMembership = article.copy(fields = Some(ContentFields(membershipAccess = Some(MembershipTier.MembersOnly))))
    Content(outsideMembership).requiresMembershipAccess should be(false)

  }

  private def tag(id: String = "/id", tagType: String = "keyword", name: String = "", url: String = "") = {
    ApiTag(id = id, `type` = TagType.valueOf(tagType).get, webTitle = name,
      sectionId = None, sectionName = None, webUrl = url, apiUrl = "apiurl", references = Nil)
  }

  private def content(contentType: String, elements: List[ApiElement]): Content = {
    Content(
      ApiContent(
        id = "/content",
        sectionId = None,
        sectionName = None,
        webPublicationDate = Some(DateTime.now().toCapiDateTime),
        webTitle = "webTitle",
        webUrl = "webUrl",
        apiUrl = "apiUrl",
        tags = List(tag(s"type/$contentType")),
        elements = Some(elements)
      )
    )
  }

  private val article: ApiContent =
    content("article", Nil).delegate

  private def image(  id: String,
                      relation: String,
                      caption: String,
                      width: Int,
                      index: Int): ApiElement = {
    ApiElement(id, relation, ElementType.Image, Some(index), List(asset(caption, width)))
  }

  private def asset(caption: String, width: Int): Asset = {
    Asset(AssetType.Image, Some("image/jpeg"), Some("http://www.foo.com/bar"), Some(AssetFields(caption = Some(caption), width = Some(width))))
  }
}
