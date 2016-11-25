scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.seleniumhq.webdriver" % "webdriver-htmlunit" % "0.9.7376"
  )

initialCommands in console := """
import com.github.haretaro.aozoradler._
import scala.collection.JavaConversions._
import org.openqa.selenium.htmlunit.HtmlUnitDriver
import Main._
"""
