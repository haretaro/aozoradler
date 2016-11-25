package com.github.haretaro.aozoradler

import java.io.File
import java.net.URL
import org.openqa.selenium.htmlunit.HtmlUnitDriver
import org.openqa.selenium.NoSuchElementException
import scala.collection.JavaConversions._
import scala.sys.process._
import java.util.zip.ZipFile
import java.io.FileInputStream
import java.io.FileOutputStream
import java.util.zip.ZipEntry
import java.io.InputStream
import java.io.OutputStream
import org.apache.commons.io.FileUtils

object Main extends App{
  def url = "http://www.aozora.gr.jp/index_pages/person148.html" //夏目漱石の作者ページ
  val zipFolder = "output/zip"
  val txtFolder = "output/txt"
  val formattedFolder = "output/formatted"

  bookLinksOf(url)
    .flatMap(l => {
      println("downloading %s".format(l))
      download(l, zipFolder)
    })
    .foreach(f => {
      println("unziping %s".format(f))
      unzip(f, txtFolder)
    })

  val resultFolder = new File(formattedFolder)
  resultFolder.mkdirs
  (new File(txtFolder)).listFiles.foreach( f => {
    println("processing %s".format(f))
    formatTextFile(f, new File(resultFolder, f.getName))
  })

  /**
   * @param author 作者ページのurl
   * @return 青空文庫図書カードのurl
   */
  def bookLinksOf(author: String): List[String] = {
    val driver = new HtmlUnitDriver()
    driver.get(author)
    val links = driver.findElementsByXPath("//li/a")
    links.map(_.getAttribute("href"))
      .map(_.replaceAll("\\.\\.","http://www.aozora.gr.jp"))
      .toList
  }

  /**
   * 青空文庫のzipファイルをダウンロードする
   * @param url: 青空文庫図書カードのurl
   */
  def download(url: String, dst: String): Option[File] = {
    try{
      val driver = new HtmlUnitDriver()
      driver.get(url)
      val link = driver.findElementByXPath("//a[contains(.,'zip')]")
        .getAttribute("href")
      val relativeFilePath = "files.+".r.findFirstIn(link)
      val zipurl = "http://www.aozora.+/cards/.+/".r.findFirstIn(url)
        .flatMap( base => relativeFilePath.map(base + _))
      val filename = relativeFilePath.map(path => path.slice(6,1000))
      (zipurl, filename) match{
        case (Some(url), Some(path)) =>{
          val base = new File(dst)
          base.mkdirs
          val file = new File(base, path)
          new URL(url) #> file !!;
          Option(file)
        }
        case _ => None
      }
    } catch {
      case e: NoSuchElementException => None
    }
  }

  /**
   * zipファイルを解凍する
   * @param src: 解凍するファイル名
   * @param dist: 解凍先フォルダ名
   */
  def unzip(src: File, dist: String): Unit = unzip(src.getPath, dist)
  def unzip(src: String, dist: String): Unit = {
    val BUFSIZE=4096
    val buffer = new Array[Byte](BUFSIZE)
    val zipFile = new ZipFile(src)
    val target = new File(dist)
    target.mkdirs
    unzipAll(zipFile.entries.toList, getZipEntryInputStream(zipFile)_, target)

    def getZipEntryInputStream(zipFile: ZipFile)(entry: ZipEntry) = zipFile.getInputStream(entry)

    def unzipAll(entryList: List[ZipEntry], inputGetter: ZipEntry => InputStream, targetFolder: File): Boolean = {
      entryList match {
        case entry :: entries =>
          if (entry.isDirectory)
            new File(targetFolder, entry.getName).mkdirs
          else
            saveFile(inputGetter(entry), new FileOutputStream(new File(targetFolder, entry.getName)))
          unzipAll(entries, inputGetter, targetFolder)

        case _ => true
      }
    }

    def saveFile(fis: InputStream, fos: OutputStream) = {
      writeToFile(bufferReader(fis)_, fos)
      fis.close
      fos.close
    }

    def bufferReader(fis: InputStream)(buffer: Array[Byte]) = (fis.read(buffer), buffer)

    def writeToFile(reader: (Array[Byte]) => Tuple2[Int, Array[Byte]], fos: OutputStream): Boolean = {
      val (length, data) = reader(buffer)
      if (length >= 0) {
        fos.write(data, 0, length)
        writeToFile(reader, fos)
      } else
        true
    }
  }

  def formatText(src: String): String =
    postProcess( preProcess( src))

  /**
   * 不要な文字列を取り除く
   */
  def preProcess(src: String): String =
    src.replaceAll("《.+?》", "")//ルビ
      .replaceAll("(?s)^.+-{20,}", "")//タイトル注意書き
      .replaceAll("(?s)底本.+$", "") //底本から文書の最後まで
      .replaceAll("［＃.+］", "")//章見出し, 脚注
      .replaceAll("（.*?）", "")//かっこ
      .replaceAll("[　—…]", "")//ゴミ取り
      .replaceAll("[\r\n]", "")//改行

  /**
   * 文書に区切りを入れる
   * 分かち書きをする
   */
  def postProcess(src: String): String = {
    val str = src.replaceAll("。」", "」\n")
      .replaceAll("。", "\n")
    str.split("\n").map( s =>
      "echo %s".format(s) #| "mecab -Owakati" !!
    ).foldLeft("")(_+_)
  }

  /**
   * テキストファイルを開いて編集して保存する
   */
  def formatTextFile(src: File, dst: File): Unit = {
    val str = FileUtils.readFileToString(src, "sjis")
    val str1 = formatText(str)
    FileUtils.writeStringToFile(dst, str1, "utf-8")
  }
  def fromatTextFile(src: File, dst: String): Unit = formatTextFile(src, new File(dst))
  def formatTextFile(src: String, dst: String): Unit = formatTextFile(new File(src), new File(dst))
}
