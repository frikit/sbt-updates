package com.timushev.sbt.updates

import java.io.{ByteArrayInputStream, FileNotFoundException, InputStream}
import java.net.{ConnectException, HttpURLConnection, InetSocketAddress, ServerSocket, SocketTimeoutException, URI, URL}
import java.net.{URLConnection, URLStreamHandler}
import java.nio.charset.StandardCharsets.UTF_8

import com.sun.net.httpserver.{HttpExchange, HttpServer}
import com.timushev.sbt.updates.authentication.RepositoryAuthentication
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import sbt.{ConsoleLogger, IO}

import scala.annotation.nowarn

class DownloaderSpec extends AnyFreeSpec with Matchers {
  // Plugging in a handler needs this constructor on Java 8; its replacement, URL.of, only exists since Java 20.
  @nowarn("cat=deprecation")
  private def url(spec: String)(open: URL => URLConnection): URL =
    new URL(
      null,
      spec,
      new URLStreamHandler {
        override def openConnection(url: URL): URLConnection = open(url)
      }
    )

  private def http(
      url: URL,
      status: Int,
      onConnect: () => Unit = () => (),
      onResponse: () => Unit = () => (),
      headers: Map[String, String] = Map.empty,
      body: () => InputStream = () => new ByteArrayInputStream(Array[Byte](42))
  ): HttpURLConnection =
    new HttpURLConnection(url) {
      override def connect(): Unit                      = onConnect()
      override def disconnect(): Unit                   = ()
      override def usingProxy(): Boolean                = false
      override def getResponseCode(): Int               = { onResponse(); status }
      override def getHeaderField(name: String): String = headers.getOrElse(name, null)
      override def getInputStream()                     = body()
    }

  private def downloader(
      authentications: Seq[RepositoryAuthentication] = Nil,
      session: LookupSession = new LookupSession
  ) =
    new Downloader("repository", authentications, ConsoleLogger(), session)

  private def requestProperties(authentication: RepositoryAuthentication): URLConnection = {
    var connection: URLConnection = null
    val target                    = url("https://repo.example/metadata") { u =>
      connection = new URLConnection(u) {
        override def connect(): Unit = ()
        override def getInputStream  = new ByteArrayInputStream(Array[Byte](42))
      }
      connection
    }
    val stream = downloader(Seq(authentication)).startDownload(target)
    try stream.read() shouldBe 42
    finally stream.close()
    connection
  }

  "Downloader" - {
    "identify the plugin while preserving repository headers and authentication" in {
      val connection = requestProperties(
        RepositoryAuthentication(
          Some("repository"),
          None,
          None,
          "user",
          "password",
          Seq("X-Repository-Token" -> "token")
        )
      )
      connection.getRequestProperty("User-Agent") shouldBe "sbt-updates"
      connection.getRequestProperty("Accept") shouldBe "*/*"
      connection.getRequestProperty("Authorization") shouldBe "Basic dXNlcjpwYXNzd29yZA=="
      connection.getRequestProperty("X-Repository-Token") shouldBe "token"
    }
    "encode credentials as UTF-8" in {
      val connection = requestProperties(RepositoryAuthentication(Some("repository"), None, None, "ü", "p", Nil))
      connection.getRequestProperty("Authorization") shouldBe "Basic w7w6cA=="
    }
    "report a missing document as not found" in {
      a[FileNotFoundException] should be thrownBy downloader().startDownload(
        url("https://repo.example/a")(http(_, 404))
      )
    }
    "report other HTTP errors with their status" in {
      for (status <- Seq(401, 410, 503)) {
        val error =
          the[HttpStatusException] thrownBy downloader().startDownload(url("https://repo.example/a")(http(_, status)))
        error.status shouldBe status
      }
    }
    "stop connecting to a host once it could not be reached" in {
      var attempts              = 0
      def refused(path: String) =
        url(s"https://repo.example/$path") { u =>
          http(u, 200, () => { attempts += 1; throw new ConnectException("Connection refused") })
        }
      val session = new LookupSession
      a[RepositoryUnreachableException] should be thrownBy downloader(session = session).startDownload(refused("a"))
      a[RepositoryUnreachableException] should be thrownBy downloader(session = session).startDownload(refused("b"))
      attempts shouldBe 1
      a[RepositoryUnreachableException] should be thrownBy downloader().startDownload(refused("b"))
      attempts shouldBe 2
    }
    "stop asking a host that doesn't answer" in {
      var attempts             = 0
      def silent(path: String) =
        url(s"https://repo.example/$path") { u =>
          http(u, 200, onResponse = () => { attempts += 1; throw new SocketTimeoutException("Read timed out") })
        }
      val session = new LookupSession
      a[RepositoryUnreachableException] should be thrownBy downloader(session = session).startDownload(silent("a"))
      a[RepositoryUnreachableException] should be thrownBy downloader(session = session).startDownload(silent("b"))
      attempts shouldBe 1
    }
    "stop asking a host whose responses stall after their headers" in {
      var attempts               = 0
      def stalling(path: String) =
        url(s"https://repo.example/$path") { u =>
          http(
            u,
            200,
            onConnect = () => attempts += 1,
            body =
              () => new InputStream { override def read(): Int = throw new SocketTimeoutException("Read timed out") }
          )
        }
      val session = new LookupSession
      val in      = downloader(session = session).startDownload(stalling("a"))
      val error   = the[RepositoryUnreachableException] thrownBy in.read()
      error.getMessage shouldBe "could not reach https://repo.example (Read timed out)"
      a[RepositoryUnreachableException] should be thrownBy downloader(session = session).startDownload(stalling("b"))
      attempts shouldBe 1
    }
    "not repeat a Location header it can't parse, as it may carry a token" in {
      val error = the[HttpStatusException] thrownBy downloader().startDownload(
        url("https://repo.example/a")(http(_, 302, headers = Map("Location" -> "/download?token=s3cret|")))
      )
      error.getMessage shouldBe "HTTP 302 for https://repo.example/a"
    }
    "keep credentials embedded in repository URLs out of failures" in {
      val secret = "https://user:s3cret@repo.example/a"
      val status = the[HttpStatusException] thrownBy downloader().startDownload(url(secret)(http(_, 401)))
      status.getMessage shouldBe "HTTP 401 for https://repo.example/a"
      val unreachable = the[RepositoryUnreachableException] thrownBy downloader().startDownload(
        url(secret)(http(_, 200, () => throw new ConnectException("Connection refused")))
      )
      unreachable.getMessage shouldBe "could not reach https://repo.example (Connection refused)"
      val missing = the[FileNotFoundException] thrownBy downloader().startDownload(url(secret)(http(_, 404)))
      (missing.getMessage should not).include("s3cret")
    }
    "follow redirects but send credentials only to the requested origin" in {
      @volatile var seenByOtherHost                        = Set.empty[String]
      @volatile var seenBySameOrigin                       = Set.empty[String]
      def headerNames(exchange: HttpExchange): Set[String] =
        exchange.getRequestHeaders.keySet().toArray(Array.empty[String]).map(_.toLowerCase).toSet
      def ok(exchange: HttpExchange): Unit = {
        exchange.sendResponseHeaders(200, 2)
        exchange.getResponseBody.write("ok".getBytes(UTF_8))
        exchange.close()
      }
      def redirect(exchange: HttpExchange, location: String): Unit = {
        exchange.getResponseHeaders.add("Location", location)
        exchange.sendResponseHeaders(302, -1)
        exchange.close()
      }
      val other  = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
      val origin = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
      other.createContext("/", exchange => { seenByOtherHost = headerNames(exchange); ok(exchange) })
      origin.createContext("/elsewhere", redirect(_, s"http://127.0.0.1:${other.getAddress.getPort}/moved"))
      origin.createContext("/here", redirect(_, "/here/moved"))
      origin.createContext("/here/moved", exchange => { seenBySameOrigin = headerNames(exchange); ok(exchange) })
      other.start()
      origin.start()
      try {
        val base = s"http://127.0.0.1:${origin.getAddress.getPort}"
        val auth = RepositoryAuthentication(Some("repository"), None, None, "user", "secret", Seq("X-Api-Key" -> "key"))
        def download(path: String): Unit = {
          val in = downloader(Seq(auth)).startDownload(new URI(base + path).toURL)
          try IO.readStream(in) shouldBe "ok"
          finally in.close()
        }
        download("/here")
        (seenBySameOrigin should contain).allOf("authorization", "x-api-key")
        download("/elsewhere")
        seenByOtherHost should contain noneOf ("authorization", "x-api-key")
      } finally {
        origin.stop(0)
        other.stop(0)
      }
    }
    "keep sending credentials when a redirect only changes the spelling of the host name" in {
      @volatile var seen = Set.empty[String]
      val server         = HttpServer.create(new InetSocketAddress("localhost", 0), 0)
      server.createContext(
        "/moved",
        exchange => {
          seen = exchange.getRequestHeaders.keySet().toArray(Array.empty[String]).map(_.toLowerCase).toSet
          exchange.sendResponseHeaders(200, 2)
          exchange.getResponseBody.write("ok".getBytes(UTF_8))
          exchange.close()
        }
      )
      server.createContext(
        "/a",
        exchange => {
          exchange.getResponseHeaders.add("Location", s"http://localhost:${server.getAddress.getPort}/moved")
          exchange.sendResponseHeaders(302, -1)
          exchange.close()
        }
      )
      server.start()
      try {
        val auth = RepositoryAuthentication(Some("repository"), None, None, "user", "secret", Nil)
        val in = downloader(Seq(auth)).startDownload(new URI(s"http://LocalHost:${server.getAddress.getPort}/a").toURL)
        try IO.readStream(in) shouldBe "ok"
        finally in.close()
        seen should contain("authorization")
      } finally server.stop(0)
    }
    "stop at redirects that leave http(s) and name the host that could not be reached" in {
      val closed   = new ServerSocket(0)
      val deadPort = closed.getLocalPort
      closed.close()
      def redirect(exchange: HttpExchange, location: String): Unit = {
        exchange.getResponseHeaders.add("Location", location)
        exchange.sendResponseHeaders(302, -1)
        exchange.close()
      }
      val origin = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
      origin.createContext("/file", redirect(_, "file:///etc/hosts"))
      origin.createContext("/dead", redirect(_, s"http://127.0.0.1:$deadPort/moved"))
      origin.start()
      try {
        val base = s"http://127.0.0.1:${origin.getAddress.getPort}"
        val file = the[HttpStatusException] thrownBy downloader().startDownload(new URI(s"$base/file").toURL)
        file.getMessage shouldBe s"HTTP 302 for $base/file"
        val dead = the[RepositoryUnreachableException] thrownBy downloader().startDownload(new URI(s"$base/dead").toURL)
        dead.getMessage should startWith(s"could not reach http://127.0.0.1:$deadPort ")
      } finally origin.stop(0)
    }
    "keep asking a repository after one of its redirects led to a host that could not be reached" in {
      val closed   = new ServerSocket(0)
      val deadPort = closed.getLocalPort
      closed.close()
      val origin = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
      origin.createContext(
        "/moved",
        exchange => {
          exchange.getResponseHeaders.add("Location", s"http://127.0.0.1:$deadPort/moved")
          exchange.sendResponseHeaders(302, -1)
          exchange.close()
        }
      )
      origin.createContext(
        "/here",
        exchange => {
          exchange.sendResponseHeaders(200, 2)
          exchange.getResponseBody.write("ok".getBytes(UTF_8))
          exchange.close()
        }
      )
      origin.start()
      try {
        val base    = s"http://127.0.0.1:${origin.getAddress.getPort}"
        val session = new LookupSession
        a[RepositoryUnreachableException] should be thrownBy
          downloader(session = session).startDownload(new URI(s"$base/moved").toURL)
        val in = downloader(session = session).startDownload(new URI(s"$base/here").toURL)
        try IO.readStream(in) shouldBe "ok"
        finally in.close()
      } finally origin.stop(0)
    }
  }
}
