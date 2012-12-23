/**
 * la-scala static http server
 * 참고: http://twitter.github.com/scala_school/concurrency.html
 */
package daewon.http

import java.net._
import java.util.concurrent._
import java.util.Date
import scala.io._
import java.io._

/**
 * 서버 메인
 * Excutors 스레드 풀 사용
 */ 
class HttpServer(docRoot: String, port: Int) {
  val serverSocket = new ServerSocket(port) 
  val pool: ExecutorService = Executors.newCachedThreadPool 
	
  def run() {
    try {
      while (true) {
        val socket = serverSocket.accept() // block here
        pool.execute(new Handler(docRoot, socket))
      }
    } finally {
      pool.shutdown()
    }
  }
}

/**
 * Http 상수 모음
 */
object HttpConst {
	private val htmlExt = Set("html", "htm")
  val SP = " "
  val CRLF = "\r\n"
  val BODYDELIMITER = CRLF + CRLF

	object Method {
		val GET = "GET"
		val POST = "POST"
		val DELETE = "DELETE"
		val PUT = "PUT"
	}

	def isHTMLPage(path: String) = htmlExt(path.toLowerCase().split("\\.").last)
}

/**
 * 응답 핸들러
 * 200 OK, 404 NotFound 에 대한 처리를 담당
 */
class Handler(docRoot: String, socket: Socket) extends Runnable {
	val iterator = io.Source.fromInputStream(socket.getInputStream)(Codec.UTF8).getLines()

  def run() {
		try {
			if (iterator.isEmpty){
				log("cannot read input stream"); return
			}
			// 01. input stream으로부터 첫줄 읽어내서 메소드, 경로 등 파싱
			val Array(method, path, version) = iterator.next().split(HttpConst.SP)

			log(method, path, version)

			val file = new File(docRoot + path)
			val os = socket.getOutputStream

			method match {
				case HttpConst.Method.GET => {
					if (!file.exists()) NotFound(os)
					else {
						if (HttpConst.isHTMLPage(path)) OK(file, os)
						else OKBinary(file, os)
					}
				}
				case _ => throw new Error("unsupported method ")
			}
		} finally {
			socket.close
		}
	}
}

/*
 * 응답 모음
 * 응답 헤더 모음 쪽으로 리팩터링 필요
 */ 
abstract class Response {
	val BUF_SIZE = 1024 * 1024

	/**
	 * 날자는 매 응답마다 변경되어야 한다.
	 */
	def date = "Date: " + new Date().toString + HttpConst.CRLF

	/**
	 * 파일을 메모리에 들지 않기 위해 사용
	 */
	def writeFileAsStream(file: File, os: OutputStream){
		val fs = new FileInputStream(file)	
		val buf: Array[Byte] = new Array[Byte](BUF_SIZE)

		try {
			Stream.continually(fs.read(buf, 0, BUF_SIZE)).takeWhile(_ != -1).foreach( cnt => os.write(buf, 0, cnt))
		} catch{
			case e: IOException => log("can't write outputstream")
		} finally{
			fs.close
		}
	}
}

/**
 * 404 에러 반환
 */ 
case class NotFound(os: OutputStream) extends Response {
val head = """HTTP/1.1 404 Not Found
Connection: Close""" + HttpConst.BODYDELIMITER

	log(head)

	os.write( (head + date) getBytes )
}

/**
 * 200 OK with HTML 페이지
 */
case class OK(file: File, os: OutputStream) extends Response {
val head = """HTTP/1.1 200 OK
Connection: Close
Content-Type: text/html; charset=utf-8
""" + "Content-Length: " + file.length + HttpConst.BODYDELIMITER 

	log(head)

	os.write(head getBytes)
	this.writeFileAsStream(file, os)
}

/**
 * 200 OK 바이너리 (파일 다운로드)
 */ 
case class OKBinary(file: File, os: OutputStream) extends Response {
val head = """HTTP/1.1 200 OK
Connection: Close
""" +  "Content-type : application/octet-stream; " + 	file.getName + HttpConst.CRLF + "Content-Length: " + file.length + HttpConst.CRLF + "Content-Disposition: attachment; " + file.getName + HttpConst.BODYDELIMITER

	log(head)

	os.write(head getBytes)
	this.writeFileAsStream(file, os)
}

/**
 * 추후 로그 변경을 위해서
 */
case class log(msg: String*) {
	println( msg.mkString(", ") )
}

/**
 * Main
 */ 
object Main extends App {
	val docRoot = "."
	(new HttpServer(docRoot, 8080)).run	
}
