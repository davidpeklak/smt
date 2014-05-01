package smt.report

import scalaz.{\/-, \/}
import smt.NamedMoveStates

import java.util._
import javax.mail._
import javax.mail.internet._
import javax.activation._

class MailReporter(
                    mailHost: String, // chgvasmtphub
                    port: Int, // 25
                    from: String,
                    to: String,
                    subject: String,
                    textHeader: String
                    ) extends Reporter {


  def report(nmss: NamedMoveStates): String \/ Unit = \/- {

    val properties = System.getProperties()

    properties.setProperty("mail.host", mailHost)
    properties.setProperty("mail.smtp.host", mailHost)
    properties.setProperty("mail.smtp.port", port.toString)


    val session = Session.getInstance(properties)


    try {
      // Create a default MimeMessage object.
      val message = new MimeMessage(session)

      // Set From: header field of the header.
      message.setFrom(new InternetAddress(from))

      // Set To: header field of the header.
      message.addRecipient(Message.RecipientType.TO,
        new InternetAddress(to))

      // Set Subject: header field
      message.setSubject(subject)

      val descrs = for {
        (name, nms) <- nmss.actions
      } yield MoveStateDescription.describe(name, nms)

      val txt = (textHeader +: descrs).mkString("\n")

      // Now set the actual message
      message.setText(txt)

      // Send message
      Transport.send(message)
      println("Sent message successfully....")
    }
    catch {
      case e: Exception => println(e.toString)
    }
  }
}