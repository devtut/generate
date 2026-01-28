---
metaTitle: ".NET Framework - System.Net.Mail"
description: "MailMessage, Mail with Attachment"
---

# System.Net.Mail



## MailMessage


Here is the example of creating of mail message with attachments. After creating we send this message with the help of `SmtpClient` class. Default 25 port is used here.

```dotnet
public class clsMail
{
    private static bool SendMail(string mailfrom, List<string>replytos, List<string> mailtos, List<string> mailccs, List<string> mailbccs, string body, string subject, List<string> Attachment)
    {
        try
        {
            using(MailMessage MyMail = new MailMessage())
            {
                MyMail.From = new MailAddress(mailfrom);
                foreach (string mailto in mailtos)
                    MyMail.To.Add(mailto);

                if (replytos != null && replytos.Any())
                {
                    foreach (string replyto in replytos)
                        MyMail.ReplyToList.Add(replyto);
                }

                if (mailccs != null && mailccs.Any())
                {
                    foreach (string mailcc in mailccs)
                        MyMail.CC.Add(mailcc);
                }

                if (mailbccs != null && mailbccs.Any())
                {
                    foreach (string mailbcc in mailbccs)
                        MyMail.Bcc.Add(mailbcc);
                }                         

                MyMail.Subject = subject;
                MyMail.IsBodyHtml = true;
                MyMail.Body = body;
                MyMail.Priority = MailPriority.Normal;

                if (Attachment != null && Attachment.Any())
                {
                    System.Net.Mail.Attachment attachment;
                    foreach (var item in Attachment)
                    {
                        attachment = new System.Net.Mail.Attachment(item);
                        MyMail.Attachments.Add(attachment);
                    }
                }

                SmtpClient smtpMailObj = new SmtpClient();
                smtpMailObj.Host = "your host";
                smtpMailObj.Port = 25;
                smtpMailObj.Credentials = new System.Net.NetworkCredential("uid", "pwd");

                smtpMailObj.Send(MyMail);
                return true;
            }
        }
        catch
        {
            return false;
        }
    }
}

```



## Mail with Attachment


`MailMessage` represents mail message which can be sent further using `SmtpClient` class. Several attachments (files) can be added to mail message.

```dotnet
using System.Net.Mail;

using(MailMessage myMail = new MailMessage())
{
     Attachment attachment = new Attachment(path);
     myMail.Attachments.Add(attachment);

     // further processing to send the mail message

}

```



#### Remarks


It is important to Dispose a System.Net.MailMessage because every single attachment contains a Stream and these Streams need to be freed as soon as possible. The using statement ensures that the Disposable object is Disposed also in case of Exceptions

