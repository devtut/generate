---
metaTitle: "PowerShell - Sending Email"
description: "Send-MailMessage with predefined parameters, Simple Send-MailMessage, SMTPClient - Mail with .txt file in body message"
---

# Sending Email


A useful technique for Exchange Server administrators is to be able to send email messages via SMTP from PowerShell. Depending on the version of PowerShell installed on your computer or server, there are multiple ways to send emails via powershell. There is a native cmdlet option that is simple and easy to use. It uses the cmdlet ****Send-MailMessage****.



## Send-MailMessage with predefined parameters


```powershell
$parameters = @{
    From = 'from@bar.com'
    To = 'to@bar.com'
    Subject = 'Email Subject'
    Attachments =  @('C:\files\samplefile1.txt','C:\files\samplefile2.txt')
    BCC = 'bcc@bar.com'
    Body = 'Email body'
    BodyAsHTML = $False
    CC = 'cc@bar.com'
    Credential = Get-Credential
    DeliveryNotificationOption = 'onSuccess'
    Encoding = 'UTF8'
    Port = '25'
    Priority = 'High'
    SmtpServer = 'smtp.com'
    UseSSL = $True
}

# Notice: Splatting requires @ instead of $ in front of variable name
Send-MailMessage @parameters

```



## Simple Send-MailMessage


```powershell
Send-MailMessage -From sender@bar.com -Subject "Email Subject" -To receiver@bar.com -SmtpServer smtp.com

```



## SMTPClient - Mail with .txt file in body message


```powershell
# Define the txt which will be in the email body
$Txt_File = "c:\file.txt"

function Send_mail {
    #Define Email settings
    $EmailFrom = "source@domain.com"
    $EmailTo = "destination@domain.com"
    $Txt_Body = Get-Content $Txt_File -RAW
    $Body = $Body_Custom + $Txt_Body
    $Subject = "Email Subject"
    $SMTPServer = "smtpserver.domain.com"
    $SMTPClient = New-Object Net.Mail.SmtpClient($SmtpServer, 25) 
    $SMTPClient.EnableSsl = $false
    $SMTPClient.Send($EmailFrom, $EmailTo, $Subject, $Body)

}

$Body_Custom = "This is what contain file.txt : "

Send_mail

```



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|Attachments<String[]>|Path and file names of files to be attached to the message. Paths and filenames can be piped to Send-MailMessage.
|Bcc<String[]>|Email addresses that receive a copy of an email message but does not appear as a recipient in the message. Enter names (optional) and the email address (required), such as Name someone@example.com or someone@example.com.
|Body <String_>|Content of the email message.
|BodyAsHtml|It indicates that the content is in HTML format.
|Cc<String[]>|Email addresses that receive a copy of an email message. Enter names (optional) and the email address (required), such as Name someone@example.com or someone@example.com.
|Credential|Specifies a user account that has permission to send message from specified email address. The default is the current user. Enter name such as User or Domain\User, or enter a PSCredential object.
|DeliveryNotificationOption|Specifies the delivery notification options for the email message. Multiple values can be specified. Delivery notifications are sent in message to address specified in To parameter. Acceptable values: None, OnSuccess, OnFailure, Delay, Never.
|Encoding|Encoding for the body and subject. Acceptable values: ASCII, UTF8, UTF7, UTF32, Unicode, BigEndianUnicode, Default, OEM.
|From|Email addresses from which the mail is sent. Enter names (optional) and the email address (require), such as Name someone@example.com or someone@example.com.
|Port|Alternate port on the SMTP server. The default value is 25. Available from Windows PowerShell 3.0.
|Priority|Priority of the email message. Acceptable values: Normal, High, Low.
|SmtpServer|Name of the SMTP server that sends the email message. Default value is the value of the $PSEmailServer variable.
|Subject|Subject of the email message.
|To|Email addresses to which the mail is sent. Enter names (optional) and the email address (required), such as Name someone@example.com or someone@example.com
|UseSsl|Uses the Secure Sockets Layer (SSL) protocol to establish a connection to the remote computer to send mail

