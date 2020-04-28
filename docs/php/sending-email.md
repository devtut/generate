---
metaTitle: "Sending Email"
description: "Sending Email - The basics, more details, and a full example, Sending HTML Email Using mail(), Sending Email With An Attachment Using mail(), Sending Plain Text Email Using PHPMailer, Sending HTML Email Using PHPMailer, Sending Email With An Attachment Using PHPMailer, Sending Plain Text Email Using Sendgrid, Sending Email With An Attachment Using Sendgrid"
---

# Sending Email



## Sending Email - The basics, more details, and a full example


A typical email has three main components:

1. A recipient (represented as an email address)
1. A subject
1. A message body

Sending mail in PHP can be as simple as calling the built-in function `mail()`. `mail()` takes up to five parameters but the first three are all that is required to send an email (although the four parameters is commonly used as will be demonstrated below). The first three parameters are:

1. The recipient's email address (string)
1. The email's subject (string)
1. The body of the email (string) (e.g. the content of the email)

A minimal example would resemble the following code:

```
mail('recipient@example.com', 'Email Subject', 'This is the email message body');

```

The simple example above works well in limited circumstances such as hardcoding an email alert for an internal system. However, it is common to place the data passed as the parameters for `mail()` in variables to make the code cleaner and easier to manage (for example, dynamically building an email from a form submission).

Additionally, `mail()` accepts a fourth parameter which allows you to have additional mail headers sent with your email. These headers can allow you to set:

- the `From` name and email address the user will see
- the `Reply-To` email address the user's response will be sent to
- additional non-standards headers like `X-Mailer` which can tell the recipient this email was sent via PHP

```
$to      = 'recipient@example.com';             // Could also be $to      = $_POST['recipient'];  
$subject = 'Email Subject';                     // Could also be $subject = $_POST['subject'];  
$message = 'This is the email message body';    // Could also be $message = $_POST['message'];  
$headers = implode("\r\n", [
    'From: John Conde <webmaster@example.com>',
    'Reply-To: webmaster@example.com',
    'X-Mailer: PHP/' . PHP_VERSION
]);

```

The optional fifth parameter can be used to pass additional flags as command line options to the program configured to be used when sending mail, as defined by the `sendmail_path` configuration setting. For example, this can be used to set the envelope sender address when using sendmail/postfix with the `-f` sendmail option.

```
$fifth  = '-fno-reply@example.com';

```

Although using `mail()` can be pretty reliable, it is by no means guaranteed that an email will be sent when `mail()` is called. To see if there is a potential error when sending your email, you should capture the return value from `mail()`. `TRUE` will be returned if the mail was successfully accepted for delivery. Otherwise, you will receive `FALSE`.

```
$result = mail($to, $subject, $message, $headers, $fifth);

```

**NOTE**: Although `mail()` may return `TRUE`, it does **not** mean the email was sent or that the email will be received by the recipient. It only indicates the mail was successfully handed over to your system's mail system successfully.

If you wish to send an HTML email, there isn't a lot more work you need to do. You need to:

1. Add the `MIME-Version` header
1. Add the `Content-Type` header
1. Make sure your email content is HTML

```
$to      = 'recipient@example.com';                            
$subject = 'Email Subject';                                     
$message = '<html><body>This is the email message body</body></html>';       
$headers = implode("\r\n", [
    'From: John Conde <webmaster@example.com>',
    'Reply-To: webmaster@example.com',
    'MIME-Version: 1.0',
    'Content-Type: text/html; charset=ISO-8859-1',
    'X-Mailer: PHP/' . PHP_VERSION
]);

```

Here's a full example of using PHP's `mail()` function

```
<?php

// Debugging tools. Only turn these on in your development environment.

error_reporting(-1);
ini_set('display_errors', 'On');
set_error_handler("var_dump");

// Special mail settings that can make mail less likely to be considered spam
// and offers logging in case of technical difficulties.

ini_set("mail.log", "/tmp/mail.log");
ini_set("mail.add_x_header", TRUE);

// The components of our email

$to      = 'recipient@example.com';
$subject = 'Email Subject';
$message = 'This is the email message body';
$headers = implode("\r\n", [
    'From: webmaster@example.com',
    'Reply-To: webmaster@example.com',
    'X-Mailer: PHP/' . PHP_VERSION
]);

// Send the email

$result = mail($to, $subject, $message, $headers);

// Check the results and react accordingly

if ($result) {
  
    // Success! Redirect to a thank you page. Use the
    // POST/REDIRECT/GET pattern to prevent form resubmissions
    // when a user refreshes the page.
  
    header('Location: http://example.com/path/to/thank-you.php', true, 303);
    exit;
  
}
else {
  
    // Your mail was not sent. Check your logs to see if
    // the reason was reported there for you.
  
}

```

See Also

Official documentation

- [`mail()`](http://php.net/manual/en/function.mail.php)
- [PHP `mail()` configuration](http://php.net/manual/en/mail.configuration.php)

Related Stack Overflow Questions

- [PHP mail form doesn't complete sending e-mail](http://stackoverflow.com/q/24644436/250259)
- [How do you make sure email you send programmatically is not automatically marked as spam?](http://stackoverflow.com/q/371/250259)
- [How to use SMTP to send email](http://stackoverflow.com/questions/15965376/how-to-configure-xampp-to-send-mail-from-localhost/18185233#18185233)
- [Setting envelope from address](http://stackoverflow.com/a/5666682/2417031)

Alternative Mailers

- [PHPMailer](https://github.com/Synchro/PHPMailer)
- [SwiftMailer](http://swiftmailer.org/)
- [PEAR::Mail](https://pear.php.net/package/Mail)

Email Servers

- [Mercury Mail (Windows)](http://www.pmail.com/overviews/ovw_mercury.htm)

Related Topics

- [Post/Redirect/Get](https://en.wikipedia.org/wiki/Post/Redirect/Get)



## Sending HTML Email Using mail()


```
<?php
$to      = 'recipent@example.com';
$subject = 'Sending an HTML email using mail() in PHP';
$message = '<html><body><p><b>This paragraph is bold.</b></p><p><i>This text is italic.</i></p></body></html>';

$headers = implode("\r\n", [
    "From: John Conde <webmaster@example.com>",
    "Reply-To: webmaster@example.com",
    "X-Mailer: PHP/" . PHP_VERSION,
    "MIME-Version: 1.0",
    "Content-Type: text/html; charset=UTF-8"
]);

mail($to, $subject, $message, $headers);

```

This is not much different then [sending a plain text email](http://stackoverflow.com/documentation/php/458/sending-email/2059/sending-email-the-basics-more-details-and-a-full-example). Thet key differences being the content body is structured like an HTML document and there are two additional headers that must be included so the email client knows to trender the email as HTML. They are:

- MIME-Version: 1.0
- Content-Type: text/html; charset=UTF-8



## Sending Email With An Attachment Using mail()


```
<?php

$to         = 'recipient@example.com';
$subject    = 'Email Subject';
$message    = 'This is the email message body';

$attachment = '/path/to/your/file.pdf';
$content = file_get_contents($attachment);

/* Attachment content transferred in Base64 encoding
MUST be split into chunks 76 characters in length as
specified by RFC 2045 section 6.8. By default, the
function chunk_split() uses a chunk length of 76 with
a trailing CRLF (\r\n). The 76 character requirement
does not include the carriage return and line feed */
$content = chunk_split(base64_encode($content));

/* Boundaries delimit multipart entities. As stated
in RFC 2046 section 5.1, the boundary MUST NOT occur
in any encapsulated part. Therefore, it should be
unique. As stated in the following section 5.1.1, a
boundary is defined as a line consisting of two hyphens
("--"), a parameter value, optional linear whitespace,
and a terminating CRLF. */
$prefix     = "part_"; // This is an optional prefix
/* Generate a unique boundary parameter value with our
prefix using the uniqid() function. The second parameter
makes the parameter value more unique. */
$boundary   = uniqid($prefix, true);

// headers
$headers    = implode("\r\n", [
    'From: webmaster@example.com',
    'Reply-To: webmaster@example.com',
    'X-Mailer: PHP/' . PHP_VERSION,
    'MIME-Version: 1.0',
    // boundary parameter required, must be enclosed by quotes
    'Content-Type: multipart/mixed; boundary="' . $boundary . '"',
    "Content-Transfer-Encoding: 7bit",
    "This is a MIME encoded message." // message for restricted transports
]);

// message and attachment
$message    = implode("\r\n", [ 
    "--" . $boundary, // header boundary delimiter line
    'Content-Type: text/plain; charset="iso-8859-1"',
    "Content-Transfer-Encoding: 8bit",
    $message,
    "--" . $boundary, // content boundary delimiter line
    'Content-Type: application/octet-stream; name="RenamedFile.pdf"',
    "Content-Transfer-Encoding: base64",
    "Content-Disposition: attachment",
    $content,
    "--" . $boundary . "--" // closing boundary delimiter line
]);

$result = mail($to, $subject, $message, $headers); // send the email

if ($result) {
    // Success! Redirect to a thank you page. Use the
    // POST/REDIRECT/GET pattern to prevent form resubmissions
    // when a user refreshes the page.
  
    header('Location: http://example.com/path/to/thank-you.php', true, 303);
    exit;
}
else {
    // Your mail was not sent. Check your logs to see if
    // the reason was reported there for you.
}

```

### Content-Transfer-Encodings

The available encodings are **7bit**, **8bit**, **binary**, **quoted-printable**, **base64**, **ietf-token**, and **x-token**. Of these encodings, when a header has a **multipart** Content-Type, the Content-Transfer-Encoding **must not** be any other value other than **7bit**, **8bit**, or **binary** as stated in RFC 2045, section 6.4.

Our example chooses the 7bit encoding, which represents US-ASCII characters, for the multipart header because, as noted in RFC 2045 section 6, some protocols support only this encoding. Data within the boundaries can then be encoded on a part-by-part basis (RFC 2046, section 5.1). This example does exactly this. The first part, which contains the text/plain message, is defined to be 8bit since it may be necessary to support additional characters. In this case, the Latin1 (iso-8859-1) character set is being used. The second part is the attachment and so it is defined as a base64-encoded application/octet-stream. Since base64 transforms arbitrary data into the 7bit range, it can be sent over restricted transports (RFC 2045, section 6.2).



## Sending Plain Text Email Using PHPMailer


**Basic Text Email**

```
<?php

$mail = new PHPMailer();

$mail->From     = "from@example.com";
$mail->FromName = "Full Name";
$mail->addReplyTo("reply@example.com", "Reply Address");
$mail->Subject  = "Subject Text";
$mail->Body     = "This is a sample basic text email using PHPMailer.";

if($mail->send()) {
    // Success! Redirect to a thank you page. Use the
    // POST/REDIRECT/GET pattern to prevent form resubmissions
    // when a user refreshes the page.
  
    header('Location: http://example.com/path/to/thank-you.php', true, 303);
    exit;
} 
else {
    echo "Mailer Error: " . $mail->ErrorInfo;
}

```

**Adding addtional recipients, CC recipients, BCC recipients**

```
<?php

$mail = new PHPMailer();

$mail->From     = "from@example.com";
$mail->FromName = "Full Name";
$mail->addReplyTo("reply@example.com", "Reply Address");
$mail->addAddress("recepient1@example.com", "Recepient Name");
$mail->addAddress("recepient2@example.com"); 
$mail->addCC("cc@example.com");
$mail->addBCC("bcc@example.com");
$mail->Subject  = "Subject Text";
$mail->Body     = "This is a sample basic text email using PHPMailer.";

if($mail->send()) {
    // Success! Redirect to a thank you page. Use the
    // POST/REDIRECT/GET pattern to prevent form resubmissions
    // when a user refreshes the page.
  
    header('Location: http://example.com/path/to/thank-you.php', true, 303);
    exit;
} 
else {
    echo "Error: " . $mail->ErrorInfo;
}   

```



## Sending HTML Email Using PHPMailer


```
<?php

$mail = new PHPMailer();

$mail->From     = "from@example.com";
$mail->FromName = "Full Name";
$mail->addReplyTo("reply@example.com", "Reply Address");
$mail->addAddress("recepient1@example.com", "Recepient Name");
$mail->addAddress("recepient2@example.com"); 
$mail->addCC("cc@example.com");
$mail->addBCC("bcc@example.com");
$mail->Subject  = "Subject Text";
$mail->isHTML(true);
$mail->Body     = "<html><body><p><b>This paragraph is bold.</b></p><p><i>This text is italic.</i></p></body></html>";
$mail->AltBody = "This paragraph is not bold.\n\nThis text is not italic.";

if($mail->send()) {
    // Success! Redirect to a thank you page. Use the
    // POST/REDIRECT/GET pattern to prevent form resubmissions
    // when a user refreshes the page.
  
    header('Location: http://example.com/path/to/thank-you.php', true, 303);
    exit;
} 
else {
    echo "Error: " . $mail->ErrorInfo;
}   

```



## Sending Email With An Attachment Using PHPMailer


```
<?php

$mail = new PHPMailer();

$mail->From     = "from@example.com";
$mail->FromName = "Full Name";
$mail->addReplyTo("reply@example.com", "Reply Address");
$mail->Subject  = "Subject Text";
$mail->Body     = "This is a sample basic text email with an attachment using PHPMailer.";

// Add Static Attachment
$attachment = '/path/to/your/file.pdf';
$mail->AddAttachment($attachment , 'RenamedFile.pdf');

// Add Second Attachment, run-time created. ie: CSV to be open with Excel
$csvHeader = "header1,header2,header3";
$csvData = "row1col1,row1col2,row1col3\nrow2col1,row2col2,row2col3";

$mail->AddStringAttachment($csvHeader ."\n" . $csvData, 'your-csv-file.csv', 'base64', 'application/vnd.ms-excel');

if($mail->send()) {
    // Success! Redirect to a thank you page. Use the
    // POST/REDIRECT/GET pattern to prevent form resubmissions
    // when a user refreshes the page.
  
    header('Location: http://example.com/path/to/thank-you.php', true, 303);
    exit;
} 
else {
    echo "Error: " . $mail->ErrorInfo;
}   

```



## Sending Plain Text Email Using Sendgrid


**Basic Text Email**

```
<?php

$sendgrid = new SendGrid("YOUR_SENDGRID_API_KEY");
$email    = new SendGrid\Email();

$email->addTo("recipient@example.com")
      ->setFrom("sender@example.com")
      ->setSubject("Subject Text")
      ->setText("This is a sample basic text email using ");

$sendgrid->send($email);

```

**Adding addtional recipients, CC recipients, BCC recipients**

```
<?php

$sendgrid = new SendGrid("YOUR_SENDGRID_API_KEY");
$email    = new SendGrid\Email();

$email->addTo("recipient@example.com")
      ->setFrom("sender@example.com")
      ->setSubject("Subject Text")
      ->setHtml("<html><body><p><b>This paragraph is bold.</b></p><p><i>This text is italic.</i></p></body></html>");
      
$personalization = new Personalization();
$email = new Email("Recepient Name", "recepient1@example.com");
$personalization->addTo($email);
$email = new Email("RecepientCC Name", "recepient2@example.com");
$personalization->addCc($email);
$email = new Email("RecepientBCC Name", "recepient3@example.com");
$personalization->addBcc($email);
$email->addPersonalization($personalization);
    
$sendgrid->send($email);

```



## Sending Email With An Attachment Using Sendgrid


```
<?php

$sendgrid = new SendGrid("YOUR_SENDGRID_API_KEY");
$email    = new SendGrid\Email();

$email->addTo("recipient@example.com")
      ->setFrom("sender@example.com")
      ->setSubject("Subject Text")
      ->setText("This is a sample basic text email using ");
      
$attachment = '/path/to/your/file.pdf';
$content    = file_get_contents($attachment);
$content    = chunk_split(base64_encode($content));

$attachment = new Attachment();
$attachment->setContent($content);
$attachment->setType("application/pdf");
$attachment->setFilename("RenamedFile.pdf");
$attachment->setDisposition("attachment");
$email->addAttachment($attachment);

$sendgrid->send($email);

```



#### Parameters


|Parameter|Details
|------
|`string $to`|The recipient email address
|`string $subject`|The subject line
|`string $message`|The body of the email
|`string $additional_headers`|Optional: headers to add to the email
|`string $additional_parameters`|Optional: arguments to pass to the configured mail send application in the command line



#### Remarks


**E-Mail I'm sending through my script never arrives. What should I do?**

<li>
Make sure you have error reporting turned on to see any errors.
</li>
<li>
If you have access to PHP's error log files, check those.
</li>
<li>
Is the `mail()` command [configured properly on your server](http://uk3.php.net/manual/en/mail.configuration.php)? (If you are on shared hosting, you can not change anything here.)
</li>
<li>
If E-Mails are just disappearing, start an E-Mail account with a freemail service that has a spam folder (or use a mail account that does no spam filtering at all). This way, you can see whether the E-Mail is not getting sent out, or perhaps sent out but filtered as spam.
</li>
<li>
Did you check the "from:" address you used for possible "returned to sender" mails? You can also set up a separate [bounce address](http://stackoverflow.com/questions/5303541/set-email-headers-so-bounced-emails-go-to-a-specific-address) for error mails.
</li>

**The E-Mail I'm sending is getting filtered as spam. What should I do?**

<li>
Does the sender address ("From") belong to a domain that runs on the server you send the E-Mail from? If not, change that.
Never use sender addresses like `xxx@gmail.com`. Use `reply-to` if you need replies to arrive at a different address.
</li>
<li>
Is your server on a blacklist? This is a possibility when you're on shared hosting when neighbours behave badly. Most blacklist providers, like [Spamhaus](https://www.spamhaus.org/lookup/), have tools that allow you to look up your server's IP. There's also third party tools like [MX Toolbox.](http://mxtoolbox.com/blacklists.aspx)
</li>
<li>
Some installations of PHP require setting a [fifth parameter](http://stackoverflow.com/questions/1376152/what-does-the-f-flag-mean-in-the-fifth-parameter-in-the-php-mail-function) to mail() to add a sender address. See whether this might be the case for you.
</li>
<li>
If all else fails, consider using email-as-a-service such as [Mailgun](https://www.mailgun.com/), [SparkPost](https://www.sparkpost.com/), [Amazon SES](https://aws.amazon.com/ses/), [Mailjet](https://www.mailjet.com/), [SendinBlue](https://www.sendinblue.com/) or [SendGrid](https://sendgrid.com/)—to name a few—instead. They all have APIs that can be called using PHP.
</li>

