---
metaTitle: "ASP.NET Identity"
description: "How to implement password reset token in asp.net identity using user manager."
---

# ASP.NET Identity


Tutorials concerning asp.net Identity such as user management, role management, creating tokens and more.



## How to implement password reset token in asp.net identity using user manager.


<li>
Create a new folder called MyClasses and create and add the following class

```cs
public class GmailEmailService:SmtpClient
{
    // Gmail user-name
    public string UserName { get; set; }

    public GmailEmailService() :
        base(ConfigurationManager.AppSettings["GmailHost"], Int32.Parse(ConfigurationManager.AppSettings["GmailPort"]))
    {
        //Get values from web.config file:
        this.UserName = ConfigurationManager.AppSettings["GmailUserName"];
        this.EnableSsl = Boolean.Parse(ConfigurationManager.AppSettings["GmailSsl"]);
        this.UseDefaultCredentials = false;
        this.Credentials = new System.Net.NetworkCredential(this.UserName, ConfigurationManager.AppSettings["GmailPassword"]);
    }
}

```

</li>
<li>
Configure your Identity Class

```cs
public async Task SendAsync(IdentityMessage message)
{
    MailMessage email = new MailMessage(new MailAddress("youremailadress@domain.com", "(any subject here)"),
    new MailAddress(message.Destination));
    email.Subject = message.Subject;
    email.Body = message.Body;

    email.IsBodyHtml = true;

    GmailEmailService mailClient = new GmailEmailService();
    await mailClient.SendMailAsync(email);
}

```

</li>
<li>
Add your credentials to the web.config. I did not use gmail in this portion because the use of gmail is blocked in my workplace and it still works perfectly.

    
    
    
    

```cs
<add key="GmailUserName" value="youremail@yourdomain.com"/>
<add key="GmailPassword" value="yourPassword"/>
<add key="GmailHost" value="yourServer"/>
<add key="GmailPort" value="yourPort"/>
<add key="GmailSsl" value="chooseTrueOrFalse"/>
<!--Smptp Server (confirmations emails)-->

```



</li>
<li>
Make necessary changes to your Account Controller. Add the following highlighted code.
</li>

[<img src="https://i.stack.imgur.com/mJz6k.jpg" alt="First do this" />](https://i.stack.imgur.com/mJz6k.jpg)

[<img src="https://i.stack.imgur.com/S8jvL.jpg" alt="Then This" />](https://i.stack.imgur.com/S8jvL.jpg)

Compile then run. Cheers!

