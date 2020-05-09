---
metaTitle: "iOS - iOS - Implementation of XMPP with Robbie Hanson framework"
description: "iOS XMPP Robbie Hanson Example with Openfire"
---

# iOS - Implementation of XMPP with Robbie Hanson framework




## iOS XMPP Robbie Hanson Example with Openfire


### **SRXMPPDemo**

### Download the example and all the classes here - [https://github.com/SahebRoy92/SRXMPPDemo](https://github.com/SahebRoy92/SRXMPPDemo)

A demo on XMPP in Objective C, with various simple and complex features implemented in it. All the features of XMPP is done by **"in band"** xmpp functions.
Few features this project contains are --

**SRXMPP** - A wrapper Singleton class that almost has all features needed for one-to-one chat application.

- one to one chat
- Core data implementation of chat (text message) thus having saving of previous messages, offline messages.
- implementation of vCard(profile information of user, own and others too) from XML and Core Data provided by Robbie Hanson's own framework.
- availability of friends status (online/offline/typing)

### **Steps to follow**

You want to use this project as a reference then you can do the following--

**1. Installed Openfire in a live server**     - Rent a server, install openfire.

**2. Want to try it out without a hassle in your own computer** -
You need to download, install and setup 3 things to start

**a. Java -**

- Download and install Java for Mac.

**b. XAMPP** -

- Install XAMPP is relatively easy.
<li>After installation just start the XAMPP and start **Database(SQL)** and **Apache Server**.
<img src="http://i.imgur.com/mXQmnhh.png?1" alt="image reference" /></li>
<li>Then open browser and paste this URL
[**[http://localhost/phpmyadmin/]**](http://localhost/phpmyadmin/)</li>
- . Create a new DB from the left hand side panel.
- **Name the DB anything but remember this name, suppose we name it ChatDB**

**c. Openfire** -

<li>Install Openfire and run the application and "Start Openfire"
<img src="http://i.imgur.com/Ct8ft15.png?1" alt="image reference" /></li>
- Open Browser and Paste this URL - [[http://localhost:9090/setup/index.jsp](http://localhost:9090/setup/index.jsp)](http://localhost:9090/setup/index.jsp%5D(http://localhost:9090/setup/index.jsp))
<li>Do normal setup
<ul>
- Select Language >
- Server settings, leave as it is, just do continue >
- Database Settings, leave as it is as "Standard Database Connection as selected >
- Database Settings - Standard Connection". Now remember the name of the DB you set was **ChatDB**.
<li>Select Database Driver Presets as ***"<em>MySQL"**</em>. Leave JDBC Driver Class as it is. Now in the Database URL you can see, brackets mentioning hostname and Database Name. Just change Hostname to **"localhost"**, and database name to **"ChatDB"**, or any other name of DB you have set earlier, while seting up XAMPP.
Leave the Username and password as blank.Fill up details like the image here
<img src="http://i.imgur.com/BKRBG3c.png?1" alt="image reference" />.</li>
<li>Next complete setup by giving a username and password and reconfirming it.
Thats it your done Setting up Openfire.</li>

Now the part comes when you have to change a tiny detail in the code.

#**Important**
We need to go to the class - **SRXMPP.m**, locate the NSString extern **SRXMPP_Hostname** (in the top) and overwrite the value of it to the

<li>IP of the server where OpenFire is installed ,
**OR**</li>
<li>if you have installed it locally, overwrite the value to -
**"localhost"**.</li>

Thats it, you are ready to use this example project and start coding and making it into a better project of your own.

This starter pack will help you in understanding XMPP structure better as well as getting a grasp into XMPP protocols.

You can find other XMPP protocols here in this site - [[https://xmpp.org/rfcs/rfc3920.html](https://xmpp.org/rfcs/rfc3920.html)](https://xmpp.org/rfcs/rfc3920.html%5D(https://xmpp.org/rfcs/rfc3920.html))

Development is still left and parts where I hope to include them later on

1. Group Chat
1. Image sending support

In short this example project along with the singleton has almost all features that are needed for a One-to-One chat application to have.

