---
metaTitle: "Android - Jenkins CI setup for Android Projects"
description: "Step by step approach to set up Jenkins for Android"
---

# Jenkins CI setup for Android Projects



## Step by step approach to set up Jenkins for Android


This is a step by step guide to set up the automated build process using Jenkins CI for your Android projects. The following steps assume that you have new hardware with just any flavor of Linux installed. It is also taken into account that you might have a remote machine.

### PART I: Initial setup on your machine

&lt;li>
Log in via **ssh** to your Ubuntu machine:
<blockquote>
ssh username@xxx.xxx.xxx
</blockquote>
&lt;/li>
&lt;li>
Download a version of the Android SDK on your machine:
<blockquote>
wget [https://dl.google.com/android/android-sdk_r24.4.1-linux.tgz](https://dl.google.com/android/android-sdk_r24.4.1-linux.tgz)
</blockquote>
&lt;/li>
&lt;li>
Unzip the downloaded **tar** file:
<blockquote>
<p>sudo apt-get install tar<br />
tar -xvf android-sdk_r24.4.1-linux.tgz</p>
</blockquote>
&lt;/li>
&lt;li>
Now you need to install Java 8 on your Ubuntu machine, which is a requirement for Android builds on Nougat. Jenkins would require you to install JDK and JRE 7 using the steps below:
<blockquote>
<p>sudo apt-get install python-software-properties<br />
sudo add-apt-repository ppa:webupd8team/java<br />
sudo apt-get update<br />
apt-get install openjdk-8-jdk</p>
</blockquote>
&lt;/li>
&lt;li>
Now install Jenkins on your Ubuntu machine:
<blockquote>
<p>wget -q -O - [https://pkg.jenkins.io/debian/jenkins-ci.org.key](https://pkg.jenkins.io/debian/jenkins-ci.org.key) | sudo apt-key add -<br />
sudo sh -c 'echo deb [http://pkg.jenkins.io/debian-stable](http://pkg.jenkins.io/debian-stable) binary/ > /etc/apt/sources.list.d/jenkins.list'<br />
sudo apt-get update<br />
sudo apt-get install jenkins</p>
</blockquote>
&lt;/li>
&lt;li>
Download the latest supported Gradle version for your Android setup:
<blockquote>
<p>wget [https://services.gradle.org/distributions/gradle-2.14.1-all.zip](https://services.gradle.org/distributions/gradle-2.14.1-all.zip)<br />
unzip gradle-2.14.1-all.zip</p>
</blockquote>
&lt;/li>
&lt;li>
Set up Android on your Ubuntu machine. First move to the **tools** folder in the Android SDK folder downloaded in step 2:
<blockquote>
<p>cd android-sdk-linux/tools **// lists available SDK**<br />
android update sdk --no-ui **// Updates SDK version**<br />
android list sdk -a | grep "SDK Build-tools" **// lists available build tools**<br />
android update sdk -a -u -t 4   **// updates build tools version to one listed as 4 by prev. cmd.**<br />
update java</p>
</blockquote>
&lt;/li>
&lt;li>
Install **Git** or any other VCS on your machine:
<blockquote>
sudo apt-get install git
</blockquote>
&lt;/li>
&lt;li>
Now log in to Jenkins using your internet browser. Type **`ipAddress:8080`** into the address bar.
&lt;/li>
&lt;li>
In order to receive the password for the first-time login, please check the corresponding file as follows (you will need su permissions to access this file):
<blockquote>
cat /var/lib/jenkins/secrets/initialAdminPassword
</blockquote>
&lt;/li>

> 
wget [https://dl.google.com/android/android-sdk_r24.4.1-linux.tgz](https://dl.google.com/android/android-sdk_r24.4.1-linux.tgz)


> 
<p>sudo apt-get install python-software-properties<br />
sudo add-apt-repository ppa:webupd8team/java<br />
sudo apt-get update<br />
apt-get install openjdk-8-jdk</p>


> 
<p>wget [https://services.gradle.org/distributions/gradle-2.14.1-all.zip](https://services.gradle.org/distributions/gradle-2.14.1-all.zip)<br />
unzip gradle-2.14.1-all.zip</p>


> 
sudo apt-get install git


### PART II: Set up Jenkins to build Android Jobs

&lt;li>
Once logged in, go to the following path:
<blockquote>
Jenkins > Manage Jenkins > Global Tool Configuration
</blockquote>
&lt;/li>
&lt;li>
At this location, add `JAVA_HOME` with the following entries:
<blockquote>
<p>Name = JAVA_HOME<br />
JAVA_HOME = /usr/lib/jvm/java-8-openjdk-amd64</p>
</blockquote>
&lt;/li>
&lt;li>
Also add the following values to **Git** and save the environment variables:
<blockquote>
<p>Name = Default<br />
/usr/bin/git</p>
</blockquote>
&lt;/li>
&lt;li>
Now go to the following path:
<blockquote>
Jenkins > Manage Jenkins > Configuration
</blockquote>
&lt;/li>
&lt;li>
At this location, add `ANDROID_HOME` to the "global properties":
<blockquote>
<p>Name = ANDROID_HOME<br />
Value = /home/username/android-sdk-linux</p>
</blockquote>
&lt;/li>

> 
<p>Name = JAVA_HOME<br />
JAVA_HOME = /usr/lib/jvm/java-8-openjdk-amd64</p>


> 
Jenkins > Manage Jenkins > Configuration


### Part III: Create a Jenkins Job for your Android project

&lt;li>
Click on **New Item** in the Jenkins home screen.
&lt;/li>
&lt;li>
Add a **Project Name** and **Description**.
&lt;/li>
&lt;li>
In the **General** tab, select **Advanced**. Then select **Use custom workspace**:
<blockquote>
Directory /home/user/Code/ProjectFolder
</blockquote>
&lt;/li>
&lt;li>
In the source code management select **Git**. I am using **Bitbucket** for the purpose of this example:
<blockquote>
Repository URL = [https://username:password@bitbucket.org/project/projectname.git](https://username:password@bitbucket.org/project/projectname.git)
</blockquote>
&lt;/li>
&lt;li>
Select additional behaviors for your repository:
<blockquote>
<p>Clean Before Checkout<br />
Checkout to a sub-directory. Local subdirectory for repo /home/user/Code/ProjectFolder</p>
</blockquote>
&lt;/li>
&lt;li>
Select a branch you want to build:
<blockquote>
*/master
</blockquote>
&lt;/li>
&lt;li>
In the **Build** tab, select **Execute Shell** in **Add build step**.
&lt;/li>
&lt;li>
In the **Execute shell**, add the following command:
<blockquote>
cd /home/user/Code/ProjectFolder && gradle clean assemble --no-daemon
</blockquote>
&lt;/li>
&lt;li>
If you want to run Lint on the project, then add another build step into the **Execute shell**:
<blockquote>
/home/user/gradle/gradle-2.14.1/bin/gradle lint
</blockquote>
&lt;/li>

> 
Repository URL = [https://username:password@bitbucket.org/project/projectname.git](https://username:password@bitbucket.org/project/projectname.git)


> 
*/master


> 
/home/user/gradle/gradle-2.14.1/bin/gradle lint


Now your system is finally set up to build Android projects using Jenkins. This setup makes your life so much easier for releasing builds to QA and UAT teams.

PS: Since Jenkins is a different user on your Ubuntu machine, you should give it rights to create folders in your workspace by executing the following command:

> 
chown -R jenkins .git


