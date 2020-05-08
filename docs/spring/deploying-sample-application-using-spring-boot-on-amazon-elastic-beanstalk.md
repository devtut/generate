---
metaTitle: "Spring Boot - Deploying Sample application using Spring-boot on Amazon Elastic Beanstalk"
description: "Deploying sample application using Spring-boot in Jar format on AWS"
---

# Deploying Sample application using Spring-boot on Amazon Elastic Beanstalk



## Deploying sample application using Spring-boot in Jar format on AWS


<li>
Create a sample application using spring-boot from [spring-boot initializer](https://start.spring.io/) site.
</li>
<li>
Import the code in your local IDE and run the goal as **clean install spring-boot:run -e**
</li>
<li>
Go to target folder and check for the jar file.
</li>
<li>
Open your Amazon account or create a new Amazon Account and select for the Elastic Beanstalk as shown in the below [<img src="http://i.stack.imgur.com/TEys0.png" alt="image" />](http://i.stack.imgur.com/TEys0.png).
</li>
<li>
Create a new web server environment as shown in below [<img src="http://i.stack.imgur.com/R9Xh2.png" alt="figure" />](http://i.stack.imgur.com/R9Xh2.png).
</li>
<li>
<p>Select the Environment type as Java for **JAR** file deployment for Spring-boot, if you are planning to deploy as a **WAR** file, it should be selected as tomcat as shown in below [<img src="http://i.stack.imgur.com/0AIjj.png" alt="figures" />](http://i.stack.imgur.com/0AIjj.png).
[<img src="http://i.stack.imgur.com/fDWVZ.png" alt="figure2" />](http://i.stack.imgur.com/fDWVZ.png)</p>
</li>
<li>
Select with Default configuration's upon clicking next next ...
</li>
<li>
<p>Once you complete the default configuration's, in the overview screen the JAR file can be uploaded and deployed as shown in the figures.
[<img src="http://i.stack.imgur.com/SRUap.png" alt="uploadanddeploy" />](http://i.stack.imgur.com/SRUap.png)
[<img src="http://i.stack.imgur.com/pasDz.png" alt="deploy" />](http://i.stack.imgur.com/pasDz.png)</p>
</li>
<li>
<p>Once the Deployment is successful (5 -10 minutes for the first time)
you can hit the context url as shown in the figure below.
[<img src="http://i.stack.imgur.com/p68LF.png" alt="contexturl" />](http://i.stack.imgur.com/p68LF.png)</p>
</li>
<li>
Result is as shown below,it should work as same as with your local env.
</li>

[<img src="http://i.stack.imgur.com/soXoe.png" alt="Env" />](http://i.stack.imgur.com/soXoe.png)

1. Please find my [Github URL](https://github.com/Praveenmail2him/awsjardeployspringboot)

