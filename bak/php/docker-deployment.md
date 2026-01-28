---
metaTitle: "PHP - Docker deployment"
description: "Get docker image for php, Writing dockerfile, Building image, Starting application container"
---

# Docker deployment


[Docker](http://www.docker.com) is a very popular container solution being used widely for deploying code in production environments. It makes it easier to **Manage** and **Scale** web-applications and microservices.



## Get docker image for php


In order to deploy the application on docker, first we need to get the image from registry.

```php
docker pull php

```

This will get you the latest version of image from **official php repository**. Generally speaking, `PHP` is usually used to deploy web-applications so we need an http server to go with the image. `php:7.0-apache` image comes pre-installed  with apache to make deployment hastle free.



## Writing dockerfile


`Dockerfile` is used to configure the custom image that we will be building with the web-application codes. Create a new file `Dockerfile` in the root folder of project and then put the following contents in the same

```php
FROM php:7.0-apache
COPY /etc/php/php.ini /usr/local/etc/php/
COPY . /var/www/html/
EXPOSE 80

```

The first line is pretty straight forward and is used to describe which image should be used to build out new image. The same could be changed to any other specific version of PHP from the registry.

Second line is simply to upload `php.ini` file to our image. You can always change that file to some other custom file location.

The third line would copy the codes in current directory to `/var/www/html` which is our webroot. Remember `/var/www/html` inside the image.

The last line would simply open up port 80 inside the docker container.

### Ignoring files

In some instances there might be some files that you don't want on server like environment configuration etc. Let us assume that we have our environment in `.env`. Now in order to ignore this file, we can simply add it to `.dockerignore` in the root folder of our codebase.



## Building image


Building image is not something specific to `php`, but in order to build the image that we described above, we can simply use

```php
docker build -t <Image name> .

```

Once the image is built, you can verify the same using

```php
docker images

```

Which would list out all the images installed in your system.



## Starting application container


Once we have an image ready, we can start and serve the same. In order to create a `container` from the image, use

```php
docker run -p 80:80 -d <Image name>

```

In the command above `-p 80:80` would forward port `80` of your server to port `80` of the container. The flag `-d` tells that the container should run as background job. The final  specifies which image should be used to build the container.

### Checking container

In order to check running containers, simply use

```php
docker ps

```

This will list out all the containers running on docker daemon.

### Application logs

Logs are very important to debug the application. In order to check on them use

```php
docker logs <Container id>

```



#### Remarks


This document assumes that docker is installed and the daemon running. You can refer to [Docker installation](https://docs.docker.com/engine/installation/) to check on how to install the same.

