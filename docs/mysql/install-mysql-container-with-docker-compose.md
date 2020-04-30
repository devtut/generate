---
metaTitle: "Install Mysql container with Docker-Compose"
description: "Simple example with docker-compose"
---

# Install Mysql container with Docker-Compose



## Simple example with docker-compose


This is an simple example to create a mysql server with docker

1.- create **docker-compose.yml**:

**Note:** If you want to use same container for all your projects, you should create a PATH in your HOME_PATH. If you want to create it for every project you could create a **docker** directory in your project.

```sql
version: '2'
services:
  cabin_db:
    image: mysql:latest
    volumes:
      - "./.mysql-data/db:/var/lib/mysql"
    restart: always
    ports:
      - 3306:3306
    environment:
      MYSQL_ROOT_PASSWORD: rootpw
      MYSQL_DATABASE: cabin
      MYSQL_USER: cabin
      MYSQL_PASSWORD: cabinpw

```

2.- run it:

```sql
cd PATH_TO_DOCKER-COMPOSE.YML
docker-compose up -d

```

3.- connect to server

```sql
mysql -h 127.0.0.1 -u root -P 3306 -p rootpw

```

Hurray!!

4.- stop server

```sql
docker-compose stop

```

