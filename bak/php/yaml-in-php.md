---
metaTitle: "PHP - YAML in PHP"
description: "Installing YAML extension, Using YAML to store application configuration"
---

# YAML in PHP



## Installing YAML extension


YAML does not come with a standard PHP installation, instead it needs to be installed as a PECL extension.  On linux/unix it can be installed with a simple

```php
pecl install yaml

```

Note that `libyaml-dev` package must be installed on the system, as the PECL package is simply a wrapper around libYAML calls.

Installation on Windows machines is different - you can either download a pre-compiled DLL or build from sources.



## Using YAML to store application configuration


[YAML](http://www.yaml.org) provides a way to store structured data.  The data can be a simple set of name-value pairs or a complex hierarchical data with values even being arrays.

Consider the following YAML file:

```php
database:
    driver: mysql
    host: database.mydomain.com
    port: 3306
    db_name: sample_db
    user: myuser
    password: Passw0rd
debug: true
country: us

```

Let's say, it's saved as `config.yaml`.  Then to read this file in PHP the following code can be used:

```php
$config = yaml_parse_file('config.yaml');
print_r($config);

```

`print_r` will produce the following output:

```php
Array
(
    [database] => Array
        (
            [driver] => mysql
            [host] => database.mydomain.com
            [port] => 3306
            [db_name] => sample_db
            [user] => myuser
            [password] => Passw0rd
        )

    [debug] => 1
    [country] => us
)

```

Now config parameters can be used by simply using array elements:

```php
$dbConfig = $config['database'];

$connectString = $dbConfig['driver']
    . ":host={$dbConfig['host']}"
    . ":port={$dbConfig['port']}"
    . ":dbname={$dbConfig['db_name']}"
    . ":user={$dbConfig['user']}"
    . ":password={$dbConfig['password']}";
$dbConnection = new \PDO($connectString, $dbConfig['user'], $dbConfig['password']);

```

