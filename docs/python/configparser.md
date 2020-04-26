# configparser


This module provides the ConfigParser class which implements a basic configuration language in INI files. You can use this to write Python programs which can be customized by end users easily.



## Creating configuration file programatically


Configuration file contains sections, each section contains keys and values. configparser module can be used to read and write config files.
Creating the configuration file:-

```
import configparser
config = configparser.ConfigParser()
config['settings']={'resolution':'320x240',
                    'color':'blue'}
with open('example.ini', 'w') as configfile:
    config.write(configfile)

```

The output file contains below structure

```
[settings]
resolution = 320x240
color = blue

```

If you want to change particular field ,get the field and assign the value

```
settings=config['settings']
settings['color']='red'

```



## Basic usage


In config.ini:

```
[DEFAULT]
debug = True
name = Test
password = password

[FILES]
path = /path/to/file

```

In Python:

```
from ConfigParser import ConfigParser
config = ConfigParser()

#Load configuration file
config.read(&quot;config.ini&quot;)

# Access the key &quot;debug&quot; in &quot;DEFAULT&quot; section
config.get(&quot;DEFAULT&quot;, &quot;debug&quot;)
# Return 'True'

# Access the key &quot;path&quot; in &quot;FILES&quot; destion
config.get(&quot;FILES&quot;, &quot;path&quot;)
# Return '/path/to/file'

```



#### Syntax


- Each new line contains a new key value pair separated by the = sign
- Keys can be separated in sections
- In the INI file, each section title is written between brackets: []



#### Remarks


All return values from `ConfigParser.ConfigParser().get` are strings. It can be converted to more common types thanks to `eval`

