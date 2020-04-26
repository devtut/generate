# Templates in python




## Simple data output program using template


```
from string import Template

data = dict(item = &quot;candy&quot;, price = 8, qty = 2)

# define the template
t = Template(&quot;Simon bought $qty $item for $price dollar&quot;)   
print(t.substitute(data))

```

Output:

```
Simon bought 2 candy for 8 dollar

```

Templates support $-based substitutions instead of %-based substitution. **Substitute** (mapping, keywords) performs template substitution, returning a new string.

Mapping is any dictionary-like object with keys that match with the template placeholders. In this example, price and qty are placeholders. Keyword arguments can also be used as placeholders. Placeholders from keywords take precedence if both are present.



## Changing delimiter


You can change the &quot;$&quot; delimiter to any other. The following example:

```
from string import Template

class MyOtherTemplate(Template):
    delimiter = &quot;#&quot;


data = dict(id = 1, name = &quot;Ricardo&quot;)
t = MyOtherTemplate(&quot;My name is #name and I have the id: #id&quot;)
print(t.substitute(data))

```

You can read de docs [here](https://docs.python.org/3/library/string.html?highlight=template#string.Template.template)

