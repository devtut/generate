# HTML Parsing



## Using CSS selectors in BeautifulSoup


BeautifulSoup has a [limited support for CSS selectors](https://www.crummy.com/software/BeautifulSoup/bs4/doc/#css-selectors), but covers most commonly used ones. Use `select()` method to find multiple elements and `select_one()` to find a single element.

Basic example:

```
from bs4 import BeautifulSoup

data = &quot;&quot;&quot;
<ul>
    <li class=&quot;item&quot;>item1</li>
    <li class=&quot;item&quot;>item2</li>
    <li class=&quot;item&quot;>item3</li>
</ul>
&quot;&quot;&quot;

soup = BeautifulSoup(data, &quot;html.parser&quot;)

for item in soup.select(&quot;li.item&quot;):
    print(item.get_text())

```

Prints:

```
item1
item2
item3

```



## Locate a text after an element in BeautifulSoup


Imagine you have the following HTML:

```
<div>
    <label>Name:</label>
    John Smith
</div>

```

And you need to locate the text &quot;John Smith&quot; after the `label` element.

In this case, you can locate the `label` element by text and then use [`.next_sibling` property](https://www.crummy.com/software/BeautifulSoup/bs4/doc/#next-sibling-and-previous-sibling):

```
from bs4 import BeautifulSoup

data = &quot;&quot;&quot;
<div>
    <label>Name:</label>
    John Smith
</div>
&quot;&quot;&quot;

soup = BeautifulSoup(data, &quot;html.parser&quot;)

label = soup.find(&quot;label&quot;, text=&quot;Name:&quot;)
print(label.next_sibling.strip())

```

Prints `John Smith`.



## PyQuery


pyquery is a jquery-like library for python. It has very well support for css selectors.

```
from pyquery import PyQuery

html = &quot;&quot;&quot;
<h1>Sales</h1>
<table id=&quot;table&quot;>
<tr>
    <td>Lorem</td>
    <td>46</td>
</tr>
<tr>
    <td>Ipsum</td>
    <td>12</td>
</tr>
<tr>
    <td>Dolor</td>
    <td>27</td>
</tr>
<tr>
    <td>Sit</td>
    <td>90</td>
</tr>
</table>
&quot;&quot;&quot;

doc = PyQuery(html)

title = doc('h1').text()

print title

table_data = []

rows = doc('#table > tr')
for row in rows:
    name = PyQuery(row).find('td').eq(0).text()
    value = PyQuery(row).find('td').eq(1).text()

    print &quot;%s\t  %s&quot; % (name, value) 

```

