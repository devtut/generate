---
metaTitle: "Ruby - ERB"
description: "Parsing ERB"
---

# ERB




## Parsing ERB


This example is filtered text from an `IRB` session.



#### Syntax


- <% number = rand(10) %> this code will be evaluated
- <%= number %> this code will be evaluated and inserted into the output
- <%# comment text %> this comment will not be evaluated



#### Remarks


Conventions:

- ERB as a template: Abstract business logic into accompanied helper code, and keep your ERB templates clean and readable for people without Ruby knowledge.
- Append files with `.erb`: e.g. `.js.erb`, `.html.erb`, `.css.erb`, etc.

