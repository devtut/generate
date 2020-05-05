---
metaTitle: "R - Meta: Documentation Guidelines"
description: "Style, Making good examples"
---

# Meta: Documentation Guidelines




## Style


### Prompts

If you want your code to be copy-pastable, remove prompts such as `R>`, `>`, or `+` at the beginning of each new line. Some Docs authors prefer to not make copy-pasting easy, and that is okay.

### Console output

Console output should be clearly distinguished from code. Common approaches include:

- Include prompts on input (as seen when using the console).
- Comment out all output, with `#` or `##` starting each line.
- Print as-is, trusting the leading `[1]` to make the output stand out from the input.
- Add a blank line between code and console output.

### Assignment

`=` and `<-` are fine for assigning R objects. Use white space appropriately to avoid writing code that is difficult to parse, such as `x<-1` (ambiguous between `x <- 1` and `x < -1`)

### Code comments

Be sure to explain the purpose and function of the code itself. There isn't any hard-and-fast rule on whether this explanation should be in prose or in code comments. Prose may be more readable and allows for longer explanations, but code comments make for easier copy-pasting. Keep both options in mind.

### Sections

Many examples are short enough to not need sections, but if you use them, start with [H1](http://stackoverflow.com/editing-help#headers).



## Making good examples


Most of the guidance for [creating good examples](http://stackoverflow.com/questions/5963269/how-to-make-a-great-r-reproducible-example) for Q&A carries over into the documentation.

<li>
Make it minimal and get to the point. Complications and digressions are counterproductive.
</li>
<li>
Include both working code and prose explaining it. Neither one is sufficient on its own.
</li>
<li>
Don't rely on external sources for data. Generate data or use the datasets library if possible:

```r
library(help = "datasets")

```


</li>

There are some additional considerations in the context of Docs:

<li>
Refer to built-in docs like `?data.frame` whenever relevant. The SO Docs are not an attempt to replace the built-in docs. It is important to make sure new R users know that the built-in docs exist as well as how to find them.
</li>
<li>
Move content that applies to multiple examples to the Remarks section.
</li>



#### Remarks


To discuss editing the R tag Docs, visit the [R chat](http://chat.stackoverflow.com/rooms/25312/r-public).

