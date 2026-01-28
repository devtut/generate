---
metaTitle: "HTML - Global Attributes"
description: "Contenteditable Attribute"
---

# Global Attributes



## Contenteditable Attribute


```html
<p contenteditable>This is an editable paragraph.</p>

```

Upon clicking on the paragraph, the content of it can be edited similar to an input text field.

When the `contenteditable` attribute is not set on an element, the element will inherit it from its parent. So all child text of a content editable element will also be editable, but you **can** turn it off for specific text, like so:

```html
<p contenteditable>
  This is an editable paragraph.
  <span contenteditable="false">But not this.</span>
</p>

```

Note that an uneditable text element inside an editable element will still have a text cursor as inherited from its parent as well.



#### Parameters


|Attribute|Description
|---|---|---|---|---|---|---|---|---|---
|`class`|Defines one or more class names for an element. See [Classes and IDs](http://stackoverflow.com/documentation/html/586/classes-and-ids).
|`contenteditable`|Sets whether the content of an element can be edited.
|`contextmenu`|Defines a context menu shown when a user right-clicks an element.
|`dir`|Sets the text direction for text within an element.
|`draggable`|Sets whether an element can be dragged.
|`hidden`|Hides an element not currently in use on the page.
|`id`|Defines a unique identifier for an element. See [Classes and IDs](http://stackoverflow.com/documentation/html/586/classes-and-ids).
|`lang`|Defines the language of an element's content and its text attribute values. See [Content Languages](http://stackoverflow.com/documentation/html/737/content-languages).
|`spellcheck`|Sets whether to spell/grammar check the content of an element.
|`style`|Defines a set of inline CSS styles for an element.
|`tabindex`|Sets the order in which elements on a page are navigated by the tab keyboard shortcut.
|`title`|Defines additional information about an element, generally in the form of tooltip text on mouseover.
|`translate`|Defines whether to translate the content of an element.



#### Remarks


Global attributes are simply attributed which can be applied to any element in the entire document.

