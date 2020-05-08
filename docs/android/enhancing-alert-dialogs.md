---
metaTitle: "Android - Enhancing Alert Dialogs"
description: "Alert dialog containing a clickable link"
---

# Enhancing Alert Dialogs


This topic is about enhancing an [`AlertDialog`](https://developer.android.com/reference/android/app/AlertDialog.html) with additional features.



## Alert dialog containing a clickable link


In order to show an alert dialog containing a link which can be opened by clicking it, you can use the following code:

```java
AlertDialog.Builder builder1 = new AlertDialog.Builder(youractivity.this);

builder1.setMessage(Html.fromHtml("your message,<a href=\"http://www.google.com\">link</a>"));

builder1.setCancelable(false);
builder1.setPositiveButton("ok", new DialogInterface.OnClickListener() {
    @Override
    public void onClick(DialogInterface dialog, int which) {
    }
});

AlertDialog Alert1 = builder1.create();
Alert1 .show();
((TextView)Alert1.findViewById(android.R.id.message)).setMovementMethod(LinkMovementMethod.getInstance());

```

