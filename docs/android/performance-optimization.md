---
metaTitle: "Android - Performance Optimization"
description: "Save View lookups with the ViewHolder pattern"
---

# Performance Optimization


Your Apps performance is a crucial element of the user experience. Try to avoid bad performing patterns like doing work on the UI thread and learn how to write fast and responsive apps.



## Save View lookups with the ViewHolder pattern


Especially in a `ListView`, you can run into performance problems by doing too many `findViewById()` calls during scrolling. By using the `ViewHolder` pattern, you can save these lookups and improve your `ListView` performance.

If your list item contains a single `TextView`, create a `ViewHolder` class to store the instance:

```java
static class ViewHolder {
    TextView myTextView;
}

```

While creating your list item, attach a `ViewHolder` object to the list item:

```java
public View getView(int position, View convertView, ViewGroup parent) {
    Item i = getItem(position);
    if(convertView == null) {
        convertView = LayoutInflater.from(getContext()).inflate(R.layout.list_item, parent, false);

        // Create a new ViewHolder and save the TextView instance
        ViewHolder holder = new ViewHolder();
        holder.myTextView = (TextView)convertView.findViewById(R.id.my_text_view);
        convertView.setTag(holder);
    }

    // Retrieve the ViewHolder and use the TextView
    ViewHolder holder = (ViewHolder)convertView.getTag();
    holder.myTextView.setText(i.getText());
   
    return convertView;
}

```

Using this pattern, `findViewById()` will only be called when a new `View` is being created and the `ListView` can recycle your views much more efficiently.

