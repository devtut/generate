---
metaTitle: "Android - RecyclerView"
description: "Adding a RecyclerView, Smoother loading of items, Animate data change, RecyclerView with DataBinding, Popup menu with recyclerView, Using several ViewHolders with ItemViewType, Filter items inside RecyclerView with a SearchView, Drag&Drop and Swipe with RecyclerView, Add header/footer to a RecyclerView, Show default view till items load or when data is not available, Endless Scrolling in Recycleview., Add divider lines to RecyclerView items"
---

# RecyclerView


[RecyclerView](https://developer.android.com/reference/android/support/v7/widget/RecyclerView.html) is a more advanced version of List View with improved performance and additional features.



## Adding a RecyclerView


Add the dependency as described in the Remark section, then add a `RecyclerView` to your layout:

```java
<android.support.v7.widget.RecyclerView
    android:id="@+id/my_recycler_view"
    android:layout_width="match_parent"
    android:layout_height="wrap_content"/>

```

Once you have added a `RecyclerView` widget to your layout, obtain a handle to the object, connect it to a layout manager and attach an adapter for the data to be displayed:

```java
mRecyclerView = (RecyclerView) findViewById(R.id.my_recycler_view);

// set a layout manager (LinearLayoutManager in this example)

mLayoutManager = new LinearLayoutManager(getApplicationContext());
mRecyclerView.setLayoutManager(mLayoutManager);

// specify an adapter
mAdapter = new MyAdapter(myDataset);
mRecyclerView.setAdapter(mAdapter);

```

Or simply setup layout manager from xml by adding this lines:

```java
xmlns:app="http://schemas.android.com/apk/res-auto"
app:layoutManager="android.support.v7.widget.LinearLayoutManager"

```

If you know that changes in content of the `RecyclerView` won't change the layout size of the `RecyclerView`,  use the following code to improve the performance of the component. If RecyclerView has a fixed size, it knows that RecyclerView itself will not resize due to its children, so it doesn’t call request layout at all. It just handles the change itself. If invalidating whatever the parent is, the coordinator, layout, or whatever. (you can use this method even before setting `LayoutManager` and `Adapter`):

```java
mRecyclerView.setHasFixedSize(true);

```

`RecyclerView` provides these built-in layout managers to use. So you can create a list, a grid and a staggered grid using `RecyclerView`:

<li>[LinearLayoutManager](https://developer.android.com/reference/android/support/v7/widget/LinearLayoutManager.html) shows items in a vertical or horizontal
scrolling list.</li>
1. [GridLayoutManager](https://developer.android.com/reference/android/support/v7/widget/GridLayoutManager.html) shows items in a grid.
1. [StaggeredGridLayoutManager](https://developer.android.com/reference/android/support/v7/widget/StaggeredGridLayoutManager.html) shows items in a staggered grid.



## Smoother loading of items


If the items in your `RecyclerView` load data from the network (commonly images) or carry out other processing, that can take a significant amount of time and you may end up with items on-screen but not fully loaded.  To avoid this you can extend the existing `LinearLayoutManager` to preload a number of items before they become visible on-screen:

```java
package com.example;

import android.content.Context;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.OrientationHelper;
import android.support.v7.widget.RecyclerView;

/**
 * A LinearLayoutManager that preloads items off-screen.
 * <p>
 * Preloading is useful in situations where items might take some time to load
 * fully, commonly because they have maps, images or other items that require
 * network requests to complete before they can be displayed.
 * <p>
 * By default, this layout will load a single additional page's worth of items,
 * a page being a pixel measure equivalent to the on-screen size of the
 * recycler view.  This can be altered using the relevant constructor, or
 * through the {@link #setPages(int)} method.
 */
public class PreLoadingLinearLayoutManager extends LinearLayoutManager {
  private int mPages = 1;
  private OrientationHelper mOrientationHelper;

  public PreLoadingLinearLayoutManager(final Context context) {
    super(context);
  }

  public PreLoadingLinearLayoutManager(final Context context, final int pages) {
    super(context);
    this.mPages = pages;
  }

  public PreLoadingLinearLayoutManager(final Context context, final int orientation, final boolean reverseLayout) {
    super(context, orientation, reverseLayout);
  }

  @Override
  public void setOrientation(final int orientation) {
    super.setOrientation(orientation);
    mOrientationHelper = null;
  }

  /**
   * Set the number of pages of layout that will be preloaded off-screen,
   * a page being a pixel measure equivalent to the on-screen size of the
   * recycler view.
   * @param pages the number of pages; can be {@code 0} to disable preloading
   */
  public void setPages(final int pages) {
    this.mPages = pages;
  }

  @Override
  protected int getExtraLayoutSpace(final RecyclerView.State state) {
    if (mOrientationHelper == null) {
      mOrientationHelper = OrientationHelper.createOrientationHelper(this, getOrientation());
    }
    return mOrientationHelper.getTotalSpace() * mPages;
  }
}

```



## Animate data change


`RecyclerView` will perform a relevant animation if any of the "notify" methods are used except for `notifyDataSetChanged`; this includes `notifyItemChanged`, `notifyItemInserted`, `notifyItemMoved`, `notifyItemRemoved`, etc.

The adapter should extend this class instead of `RecyclerView.Adapter`.

```java
import android.support.annotation.NonNull;
import android.support.v7.widget.RecyclerView;

import java.util.List;

public abstract class AnimatedRecyclerAdapter<T, VH extends RecyclerView.ViewHolder>
        extends RecyclerView.Adapter<VH> {
    protected List<T> models;
            
    protected AnimatedRecyclerAdapter(@NonNull List<T> models) {
        this.models = models;
    }

    //Set new models.
    public void setModels(@NonNull final List<T> models) {
        applyAndAnimateRemovals(models);
        applyAndAnimateAdditions(models);
        applyAndAnimateMovedItems(models);
    }

    //Remove an item at position and notify changes.
    private T removeItem(int position) {
        final T model = models.remove(position);
        notifyItemRemoved(position);
        return model;
    }

    //Add an item at position and notify changes.
    private void addItem(int position, T model) {
        models.add(position, model);
        notifyItemInserted(position);
    }

    //Move an item at fromPosition to toPosition and notify changes.
    private void moveItem(int fromPosition, int toPosition) {
        final T model = models.remove(fromPosition);
        models.add(toPosition, model);
        notifyItemMoved(fromPosition, toPosition);
    }

    //Remove items that no longer exist in the new models.
    private void applyAndAnimateRemovals(@NonNull final List<T> newTs) {
        for (int i = models.size() - 1; i >= 0; i--) {
            final T model = models.get(i);
            if (!newTs.contains(model)) {
                removeItem(i);
            }
        }
    }

    //Add items that do not exist in the old models.
    private void applyAndAnimateAdditions(@NonNull final List<T> newTs) {
        for (int i = 0, count = newTs.size(); i < count; i++) {
            final T model = newTs.get(i);
            if (!models.contains(model)) {
                addItem(i, model);
            }
        }
    }

    //Move items that have changed their position.
    private void applyAndAnimateMovedItems(@NonNull final List<T> newTs) {
        for (int toPosition = newTs.size() - 1; toPosition >= 0; toPosition--) {
            final T model = newTs.get(toPosition);
            final int fromPosition = models.indexOf(model);
            if (fromPosition >= 0 && fromPosition != toPosition) {
                moveItem(fromPosition, toPosition);
            }
        }
    }
}

```

> 
You should **NOT** use the same `List` for `setModels` and `List` in the adapter.


You declare `models` as global variables. `DataModel` is a dummy class only.

```java
private List<DataModel> models;
private YourAdapter adapter;

```

Initialize `models` before pass it to adapter. `YourAdapter` is the implementation of `AnimatedRecyclerAdapter`.

```java
models = new ArrayList<>();
//Add models
models.add(new DataModel());
//Do NOT pass the models directly. Otherwise, when you modify global models, 
//you will also modify models in adapter.
//adapter = new YourAdapter(models); <- This is wrong.
adapter = new YourAdapter(new ArrayList(models));

```

Call this after you have updated your global `models`.

```java
adapter.setModels(new ArrayList(models));

```

> 
If you do not override `equals`, all the comparison is compared by reference.


### Example using SortedList

Android introduced the [`SortedList`](https://developer.android.com/reference/android/support/v7/util/SortedList.html) class soon after `RecyclerView` was introduced. This class handles all 'notify' method calls to the `RecyclerView.Adapter` to ensure proper animation, and even allows batching multiple changes, so the animations don't jitter.

```java
import android.support.v7.util.SortedList;
import android.support.v7.widget.RecyclerView;
import android.support.v7.widget.util.SortedListAdapterCallback;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import java.util.List;

public class MyAdapter extends RecyclerView.Adapter<MyAdapter.ViewHolder> {

    private SortedList<DataModel> mSortedList;

    class ViewHolder extends RecyclerView.ViewHolder {

        TextView text;
        CheckBox checkBox;

        ViewHolder(View itemView){
            super(itemView);

            //Initiate your code here...

        }

        void setDataModel(DataModel model) {
            //Update your UI with the data model passed here...
            text.setText(modle.getText());
            checkBox.setChecked(model.isChecked());
        }
    }

    public MyAdapter() {
        mSortedList = new SortedList<>(DataModel.class, new SortedListAdapterCallback<DataModel>(this) {
            @Override
            public int compare(DataModel o1, DataModel o2) {
                //This gets called to find the ordering between objects in the array.
                if (o1.someValue() < o2.someValue()) {
                    return -1;
                } else if (o1.someValue() > o2.someValue()) {
                    return 1;
                } else {
                    return 0;
                }
            }

            @Override
            public boolean areContentsTheSame(DataModel oldItem, DataModel newItem) {
                //This is to see of the content of this object has changed. These items are only considered equal if areItemsTheSame() returned true.

                //If this returns false, onBindViewHolder() is called with the holder containing the item, and the item's position.
                return oldItem.getText().equals(newItem.getText()) && oldItem.isChecked() == newItem.isChecked();
            }

            @Override
            public boolean areItemsTheSame(DataModel item1, DataModel item2) {
                //Checks to see if these two items are the same. If not, it is added to the list, otherwise, check if content has changed.
                return item1.equals(item2);
            }
        });
    }

    @Override
    public ViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        View itemView = //Initiate your item view here.
        return new ViewHolder(itemView);
    }

    @Override
    public void onBindViewHolder(ViewHolder holder, int position) {
        //Just update the holder with the object in the sorted list from the given position
        DataModel model = mSortedList.get(position);
        if (model != null) {
            holder.setDataModel(model);
        }
    }

    @Override
    public int getItemCount() {
        return mSortedList.size();
    }

    public void resetList(List<DataModel> models) {
        //If you are performing multiple changes, use the batching methods to ensure proper animation.
        mSortedList.beginBatchedUpdates();
        mSortedList.clear();
        mSortedList.addAll(models);
        mSortedList.endBatchedUpdates();
    }

    //The following methods each modify the data set and automatically handles calling the appropriate 'notify' method on the adapter.
    public void addModel(DataModel model) {
        mSortedList.add(model);
    }

    public void addModels(List<DataModel> models) {
        mSortedList.addAll(models);
    }

    public void clear() {
        mSortedList.clear();
    }

    public void removeModel(DataModel model) {
        mSortedList.remove(model);
    }

    public void removeModelAt(int i) {
        mSortedList.removeItemAt(i);
    }
}

```



## RecyclerView with DataBinding


Here is a generic ViewHolder class that you can use with any DataBinding layout. Here an instance of particular [`ViewDataBinding`](https://developer.android.com/reference/android/databinding/ViewDataBinding.html) class is created using the inflated `View` object and [`DataBindingUtil`](https://developer.android.com/reference/android/databinding/DataBindingUtil.html) utility class.

```java
import android.databinding.DataBindingUtil;
import android.support.v7.widget.RecyclerView;
import android.view.View;

public class BindingViewHolder<T> extends RecyclerView.ViewHolder{

    private final T binding;

    public BindingViewHolder(View itemView) {
        super(itemView);
        binding = (T)DataBindingUtil.bind(itemView);
    }

    public T getBinding() {
        return binding;
    }
}

```

After creating this class you can use the `<layout>` in your layout file to enable databinding for that layout like this:

`file name: my_item.xml`

```java
<?xml version="1.0" encoding="utf-8"?>
<layout xmlns:android="http://schemas.android.com/apk/res/android">

    <data>
        <variable
            name="item"
            type="ItemModel" />
    </data>

    <LinearLayout
        android:layout_width="match_parent"
        android:layout_height="match_parent">

        <TextView
            android:layout_width="wrap_content"
            android:layout_height="match_parent"
            android:text="@{item.itemLabel}" />
    </LinearLayout>
</layout>

```

and here is your sample dataModel:

```java
public class ItemModel {
    public String itemLabel;
}

```

By default, Android Data Binding library generates a `ViewDataBinding` class based on the layout file name, converting it to Pascal case and suffixing "Binding" to it. For this example it would be `MyItemBinding` for the layout file `my_item.xml`. That Binding class would also have a setter method to set the object defined as data in the layout file(`ItemModel` for this example).

Now that we have all the pieces we can implement our adapter like this:

```java
class MyAdapter extends RecyclerView.Adapter<BindingViewHolder<MyItemBinding>>{
    ArrayList<ItemModel> items = new ArrayList<>();
        
    public MyAdapter(ArrayList<ItemModel> items) {
        this.items = items;
    }

    @Override public BindingViewHolder<MyItemBinding> onCreateViewHolder(ViewGroup parent, int viewType) {
        return new BindingViewHolder<>(LayoutInflater.from(parent.getContext()).inflate(R.layout.my_item, parent, false));
    }

    @Override public void onBindViewHolder(BindingViewHolder<ItemModel> holder, int position) {
        holder.getBinding().setItemModel(items.get(position));
        holder.getBinding().executePendingBindings();
    }

    @Override public int getItemCount() {
        return items.size();
    }
}

```



## Popup menu with recyclerView


put this code inside your ViewHolder

note:  In this code I am using `btnExpand` click-event, for whole `recyclerview` click event you can set listener to itemView object.

```java
public class MyViewHolder extends RecyclerView.ViewHolder{
        CardView cv;
        TextView recordName, visibleFile, date, time;
        Button btnIn, btnExpand;

        public MyViewHolder(final View itemView) {
            super(itemView);

            cv = (CardView)itemView.findViewById(R.id.cardview);
            recordName = (TextView)itemView.findViewById(R.id.tv_record);
            visibleFile = (TextView)itemView.findViewById(R.id.visible_file);
            date = (TextView)itemView.findViewById(R.id.date);
            time = (TextView)itemView.findViewById(R.id.time);
            btnIn = (Button)itemView.findViewById(R.id.btn_in_out);

            btnExpand = (Button) itemView.findViewById(R.id.btn_expand);

            btnExpand.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    PopupMenu popup = new PopupMenu(btnExpand.getContext(), itemView);

                    popup.setOnMenuItemClickListener(new PopupMenu.OnMenuItemClickListener() {
                        @Override
                        public boolean onMenuItemClick(MenuItem item) {
                            switch (item.getItemId()) {
                                case R.id.action_delete:
                                    moveFile(recordName.getText().toString(), getAdapterPosition());
                                    return true;
                                case R.id.action_play:
                                    String valueOfPath = recordName.getText().toString();
                                    Intent intent = new Intent();
                                    intent.setAction(android.content.Intent.ACTION_VIEW);
                                    File file = new File(valueOfPath);
                                    intent.setDataAndType(Uri.fromFile(file), "audio/*");
                                    context.startActivity(intent);
                                    return true;
                                case R.id.action_share:
                                    String valueOfPath = recordName.getText().toString();
                                    File filee = new File(valueOfPath);
                                    try {
                                        Intent sendIntent = new Intent();
                                        sendIntent.setAction(Intent.ACTION_SEND);
                                        sendIntent.setType("audio/*");
                                        sendIntent.putExtra(Intent.EXTRA_STREAM, Uri.fromFile(filee));
                                        context.startActivity(sendIntent);
                                    } catch (NoSuchMethodError | IllegalArgumentException | NullPointerException e) {
                                        e.printStackTrace();
                                    } catch (Exception e) {
                                        e.printStackTrace();
                                    }
                                    return true;
                                default:
                                    return false;
                            }
                        }
                    });
                    // here you can inflate your menu
                    popup.inflate(R.menu.my_menu_item);
                    popup.setGravity(Gravity.RIGHT);

                    // if you want icon with menu items then write this try-catch block.
                    try {
                        Field mFieldPopup=popup.getClass().getDeclaredField("mPopup");
                        mFieldPopup.setAccessible(true);
                        MenuPopupHelper mPopup = (MenuPopupHelper) mFieldPopup.get(popup);
                        mPopup.setForceShowIcon(true);
                    } catch (Exception e) {

                    }
                    popup.show();
                }
            });

        }
    }

```

alternative way to show icons in menu

```java
try {
    Field[] fields = popup.getClass().getDeclaredFields();
    for (Field field : fields) {
        if ("mPopup".equals(field.getName())) {
            field.setAccessible(true);
            Object menuPopupHelper = field.get(popup);
            Class<?> classPopupHelper = Class.forName(menuPopupHelper
                .getClass().getName());
            Method setForceIcons = classPopupHelper.getMethod(
                "setForceShowIcon", boolean.class);
            setForceIcons.invoke(menuPopupHelper, true);
            break;
        }
    }
} catch (Exception e) {

}

```

Here is the output:

[<img src="https://i.stack.imgur.com/FthDk.png" alt="enter image description here" />](https://i.stack.imgur.com/FthDk.png)



## Using several ViewHolders with ItemViewType


Sometimes a RecyclerView will need to use several types of Views to be displayed in the list shown in the UI, and each View needs a different layout xml to be inflated.

For this issue, you may use different ViewHolders in single Adapter, by using a special method in RecyclerView - `getItemViewType(int position)`.

Below is example of using two ViewHolders:

<li>
A ViewHolder for displaying list entries
</li>
<li>
A ViewHolder for displaying multiple header views

```java
@Override
public RecyclerView.ViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
    View itemView = LayoutInflater.from(context).inflate(viewType, parent, false);
    return ViewHolder.create(itemView, viewType);
}

@Override
public void onBindViewHolder(RecyclerView.ViewHolder holder, int position) {
    final Item model = this.items.get(position);
    ((ViewHolder) holder).bind(model);
}

@Override
public int getItemViewType(int position) {
    return inSearchState ? R.layout.item_header : R.layout.item_entry;
}

abstract class ViewHolder {
    abstract void bind(Item model);

    public static ViewHolder create(View v, int viewType) {
        return viewType == R.layout.item_header ? new HeaderViewHolder(v) :new EntryViewHolder(v);
    }
}  

static class EntryViewHolder extends ViewHolder {
    private View v;

    public EntryViewHolder(View v) {
        this.v = v;
    }

    @Override public void bind(Item model) {
        // Bind item data to entry view.
    }
}

static class HeaderViewHolder extends ViewHolder {
    private View v;

    public HeaderViewHolder(View v) {
        this.v = v;
    }

    @Override public void bind(Item model) {
        // Bind item data to header view.
    }
}

```


</li>



## Filter items inside RecyclerView with a SearchView


add `filter` method in `RecyclerView.Adapter`:

```java
public void filter(String text) {
        if(text.isEmpty()){
            items.clear();
            items.addAll(itemsCopy);
        } else{
            ArrayList<PhoneBookItem> result = new ArrayList<>();
            text = text.toLowerCase();
            for(PhoneBookItem item: itemsCopy){
                //match by name or phone
                if(item.name.toLowerCase().contains(text) || item.phone.toLowerCase().contains(text)){
                    result.add(item);
                }
            }
            items.clear();
            items.addAll(result);
        }
        notifyDataSetChanged();
    }

```

`itemsCopy` is initialized in adapter's constructor like `itemsCopy.addAll(items)`.

If you do so, just call `filter` from `OnQueryTextListener` from `SearchView`:

```java
searchView.setOnQueryTextListener(new SearchView.OnQueryTextListener() {
    @Override
    public boolean onQueryTextSubmit(String query) {
        adapter.filter(query);
        return true;
    }

    @Override
    public boolean onQueryTextChange(String newText) {
        adapter.filter(newText);
        return true;
    }
});

```



## Drag&Drop and Swipe with RecyclerView


You can implement the swipe-to-dismiss and drag-and-drop features with the RecyclerView without using 3rd party libraries.<br />
Just use the [`ItemTouchHelper`](https://developer.android.com/reference/android/support/v7/widget/helper/ItemTouchHelper.html) class included in the RecyclerView support library.

Instantiate the `ItemTouchHelper` with the [`SimpleCallback`](https://developer.android.com/reference/android/support/v7/widget/helper/ItemTouchHelper.SimpleCallback.html) callback and depending on which functionality you support, you should override `onMove(RecyclerView, ViewHolder, ViewHolder)` and / or `onSwiped(ViewHolder, int)`and and finally attach to your `RecyclerView`.

```java
ItemTouchHelper.SimpleCallback simpleItemTouchCallback = new ItemTouchHelper.SimpleCallback(0, ItemTouchHelper.LEFT | ItemTouchHelper.RIGHT) {

    @Override
    public void onSwiped(RecyclerView.ViewHolder viewHolder, int swipeDir) {
        // remove item from adapter
    }

    @Override
    public boolean onMove(RecyclerView recyclerView, RecyclerView.ViewHolder viewHolder, RecyclerView.ViewHolder target) {
            final int fromPos = viewHolder.getAdapterPosition();
            final int toPos = target.getAdapterPosition();
            // move item in `fromPos` to `toPos` in adapter.
            return true;// true if moved, false otherwise
        }
    
};

ItemTouchHelper itemTouchHelper = new ItemTouchHelper(simpleItemTouchCallback);
itemTouchHelper.attachToRecyclerView(recyclerView);

```

It's worth mentioning that `SimpleCallback` constructor applies the same swiping strategy to all items in the `RecyclerView`. It's possible in any case to update the default swiping direction for specific items by simply overriding method `getSwipeDirs(RecyclerView, ViewHolder)`.

Let's suppose for example that our `RecyclerView` includes a `HeaderViewHolder` and that we obviously don't want to apply swiping to it. It will be enough to override `getSwipeDirs` as follows:

```java
@Override
public int getSwipeDirs(RecyclerView recyclerView, RecyclerView.ViewHolder viewHolder) {
    if (viewHolder instanceof HeaderViewHolder) {
        // no swipe for header
        return 0;
    }
    // default swipe for all other items
    return super.getSwipeDirs(recyclerView, viewHolder);
} 

```



## Add header/footer to a RecyclerView


This is a sample adapter code.

```java
public class SampleAdapter extends RecyclerView.Adapter<RecyclerView.ViewHolder> {

private static final int FOOTER_VIEW = 1;

// Define a view holder for Footer view

public class FooterViewHolder extends ViewHolder {
    public FooterViewHolder(View itemView) {
        super(itemView);
        itemView.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                // Do whatever you want on clicking the item
            }
        });
    }
}

// Now define the viewholder for Normal list item
public class NormalViewHolder extends ViewHolder {
    public NormalViewHolder(View itemView) {
        super(itemView);

        itemView.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                // Do whatever you want on clicking the normal items 
            }
        });
    }
}

// And now in onCreateViewHolder you have to pass the correct view
// while populating the list item.

@Override
public RecyclerView.ViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {

    View v;

    if (viewType == FOOTER_VIEW) {
        v = LayoutInflater.from(parent.getContext()).inflate(R.layout.list_item_footer, parent, false);

        FooterViewHolder vh = new FooterViewHolder(v);

        return vh;
    }

    v = LayoutInflater.from(parent.getContext()).inflate(R.layout.list_item_normal, parent, false);

    NormalViewHolder vh = new NormalViewHolder(v);

    return vh;
}

// Now bind the viewholders in onBindViewHolder
@Override
public void onBindViewHolder(RecyclerView.ViewHolder holder, int position) {

    try {
        if (holder instanceof NormalViewHolder) {
            NormalViewHolder vh = (NormalViewHolder) holder;

            vh.bindView(position);
        } else if (holder instanceof FooterViewHolder) {
            FooterViewHolder vh = (FooterViewHolder) holder;
        }
    } catch (Exception e) {
        e.printStackTrace();
    }
}

// Now the critical part. You have return the exact item count of your list
// I've only one footer. So I returned data.size() + 1
// If you've multiple headers and footers, you've to return total count
// like, headers.size() + data.size() + footers.size()

@Override
public int getItemCount() {
    if (data == null) {
        return 0;
    }

    if (data.size() == 0) {
        //Return 1 here to show nothing
        return 1;
    }

    // Add extra view to show the footer view
    return data.size() + 1;
}

// Now define getItemViewType of your own. 

@Override
public int getItemViewType(int position) {
    if (position == data.size()) {
        // This is where we'll add footer.
        return FOOTER_VIEW;
    }

    return super.getItemViewType(position);
}

// So you're done with adding a footer and its action on onClick. 
// Now set the default ViewHolder for NormalViewHolder

public class ViewHolder extends RecyclerView.ViewHolder {
    // Define elements of a row here
    public ViewHolder(View itemView) {
        super(itemView);
        // Find view by ID and initialize here
    }

    public void bindView(int position) {
        // bindView() method to implement actions
    }
}
}

```

[Here's a good read](https://plus.google.com/+WillBlaschko/posts/3MFmgPbQuWx) about the implementation of `RecyclerView` with header and footer.

**Alternate method:**

While the above answer will work you can use this approach as well using a  recycler view using a `NestedScrollView` .You can add a layout for header using the following approach:

```java
<android.support.v4.widget.NestedScrollView
        android:layout_width="match_parent"
        android:layout_height="match_parent">

     <RelativeLayout
        android:layout_width="match_parent"
        android:layout_height="match_parent">

        <include
            layout="@layout/drawer_view_header"
            android:id="@+id/navigation_header"/>

        <android.support.v7.widget.RecyclerView
            android:layout_below="@id/navigation_header"
            android:id="@+id/followers_list"
            android:layout_width="match_parent"
            android:layout_height="wrap_content"/>

    </RelativeLayout>
</android.support.v4.widget.NestedScrollView>

```

Or you may also use a `LinearLayout` with vertical alignment in your `NestedScrollView`.

**Note:** This will only work with `RecyclerView` above **23.2.0**

```java
compile 'com.android.support:recyclerview-v7:23.2.0'

```



## Show default view till items load or when data is not available


**Screenshot**<br>
[<img src="https://i.stack.imgur.com/bpv1l.png" alt="screenshot" />](https://i.stack.imgur.com/bpv1l.png)

**Adapter Class**

```java
private class MyAdapter extends RecyclerView.Adapter<RecyclerView.ViewHolder> {

final int EMPTY_VIEW = 77777;
List<CustomData> datalist = new ArrayList<>();

MyAdapter() {
    super();
}

@Override
public RecyclerView.ViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {

    LayoutInflater layoutInflater = LayoutInflater.from(parent.getContext());

    if (viewType == EMPTY_VIEW) {
        return new EmptyView(layoutInflater.inflate(R.layout.nothing_yet, parent, false));
    } else {
        return new ItemView(layoutInflater.inflate(R.layout.my_item, parent, false));
    }
}

@SuppressLint("SetTextI18n")
@Override
public void onBindViewHolder(final RecyclerView.ViewHolder holder, int position) {
    if (getItemViewType(position) == EMPTY_VIEW) {
        EmptyView emptyView = (EmptyView) holder;
        emptyView.primaryText.setText("No data yet");
        emptyView.secondaryText.setText("You're doing good !");
        emptyView.primaryText.setCompoundDrawablesWithIntrinsicBounds(null, new IconicsDrawable(getActivity()).icon(FontAwesome.Icon.faw_ticket).sizeDp(48).color(Color.DKGRAY), null, null);

    } else {
        ItemView itemView = (ItemView) holder;
        // Bind data to itemView
    }
}

@Override
public int getItemCount() {
    return datalist.size() > 0 ? datalist.size() : 1;
}

@Override
public int getItemViewType(int position) {
    if  datalist.size() == 0) {
        return EMPTY_VIEW;
    }
    return super.getItemViewType(position);
}

}

```

**nothing_yet.xml**

```java
<?xml version="1.0" encoding="utf-8"?>
<LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
    xmlns:tools="http://schemas.android.com/tools"
    android:layout_width="match_parent"
    android:layout_height="match_parent"
    android:layout_gravity="center"
    android:orientation="vertical"
    android:paddingBottom="100dp"
    android:paddingTop="100dp">

    <TextView
        android:id="@+id/nothingPrimary"
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:layout_gravity="center"
        android:drawableTint="@android:color/secondary_text_light"
        android:drawableTop="@drawable/ic_folder_open_black_24dp"
        android:enabled="false"
        android:fontFamily="sans-serif-light"
        android:text="No Item's Yet"
        android:textAppearance="?android:attr/textAppearanceLarge"
        android:textColor="@android:color/secondary_text_light"
        android:textSize="40sp"
        tools:targetApi="m" />

    <TextView
        android:id="@+id/nothingSecondary"
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:layout_gravity="center_horizontal"
        android:enabled="false"
        android:fontFamily="sans-serif-condensed"
        android:text="You're doing good !"
        android:textAppearance="?android:attr/textAppearanceSmall"
        android:textColor="@android:color/tertiary_text_light" />
</LinearLayout>

```

I'm using FontAwesome with Iconics Library for the images. Add this to your app level build.gradle file.

```java
compile 'com.mikepenz:fontawesome-typeface:4.6.0.3@aar'
compile 'com.mikepenz:iconics-core:2.8.1@aar'

```



## Endless Scrolling in Recycleview.


Here I have shared a code snippet for implementing endless scrolling in recycle view.

**Step 1:**
First make a one abstract method in Recycleview adapter like below.

```java
public abstract class ViewAllCategoryAdapter extends RecyclerView.Adapter<RecyclerView.ViewHolder> {
    public abstract void load();
}

```

**Step 2:**
Now override [onBindViewHolder](https://developer.android.com/reference/android/support/v7/widget/RecyclerView.Adapter.html) and `getItemCount()` method of ViewAllCategoryAdapter class and call `Load()` method like below.

```java
@Override
public void onBindViewHolder(RecyclerView.ViewHolder holder, final int position) {
    if ((position >= getItemCount() - 1)) {
        load();
    }
}

@Override
public int getItemCount() {
    return YOURLIST.size();
}

```

**Step 3:**
Now every backend logic is complete now it's time to execute this logic.It's simple you can override load method where you create object of your adapter.this method is automatically call while user reach at end of the listing.

```

 adapter = new ViewAllCategoryAdapter(CONTEXT, YOURLIST) {
        @Override
        public void load() {

            /* do your stuff here */
            /* This method is automatically call while user reach at end of your list. */
        }
    };
    recycleCategory.setAdapter(adapter);

```

Now `load()` method automatically call while user scroll at end of list.

**Best Luck**



## Add divider lines to RecyclerView items


Just add these lines to the initialization

```java
RecyclerView mRecyclerView = (RecyclerView) view.findViewById(recyclerView);
mRecyclerView.setLayoutManager(new LinearLayoutManager(getActivity()));
mRecyclerView.addItemDecoration(new DividerItemDecoration(getActivity(), DividerItemDecoration.VERTICAL));

```

Add an `adapter` and call `.notifyDataSetChanged();` as usual !
<br>This is not an inbuilt feature of Recyclerview but added in the support libraries. So don't forget to include this in your app level build.gradle file

```java
compile "com.android.support:appcompat-v7:25.3.1"
compile "com.android.support:recyclerview-v7:25.3.1"

```

> 
Multiple ItemDecorations can be added to a single RecyclerView.


**Changing divider color** :

It's pretty easy to set an color for a itemDecoration.

1. step is:  creating a `divider.xml` file which is located on `drawable` folder

```java
<?xml version="1.0" encoding="utf-8"?>
<shape xmlns:android="http://schemas.android.com/apk/res/android"
       android:shape="line">
    <size
        android:width="1px"
        android:height="1px"/>
    <solid android:color="@color/divider_color"/>
</shape>

```


1. step is: setting drawable

```

   // Get drawable object
    Drawable mDivider = ContextCompat.getDrawable(m_jContext, R.drawable.divider);
    // Create a DividerItemDecoration whose orientation is Horizontal
    DividerItemDecoration hItemDecoration = new DividerItemDecoration(m_jContext,
            DividerItemDecoration.HORIZONTAL);
    // Set the drawable on it
    hItemDecoration.setDrawable(mDivider);

```

[<img src="https://i.stack.imgur.com/eLytA.png" alt="horizontal_divider" />](https://i.stack.imgur.com/eLytA.png)

```

   // Create a DividerItemDecoration whose orientation is vertical
    DividerItemDecoration vItemDecoration = new DividerItemDecoration(m_jContext,
            DividerItemDecoration.VERTICAL);
    // Set the drawable on it
    vItemDecoration.setDrawable(mDivider);

```

[<img src="https://i.stack.imgur.com/pBOlO.png" alt="enter image description here" />](https://i.stack.imgur.com/pBOlO.png)



#### Parameters


|Parameter|Detail
|---|---|---|---|---|---|---|---|---|---
|Adapter|A subclass of RecyclerView.Adapter responsible for providing views that represent items in a data set
|Position|The position of a data item within an Adapter
|Index|The index of an attached child view as used in a call to getChildAt(int). Contrast with Position
|Binding|The process of preparing a child view to display data corresponding to a position within the adapter
|Recycle (view)|A view previously used to display data for a specific adapter position may be placed in a cache for later reuse to display the same type of data again later. This can drastically improve performance by skipping initial layout inflation or construction
|Scrap (view)|A child view that has entered into a temporarily detached state during layout. Scrap views may be reused without becoming fully detached from the parent RecyclerView, either unmodified if no rebinding is required or modified by the adapter if the view was considered dirty
|Dirty (view)|A child view that must be rebound by the adapter before being displayed



#### Remarks


The `RecyclerView` is a flexible view for providing a limited window into a large data set.

Before using the `RecyclerView` you have to add the support library dependency in the `build.gradle` file:

```java
dependencies {
    // Match the version of your support library dependency
    compile 'com.android.support:recyclerview-v7:25.3.1'
}

```

You can find latest version number of recyclerview from official [site](https://developer.android.com/topic/libraries/support-library/packages.html#v7-recyclerview).

### Other related topics:

There are other topics which describe the `RecyclerView` components:

- [RecyclerView LayoutManagers](http://stackoverflow.com/documentation/android/6772/recyclerview-and-layoutmanagers#t=20160915100748968774)
- [RecyclerView ItemDecorations](http://stackoverflow.com/documentation/android/506/recyclerview-decorations#t=201608060819528295736)
- [RecyclerView onClickListeners](http://stackoverflow.com/documentation/android/96/recyclerview-onclicklisteners#t=2016080608200447058)

### Official Documentation

[http://developer.android.com/reference/android/support/v7/widget/RecyclerView.html](http://developer.android.com/reference/android/support/v7/widget/RecyclerView.html)

### Older versions:

```

 //it requires compileSdkVersion 25
  compile 'com.android.support:recyclerview-v7:25.2.0'
  compile 'com.android.support:recyclerview-v7:25.1.0'
  compile 'com.android.support:recyclerview-v7:25.0.0'

  //it requires compileSdkVersion 24
  compile 'com.android.support:recyclerview-v7:24.2.1' 
  compile 'com.android.support:recyclerview-v7:24.2.0' 
  compile 'com.android.support:recyclerview-v7:24.1.1'  
  compile 'com.android.support:recyclerview-v7:24.1.0'  

  //it requires compileSdkVersion 23
  compile 'com.android.support:recyclerview-v7:23.4.0'
  compile 'com.android.support:recyclerview-v7:23.3.0'
  compile 'com.android.support:recyclerview-v7:23.2.1'
  compile 'com.android.support:recyclerview-v7:23.2.0'
  compile 'com.android.support:recyclerview-v7:23.1.1'
  compile 'com.android.support:recyclerview-v7:23.1.0'
  compile 'com.android.support:recyclerview-v7:23.0.1'
  compile 'com.android.support:recyclerview-v7:23.0.0'

  //it requires compileSdkVersion 22
  compile 'com.android.support:recyclerview-v7:22.2.1'
  compile 'com.android.support:recyclerview-v7:22.2.0'
  compile 'com.android.support:recyclerview-v7:22.1.1'
  compile 'com.android.support:recyclerview-v7:22.1.0'
  compile 'com.android.support:recyclerview-v7:22.0.0'

  //it requires compileSdkVersion 21
  compile 'com.android.support:recyclerview-v7:21.0.3'
  compile 'com.android.support:recyclerview-v7:21.0.2'
  compile 'com.android.support:recyclerview-v7:21.0.0'

```

