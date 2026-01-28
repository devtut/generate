---
metaTitle: "Android - RecyclerView Decorations"
description: "Drawing a Separator, Add divider to RecyclerView, How to add dividers using and DividerItemDecoration, Per-item margins with ItemDecoration, ItemOffsetDecoration for GridLayoutManager in RecycleView"
---

# RecyclerView Decorations



## Drawing a Separator


This will draw a line at the bottom of every view but the last to act as a separator between items.

```java
public class SeparatorDecoration extends RecyclerView.ItemDecoration {

    private final Paint mPaint;
    private final int mAlpha;

    public SeparatorDecoration(@ColorInt int color, float width) {
        mPaint = new Paint();
        mPaint.setColor(color);
        mPaint.setStrokeWidth(width);
        mAlpha = mPaint.getAlpha();
    }

    @Override
    public void getItemOffsets(Rect outRect, View view, RecyclerView parent, RecyclerView.State state) {
        final RecyclerView.LayoutParams params = (RecyclerView.LayoutParams) view.getLayoutParams();

        // we retrieve the position in the list
        final int position = params.getViewAdapterPosition();

        // add space for the separator to the bottom of every view but the last one
        if (position < state.getItemCount()) {
            outRect.set(0, 0, 0, (int) mPaint.getStrokeWidth()); // left, top, right, bottom
        } else {
            outRect.setEmpty(); // 0, 0, 0, 0
        }
    }

    @Override
    public void onDraw(Canvas c, RecyclerView parent, RecyclerView.State state) {
        // a line will draw half its size to top and bottom,
        // hence the offset to place it correctly
        final int offset = (int) (mPaint.getStrokeWidth() / 2);

        // this will iterate over every visible view
        for (int i = 0; i < parent.getChildCount(); i++) {
            final View view = parent.getChildAt(i);
            final RecyclerView.LayoutParams params = (RecyclerView.LayoutParams) view.getLayoutParams();

            // get the position
            final int position = params.getViewAdapterPosition();

            // and finally draw the separator
            if (position < state.getItemCount()) {
                // apply alpha to support animations
                mPaint.setAlpha((int) (view.getAlpha() * mAlpha));

                float positionY = view.getBottom() + offset + view.getTranslationY();
                // do the drawing
                c.drawLine(view.getLeft() + view.getTranslationX(),
                        positionY,
                        view.getRight() + view.getTranslationX(),
                        positionY,
                        mPaint);
            }
        }
    }
}

```



## Add divider to RecyclerView


First of all you need to create a class which extends `RecyclerView.ItemDecoration` :

```java
public class SimpleBlueDivider extends RecyclerView.ItemDecoration {
private Drawable mDivider;

public SimpleBlueDivider(Context context) {
    mDivider = context.getResources().getDrawable(R.drawable.divider_blue);
}

@Override
public void onDrawOver(Canvas c, RecyclerView parent, RecyclerView.State state) {
    //divider padding give some padding whatever u want or disable
    int left =parent.getPaddingLeft()+80;
    int right = parent.getWidth() - parent.getPaddingRight()-30;

    int childCount = parent.getChildCount();
    for (int i = 0; i < childCount; i++) {
        View child = parent.getChildAt(i);

        RecyclerView.LayoutParams params = (RecyclerView.LayoutParams) child.getLayoutParams();

        int top = child.getBottom() + params.bottomMargin;
        int bottom = top + mDivider.getIntrinsicHeight();

        mDivider.setBounds(left, top, right, bottom);
        mDivider.draw(c);
    }
}

}

```

Add `divider_blue.xml` to your drawable folder :

```java
<?xml version="1.0" encoding="utf-8"?>
<shape xmlns:android="http://schemas.android.com/apk/res/android" android:shape="rectangle">
<size android:width="1dp" android:height="4dp" />
<solid android:color="#AA123456" />
</shape>

```

Then use it like :

```java
recyclerView.addItemDecoration(new SimpleBlueDivider(context));

```

The result will be like :

[<img src="https://i.stack.imgur.com/j70CI.png" alt="enter image description here" />](https://i.stack.imgur.com/j70CI.png)

This image is just an example how dividers working , if you want to follow Material Design specs when adding dividers please take a look at this link : [dividers](https://material.google.com/components/dividers.html) and thanks  [@Brenden Kromhout](http://stackoverflow.com/users/2263250/brenden-kromhout) by providing link .



## How to add dividers using and DividerItemDecoration


The [`DividerItemDecoration`](https://developer.android.com/reference/android/support/v7/widget/DividerItemDecoration.html) is a `RecyclerView.ItemDecoration` that can be used as a divider between items.

```java
DividerItemDecoration mDividerItemDecoration = new DividerItemDecoration(context,
             mLayoutManager.getOrientation());
recyclerView.addItemDecoration(mDividerItemDecoration);

```

It supports both orientation using `DividerItemDecoration.VERTICAL` and `DividerItemDecoration.HORIZONTAL`.



## Per-item margins with ItemDecoration


You can use a `RecyclerView.ItemDecoration` to put extra margins around each item in a RecyclerView. This can in some cases clean up both your adapter implementation and your item view XML.

```java
public class MyItemDecoration
    extends RecyclerView.ItemDecoration {

    private final int extraMargin;

    @Override
    public void getItemOffsets(Rect outRect, View view,
            RecyclerView parent, RecyclerView.State state) {

        int position = parent.getChildAdapterPosition(view);

        // It's easy to put extra margin on the last item...
        if (position + 1 == parent.getAdapter().getItemCount()) {
            outRect.bottom = extraMargin; // unit is px
        }

        // ...or you could give each item in the RecyclerView different
        // margins based on its position...
        if (position % 2 == 0) {
            outRect.right = extraMargin;
        } else {
            outRect.left = extraMargin;
        }

        // ...or based on some property of the item itself
        MyListItem item = parent.getAdapter().getItem(position);
        if (item.isFirstItemInSection()) {
            outRect.top = extraMargin;
        }
    }

    public MyItemDecoration(Context context) {
        extraMargin = context.getResources()
                .getDimensionPixelOffset(R.dimen.extra_margin);
    }
}

```

To enable the decoration, simply add it to your RecyclerView:

```java
// in your onCreate()
RecyclerView rv = (RecyclerView) findItemById(R.id.myList);
rv.addItemDecoration(new MyItemDecoration(context));

```



## ItemOffsetDecoration for GridLayoutManager in RecycleView


Following example will help to give equal space to an item in GridLayout.

**ItemOffsetDecoration.java**

```java
public class ItemOffsetDecoration extends RecyclerView.ItemDecoration {

    private int mItemOffset;

    private int spanCount = 2;

    public ItemOffsetDecoration(int itemOffset) {
        mItemOffset = itemOffset;
    }

    public ItemOffsetDecoration(@NonNull Context context, @DimenRes int itemOffsetId) {
        this(context.getResources().getDimensionPixelSize(itemOffsetId));
    }

    @Override
    public void getItemOffsets(Rect outRect, View view, RecyclerView parent,
                               RecyclerView.State state) {
        super.getItemOffsets(outRect, view, parent, state);

        int position = parent.getChildLayoutPosition(view);

        GridLayoutManager manager = (GridLayoutManager) parent.getLayoutManager();

        if (position < manager.getSpanCount())
            outRect.top = mItemOffset;

        if (position % 2 != 0) {
            outRect.right = mItemOffset;
        }

        outRect.left = mItemOffset;
        outRect.bottom = mItemOffset;
    }
}

```

You can call ItemDecoration like below code.

```java
recyclerView = (RecyclerView) view.findViewById(R.id.recycler_view);

GridLayoutManager lLayout = new GridLayoutManager(getActivity(), 2);

ItemOffsetDecoration itemDecoration = new ItemOffsetDecoration(mActivity, R.dimen.item_offset);
recyclerView.addItemDecoration(itemDecoration);

recyclerView.setLayoutManager(lLayout);

```

> 
and example item offset

```java
<dimen name="item_offset">5dp</dimen>

```






#### Syntax


- [RecyclerView addItemDecoration(RecyclerView.ItemDecoration decoration)](https://developer.android.com/reference/android/support/v7/widget/RecyclerView.html#addItemDecoration(android.support.v7.widget.RecyclerView.ItemDecoration))
- [RecyclerView addItemDecoration(RecyclerView.ItemDecoration decoration, int index)](https://developer.android.com/reference/android/support/v7/widget/RecyclerView.html#addItemDecoration(android.support.v7.widget.RecyclerView.ItemDecoration,%20int))



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|decoration|the item decoration to add to the `RecyclerView`
|index|the index in the list of decorations for this `RecyclerView`. This is the order in which `getItemOffset` and `onDraw` are called. Later calls might overdraw previous ones.



#### Remarks


### Decorations are static

Since decorations are only drawn, it is not possible to add click listeners or other UI functionality to them.

### Multiple decorations

Adding multiple decorations to a `RecyclerView`  will work in some cases, but there is currently no public API to take other possible decorations into account when measuring or drawing. You can get the view bounds or the view decorated bounds, where the decorated bounds are the sum of all the decoration offsets applied.

### Other related topics:

[RecyclerView](http://stackoverflow.com/documentation/android/169/recyclerview#t=201608172130474779815)<br />
[RecyclerView onClickListeners](http://stackoverflow.com/documentation/android/96/recyclerview-onclicklisteners#t=2016080608200447058)

### Official javadoc

[https://developer.android.com/reference/android/support/v7/widget/RecyclerView.ItemDecoration.html](https://developer.android.com/reference/android/support/v7/widget/RecyclerView.ItemDecoration.html)

