---
metaTitle: "Android - ViewPager"
description: "ViewPager with a dots indicator, Basic ViewPager usage with fragments, ViewPager with PreferenceFragment, Adding a ViewPager, ViewPager with TabLayout, Setup OnPageChangeListener"
---

# ViewPager


ViewPager is a Layout manager that allows the user to flip left and right through pages of data. It is most often used in conjunction with Fragment, which is a convenient way to supply and manage the lifecycle of each page.



## ViewPager with a dots indicator


[<img src="https://i.stack.imgur.com/RGULH.png" alt="ViewPager with a dots indicator" />](https://i.stack.imgur.com/RGULH.png)

All we need are: [ViewPager](https://developer.android.com/reference/android/support/v4/view/ViewPager.html), [TabLayout](https://developer.android.com/reference/android/support/design/widget/TabLayout.html) and 2 drawables for selected and default dots.

Firstly, we have to add `TabLayout` to our screen layout, and connect it with `ViewPager`. We can do this in two ways:

### Nested **TabLayout** in **ViewPager**

```java
<android.support.v4.view.ViewPager
    android:id="@+id/photos_viewpager"
    android:layout_width="match_parent"
    android:layout_height="match_parent">

    <android.support.design.widget.TabLayout
        android:layout_width="match_parent"
        android:layout_height="wrap_content"/>
</android.support.v4.view.ViewPager>

```

> 
In this case `TabLayout` will be automatically connected with `ViewPager`, but `TabLayout` will be next to `ViewPager`, not over him.


### Separate **TabLayout**

```java
<android.support.v4.view.ViewPager
    android:id="@+id/photos_viewpager"
    android:layout_width="match_parent"
    android:layout_height="match_parent"/>

<android.support.design.widget.TabLayout
    android:id="@+id/tab_layout"
    android:layout_width="match_parent"
    android:layout_height="wrap_content"/>

```

> 
In this case, we can put `TabLayout` anywhere, but we have to connect `TabLayout` with `ViewPager` programmatically


```java
ViewPager pager = (ViewPager) view.findViewById(R.id.photos_viewpager);
PagerAdapter adapter = new PhotosAdapter(getChildFragmentManager(), photosUrl);
pager.setAdapter(adapter);

TabLayout tabLayout = (TabLayout) view.findViewById(R.id.tab_layout);
tabLayout.setupWithViewPager(pager, true);

```

Once we created our layout, we have to prepare our dots. So we create three files: `selected_dot.xml`, `default_dot.xml` and `tab_selector.xml`.

### selected_dot.xml

```java
<?xml version="1.0" encoding="utf-8"?>
<layer-list xmlns:android="http://schemas.android.com/apk/res/android">
    <item>
        <shape
            android:innerRadius="0dp"
            android:shape="ring"
            android:thickness="8dp"
            android:useLevel="false">
            <solid android:color="@color/colorAccent"/>
        </shape>    
    </item>
</layer-list>

```

### default_dot.xml

```java
<?xml version="1.0" encoding="utf-8"?>
<layer-list xmlns:android="http://schemas.android.com/apk/res/android">
    <item>
        <shape
            android:innerRadius="0dp"
            android:shape="ring"
            android:thickness="8dp"
            android:useLevel="false">
            <solid android:color="@android:color/darker_gray"/>
        </shape>    
    </item>
</layer-list>

```

### tab_selector.xml

```java
<?xml version="1.0" encoding="utf-8"?>
<selector xmlns:android="http://schemas.android.com/apk/res/android">

    <item android:drawable="@drawable/selected_dot"
          android:state_selected="true"/>

    <item android:drawable="@drawable/default_dot"/>
</selector>

```

Now we need to add only 3 lines of code to `TabLayout` in our xml layout and you're done.

```java
app:tabBackground="@drawable/tab_selector"
app:tabGravity="center"
app:tabIndicatorHeight="0dp"

```



## Basic ViewPager usage with fragments


A `ViewPager` allows to show multiple fragments in an activity that can be navigated by either fliping left or right. A `ViewPager` needs to be feed of either Views or Fragments by using a `PagerAdapter`.

There are however two more specific implementations that you will find most useful in case of using Fragments which are `FragmentPagerAdapter` and `FragmentStatePagerAdapter`. When a Fragment needs to be instantiated for the first time, `getItem(position)` will be called for each position that needs instantiating. The `getCount()` method will return the total number of pages so the `ViewPager` knows how many Fragments need to be shown.

Both `FragmentPagerAdapter` and `FragmentStatePagerAdapter` keep a cache of the Fragments that the `ViewPager` will need to show. By default the `ViewPager` will try to store a maximum of 3 Fragments that correspond to the currently visible Fragment, and the ones next to the right and left. Also `FragmentStatePagerAdapter` will keep the state of each of your fragments.

Be aware that both implementations assume your fragments will keep their positions, so if you keep a list of the fragments instead of having a static number of them as you can see in the `getItem()` method, you will need to create a subclass of `PagerAdapter` and override at least `instantiateItem()`,`destroyItem()` and `getItemPosition()`methods.

Just add a ViewPager in your layout as described in the [basic example](http://stackoverflow.com/documentation/android/692/viewpager/19726/adding-a-viewpager#t=201609191634519860651):

```java
<?xml version="1.0" encoding="utf-8"?>
<LinearLayout> 
    <android.support.v4.view.ViewPager
        android:id="@+id/vpPager"> 
    </android.support.v4.view.ViewPager>
</LinearLayout>

```

Then define the adapter that will determine how many pages exist and which fragment to display for each page of the adapter.

```java
public class MyViewPagerActivity extends AppCompatActivity {
    private static final String TAG = MyViewPagerActivity.class.getName();

    private MyPagerAdapter mFragmentAdapter;
    private ViewPager mViewPager;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);    
        setContentView(R.layout.myActivityLayout);

        //Apply the Adapter
        mFragmentAdapter = new MyPagerAdapter(getSupportFragmentManager());    
        mViewPager = (ViewPager) findViewById(R.id.view_pager);
        mViewPager.setAdapter(mFragmentAdapter);
    }

    private class MyPagerAdapter extends FragmentPagerAdapter{

        public MyPagerAdapter(FragmentManager supportFragmentManager) {
            super(supportFragmentManager);
        }

        // Returns the fragment to display for that page
        @Override
        public Fragment getItem(int position) {
            switch(position) {
                case 0:
                    return new Fragment1();

                case 1:
                    return new Fragment2();

                case 2:
                    return new Fragment3();

                default:
                    return null;
            }
        }

        // Returns total number of pages    
        @Override
        public int getCount() {
            return 3;
        }

    }
}

```

If you are using `android.app.Fragment` you have to add this dependency:

```java
compile 'com.android.support:support-v13:25.3.1'

```

If you are using `android.support.v4.app.Fragment` you have to add this dependency:

```java
compile 'com.android.support:support-fragment:25.3.1'

```



## ViewPager with PreferenceFragment


Until recently, using `android.support.v4.app.FragmentPagerAdapter` would prevent the usage of a `PreferenceFragment` as one of the Fragments used in the FragmentPagerAdapter.

The latest versions of the support v7 library now include the [`PreferenceFragmentCompat`](http://developer.android.com/intl/es/reference/android/support/v7/preference/PreferenceFragmentCompat.html) class, which will work with a ViewPager and the v4 version of FragmentPagerAdapter.

Example Fragment that extends `PreferenceFragmentCompat`:

```java
import android.os.Bundle; 
import android.support.v7.preference.PreferenceFragmentCompat; 
import android.view.View; 
 
public class MySettingsPrefFragment extends PreferenceFragmentCompat { 
  
    public MySettingsPrefFragment() { 
        // Required empty public constructor 
    } 
 
    @Override 
    public void onCreate(Bundle savedInstanceState) { 
        super.onCreate(savedInstanceState); 
        addPreferencesFromResource(R.xml.fragment_settings_pref); 
    } 
 
    @Override 
    public void onCreatePreferences(Bundle bundle, String s) { 
 
    } 
}

```

You can now use this Fragment in a `android.support.v4.app.FragmentPagerAdapter` subclass:

```java
private class PagerAdapterWithSettings extends FragmentPagerAdapter {

    public PagerAdapterWithSettings(FragmentManager supportFragmentManager) {
        super(supportFragmentManager);
    }

    @Override
    public Fragment getItem(int position) {
        switch(position) {
            case 0:
                return new FragmentOne();

            case 1:
                return new FragmentTwo();

            case 2:
                return new MySettingsPrefFragment();

            default:
                return null;
        }
    }
    
    // .......

}

```



## Adding a ViewPager


Make sure the following dependency is added to your app's `build.gradle` file under dependencies:

```java
compile 'com.android.support:support-core-ui:25.3.0'

```

Then add the `ViewPager` to your activity layout:

```java
<android.support.v4.view.ViewPager
    android:id="@+id/viewpager"
    android:layout_width="match_parent"
    android:layout_height="match_parent"
    />

```

Then define your [`PagerAdapter`](https://developer.android.com/reference/android/support/v4/view/PagerAdapter.html):

```java
public class MyPagerAdapter extends PagerAdapter {

    private Context mContext;

    public CustomPagerAdapter(Context context) {
        mContext = context;
    }

    @Override
    public Object instantiateItem(ViewGroup collection, int position) {
 
        // Create the page for the given position. For example:
        LayoutInflater inflater = LayoutInflater.from(mContext);
        ViewGroup layout = (ViewGroup) inflater.inflate(R.layout.xxxx, collection, false);
        collection.addView(layout);
        return layout;
    }

    @Override
    public void destroyItem(ViewGroup collection, int position, Object view) {
        // Remove a page for the given position. For example:
        collection.removeView((View) view);
    }

    @Override
    public int getCount() {
        //Return the number of views available.
        return numberOfPages;
    }

    @Override
    public boolean isViewFromObject(View view, Object object) {
        // Determines whether a page View is associated with a specific key object
        // as returned by instantiateItem(ViewGroup, int). For example:
        return view == object;
    }    
}

```

Finally setup the `ViewPager` in your Activity:

```java
public class MainActivity extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        ViewPager viewPager = (ViewPager) findViewById(R.id.viewpager);
        viewPager.setAdapter(new MyPagerAdapter(this));
    }
}

```



## ViewPager with TabLayout


A [`TabLayout`](http://stackoverflow.com/documentation/android/124/material-design-for-all-android-versions/11500/adding-a-tablayout#t=20160919163836024731) can be used for easier navigation.<br />
You can set the tabs for each fragment in your adapter by using `TabLayout.newTab()` method but there is another more convenient and easier method for this task which is [`TabLayout.setupWithViewPager()`](https://developer.android.com/reference/android/support/design/widget/TabLayout.html#setupWithViewPager(android.support.v4.view.ViewPager)).

This method will sync by creating and removing tabs according to the contents of the adapter associated with your `ViewPager` each time you call it.<br />
Also, it will set a callback so each time the user flips the page, the corresponding tab will be selected.

Just define a layout

```java
<?xml version="1.0" encoding="utf-8"?>
<LinearLayout>

    <android.support.design.widget.TabLayout
        android:id="@+id/tabs"
        app:tabMode="scrollable" />

    <android.support.v4.view.ViewPager
        android:id="@+id/viewpager"
        android:layout_width="match_parent"
        android:layout_height="0px"
        android:layout_weight="1" />

</LinearLayout>

```

Then implement the `FragmentPagerAdapter` and apply it to the `ViewPager`:

```java
public class MyViewPagerActivity extends AppCompatActivity {
    private static final String TAG = MyViewPagerActivity.class.getName();

    private MyPagerAdapter mFragmentAdapter;
    private ViewPager mViewPager;
    private TabLayout mTabLayout;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);    
        setContentView(R.layout.myActivityLayout);

        // Get the ViewPager and apply the PagerAdapter
        mFragmentAdapter = new MyPagerAdapter(getSupportFragmentManager());    
        mViewPager = (ViewPager) findViewById(R.id.view_pager);
        mViewPager.setAdapter(mFragmentAdapter);

        // link the tabLayout and the viewpager together
        mTabLayout = (TabLayout) findViewById(R.id.tab_layout);
        mTabLayout.setupWithViewPager(mViewPager);
    }

    private class MyPagerAdapter extends FragmentPagerAdapter{

        public MyPagerAdapter(FragmentManager supportFragmentManager) {
            super(supportFragmentManager);
        }

         // Returns the fragment to display for that page
        @Override
        public Fragment getItem(int position) {
            switch(position) {
                case 0:
                    return new Fragment1();

                case 1:
                    return new Fragment2();

                case 2:
                    return new Fragment3();

                default:
                    return null;
            }
        }
        
        // Will be displayed as the tab's label
        @Override
        public CharSequence getPageTitle(int position) {
            switch(position) {
                case 0:
                    return "Fragment 1 title";

                case 1:
                    return "Fragment 2 title";

                case 2:
                    return "Fragment 3 title";

                default:
                    return null;
            }
        }

        // Returns total number of pages 
        @Override
        public int getCount() {
            return 3;
        }

    }
}

```



## Setup OnPageChangeListener


If you need to listen for changes to the page selected you can implement the [`ViewPager.OnPageChangeListener`](https://developer.android.com/reference/android/support/v4/view/ViewPager.OnPageChangeListener.html) listener on the ViewPager:

```java
viewPager.addOnPageChangeListener(new OnPageChangeListener() {
    
    // This method will be invoked when a new page becomes selected. Animation is not necessarily complete.
    @Override
    public void onPageSelected(int position) {
         // Your code
    }
    
    // This method will be invoked when the current page is scrolled, either as part of 
    // a programmatically initiated smooth scroll or a user initiated touch scroll.
    @Override
    public void onPageScrolled(int position, float positionOffset, int positionOffsetPixels) {
         // Your code
    }
    
    // Called when the scroll state changes. Useful for discovering when the user begins
    // dragging, when the pager is automatically settling to the current page, 
    // or when it is fully stopped/idle.
    @Override
    public void onPageScrollStateChanged(int state) {
         // Your code
    }
});

```



#### Remarks


One important thing to note about ViewPager usage is that there are two different versions of both `FragmentPagerAdapter` and `FragmentStatePagerAdapter`.

If you are using `android.app.Fragment` native Fragments with a FragmentPagerAdapter or FragmentStatePagerAdapter, you need to use the v13 support library versions of the adapter, i.e. [`android.support.v13.app.FragmentStatePagerAdapter`](https://developer.android.com/reference/android/support/v13/app/FragmentPagerAdapter.html).

If you are using `android.support.v4.app.Fragment` support library Fragments with a FragmentPagerAdapter or FragmentStatePagerAdapter, you need to use the v4 support library versions of the adapter, i.e. [`android.support.v4.app.FragmentStatePagerAdapter`](https://developer.android.com/reference/android/support/v4/app/FragmentStatePagerAdapter.html).

