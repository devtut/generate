---
metaTitle: "Android - MVVM (Architecture)"
description: "MVVM Example using DataBinding Library"
---

# MVVM (Architecture)



## MVVM Example using DataBinding Library


The whole point of MVVM is to separate layers containing logic from the view layer.

On Android we can use the [DataBinding Library](https://developer.android.com/topic/libraries/data-binding/index.html) to help us with this and make most of our logic Unit-testable without worrying about Android dependencies.

In this example I'll show the central components for a stupid simple App that does the following:

- At start up fake a network call and show a loading spinner
- Show a view with a click counter TextView, a message TextView, and a button to increment the counter
- On button click update counter and update counter color and message text if counter reaches some number

Let's start with the view layer:

`activity_main.xml`:

If you're unfamiliar with how DataBinding works you should probably [take 10 minutes](https://developer.android.com/topic/libraries/data-binding/index.html) to make yourself familiar with it.
As you can see, all fields you would usually update with setters are bound to functions on the viewModel variable.

If you've got a question about the `android:visibility` or `app:textColor` properties check the 'Remarks' section.

```

<layout
    xmlns:android="http://schemas.android.com/apk/res/android"
    xmlns:app="http://schemas.android.com/apk/res-auto"
    xmlns:tools="http://schemas.android.com/tools">

    <data>

        <import type="android.view.View" />

        <variable
            name="viewModel"
            type="de.walled.mvvmtest.viewmodel.ClickerViewModel"/>
    </data>

    <RelativeLayout
        android:id="@+id/activity_main"
        android:layout_width="match_parent"
        android:layout_height="match_parent"
        android:padding="@dimen/activity_horizontal_margin"

        tools:context="de.walled.mvvmtest.view.MainActivity">

        <LinearLayout
            android:id="@+id/click_counter"
            android:layout_width="wrap_content"
            android:layout_height="wrap_content"
            android:layout_centerHorizontal="true"
            android:layout_marginTop="60dp"
            android:visibility="@{viewModel.contentVisible ? View.VISIBLE : View.GONE}"

            android:padding="8dp"

            android:orientation="horizontal">

            <TextView
                android:id="@+id/number_of_clicks"
                android:layout_width="wrap_content"
                android:layout_height="wrap_content"
                style="@style/ClickCounter"

                android:text="@{viewModel.numberOfClicks}"
                android:textAlignment="center"
                app:textColor="@{viewModel.counterColor}"

                tools:text="8"
                tools:textColor="@color/red"
            />

            <TextView
                android:id="@+id/static_label"
                android:layout_width="wrap_content"
                android:layout_height="wrap_content"
                android:layout_marginLeft="4dp"
                android:layout_marginStart="4dp"
                style="@style/ClickCounter"

                android:text="@string/label.clicks"
                app:textColor="@{viewModel.counterColor}"
                android:textAlignment="center"

                tools:textColor="@color/red"
            />
        </LinearLayout>


        <TextView
            android:id="@+id/message"
            android:layout_width="wrap_content"
            android:layout_height="wrap_content"
            android:layout_below="@id/click_counter"
            android:layout_centerHorizontal="true"
            android:visibility="@{viewModel.contentVisible ? View.VISIBLE : View.GONE}"

            android:text="@{viewModel.labelText}"
            android:textAlignment="center"
            android:textSize="18sp"

            tools:text="You're bad and you should feel bad!"
        />

        <Button
            android:id="@+id/clicker"
            android:layout_width="wrap_content"
            android:layout_height="wrap_content"
            android:layout_below="@id/message"
            android:layout_centerHorizontal="true"
            android:layout_marginTop="8dp"
            android:visibility="@{viewModel.contentVisible ? View.VISIBLE : View.GONE}"

            android:padding="8dp"

            android:text="@string/label.button"

            android:onClick="@{() -> viewModel.onClickIncrement()}"
        />

        <android.support.v4.widget.ContentLoadingProgressBar
            android:layout_width="wrap_content"
            android:layout_height="wrap_content"
            android:layout_marginTop="90dp"
            android:layout_centerHorizontal="true"
            style="@android:style/Widget.ProgressBar.Inverse"
            android:visibility="@{viewModel.loadingVisible ? View.VISIBLE : View.GONE}"

            android:indeterminate="true"
        />

    </RelativeLayout>

</layout>

```

Next the model layer. Here I have:

- two fields that represent the state of the app
- getters to read the number of clicks and the state of excitement
- a method to increment my click count
- a method to restore some previous state (important for orientation changes)

Also I define here a 'state of excitement' that is dependent on the number of clicks. This will later be used to update color and message on the View.

It is important to note that there are no assumptions made in the model about how the state might be displayed to the user!

`ClickerModel.java`

```java
import com.google.common.base.Optional;

import de.walled.mvvmtest.viewmodel.ViewState;

public class ClickerModel implements IClickerModel {

    private int numberOfClicks;
    private Excitement stateOfExcitement;

    public void incrementClicks() {
        numberOfClicks += 1;
        updateStateOfExcitement();
    }

    public int getNumberOfClicks() {
        return Optional.fromNullable(numberOfClicks).or(0);
    }

    public Excitement getStateOfExcitement() {
        return Optional.fromNullable(stateOfExcitement).or(Excitement.BOO);
    }

    public void restoreState(ViewState state) {
        numberOfClicks = state.getNumberOfClicks();
        updateStateOfExcitement();
    }

    private void updateStateOfExcitement() {
        if (numberOfClicks < 10) {
            stateOfExcitement = Excitement.BOO;
        } else if (numberOfClicks <= 20) {
            stateOfExcitement = Excitement.MEH;
        } else {
            stateOfExcitement = Excitement.WOOHOO;
        }
    }
}

```

Next the ViewModel.

This will trigger changes on the model and format data from the model to show them on the view.
Note that it is here where we evaluate which GUI representation is appropriate for the state given by the model (`resolveCounterColor` and `resolveLabelText`). So we could for example easily implement an `UnderachieverClickerModel` that has lower thresholds for the state of excitement without touching any code in the viewModel or view.

Also note that the ViewModel does not hold any references to view objects. All properties are bound via the `@Bindable` annotations and updated when either `notifyChange()` (signals all properties need to be updated) or `notifyPropertyChanged(BR.propertyName)` (signals this properties need to be updated).

`ClickerViewModel.java`

```java
import android.databinding.BaseObservable;

import android.databinding.Bindable;
import android.support.annotation.ColorRes;
import android.support.annotation.StringRes;
    
import com.android.databinding.library.baseAdapters.BR;
    
import de.walled.mvvmtest.R;
import de.walled.mvvmtest.api.IClickerApi;
import de.walled.mvvmtest.model.Excitement;
import de.walled.mvvmtest.model.IClickerModel;
import rx.Observable;

public class ClickerViewModel extends BaseObservable {

    private final IClickerApi api;
    boolean isLoading = false;
    private IClickerModel model;

    public ClickerViewModel(IClickerModel model, IClickerApi api) {
        this.model = model;
        this.api = api;
    }

    public void onClickIncrement() {
        model.incrementClicks();
        notifyChange();
    }

    public ViewState getViewState() {
        ViewState viewState = new ViewState();
        viewState.setNumberOfClicks(model.getNumberOfClicks());
        return viewState;
    }

    public Observable<ViewState> loadData() {
        isLoading = true;
        return api.fetchInitialState()
                .doOnNext(this::initModel)
                .doOnTerminate(() -> {
                    isLoading = false;
                    notifyPropertyChanged(BR.loadingVisible);
                    notifyPropertyChanged(BR.contentVisible);
                });
    }

    public void initFromSavedState(ViewState savedState) {
        initModel(savedState);
    }

    @Bindable
    public String getNumberOfClicks() {
        final int clicks = model.getNumberOfClicks();
        return String.valueOf(clicks);
    }

    @Bindable
    @StringRes
    public int getLabelText() {
        final Excitement stateOfExcitement = model.getStateOfExcitement();
        return resolveLabelText(stateOfExcitement);
    }

    @Bindable
    @ColorRes
    public int getCounterColor() {
        final Excitement stateOfExcitement = model.getStateOfExcitement();
        return resolveCounterColor(stateOfExcitement);
    }

    @Bindable
    public boolean isLoadingVisible() {
        return isLoading;
    }

    @Bindable
    public boolean isContentVisible() {
        return !isLoading;
    }

    private void initModel(final ViewState viewState) {
        model.restoreState(viewState);
        notifyChange();
    }

    @ColorRes
    private int resolveCounterColor(Excitement stateOfExcitement) {
        switch (stateOfExcitement) {
            case MEH:
                return R.color.yellow;
            case WOOHOO:
                return R.color.green;
            default:
                return R.color.red;
        }
    }

    @StringRes
    private int resolveLabelText(Excitement stateOfExcitement) {
        switch (stateOfExcitement) {
            case MEH:
                return R.string.label_indifferent;
            case WOOHOO:
                return R.string.label_excited;
            default:
                return R.string.label_negative;
        }
    }

}

```

Tying it all together in the Activity!

Here we see the view initializing the viewModel with all dependencies it might need, that have to be instantiated from an android context.

After the viewModel is initialized it is bound to the xml layout via the DataBindingUtil (Please check 'Syntax' section for naming of generated classes).

Note subscriptions are subscribed to on this layer because we have to handle unsubscribing them when the activity is paused or destroyed to avoid memory leaks and NPEs. Also persisting and reloading of the viewState on OrientationChanges is triggered here

`MainActivity.java`

```java
import android.databinding.DataBindingUtil;
import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;

import de.walled.mvvmtest.R;
import de.walled.mvvmtest.api.ClickerApi;
import de.walled.mvvmtest.api.IClickerApi;
import de.walled.mvvmtest.databinding.ActivityMainBinding;
import de.walled.mvvmtest.model.ClickerModel;
import de.walled.mvvmtest.viewmodel.ClickerViewModel;
import de.walled.mvvmtest.viewmodel.ViewState;
import rx.Subscription;
import rx.subscriptions.Subscriptions;

public class MainActivity extends AppCompatActivity {

    private static final String KEY_VIEW_STATE = "state.view";

    private ClickerViewModel viewModel;
    private Subscription fakeLoader = Subscriptions.unsubscribed();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        // would usually be injected but I feel Dagger would be out of scope
        final IClickerApi api = new ClickerApi();
        setupViewModel(savedInstanceState, api);

        ActivityMainBinding binding = DataBindingUtil.setContentView(this, R.layout.activity_main);
        binding.setViewModel(viewModel);
    }

    @Override
    protected void onPause() {
        fakeLoader.unsubscribe();
        super.onPause();
    }

    @Override
    protected void onDestroy() {
        fakeLoader.unsubscribe();
        super.onDestroy();
    }

    @Override
    protected void onSaveInstanceState(Bundle outState) {
        outState.putSerializable(KEY_VIEW_STATE, viewModel.getViewState());
    }

    private void setupViewModel(Bundle savedInstance, IClickerApi api) {
        viewModel = new ClickerViewModel(new ClickerModel(), api);
        final ViewState savedState = getViewStateFromBundle(savedInstance);

        if (savedState == null) {
            fakeLoader = viewModel.loadData().subscribe();
        } else {
            viewModel.initFromSavedState(savedState);
        }
    }

    private ViewState getViewStateFromBundle(Bundle savedInstance) {
        if (savedInstance != null) {
            return (ViewState) savedInstance.getSerializable(KEY_VIEW_STATE);
        }
        return null;
    }
}

```

To see everything in action check out this [example project](https://github.com/danielw93/MVVM-Example).



#### Remarks


**Syntax quirks with DataBinding**

When binding a viewModel function to a property in xml certain function prefixes like `get` or `is` are dropped. Eg. `ViewModel::getFormattedText` on the ViewModel will become `@{viewModel.formattedText}` when binding it to a property in xml. Similarly with `ViewModel::isContentVisible` -> `@{viewModel.contentVisible}` (Java Bean notation)

The generated binding classes like `ActivityMainBinding` are named after the xml they're creating bindings for, not the java class.

**Custom Bindings**

In the activity_main.xml I set the textColor attribute on the `app` and not the `android` namespace. Why is that? Because there is a custom setter defined for the attribute `textColor` that resolves a ColorRes resource id send out by the ViewModel to an actual color.

```java
public class CustomBindings {

    @TargetApi(23)
    @BindingAdapter({"bind:textColor"})
    public static void setTextColor(TextView textView, int colorResId) {
        final Context context = textView.getContext();
        final Resources resources = context.getResources();
        final int apiVersion = Build.VERSION.SDK_INT;
        int color;

        if (apiVersion >= Build.VERSION_CODES.M) {
           color = resources.getColor(colorResId, context.getTheme());
        } else {
            color = resources.getColor(colorResId);
        }

        textView.setTextColor(color);
    }

}

```

For details on how this works check [DataBinding Library: Custom Setters](https://developer.android.com/topic/libraries/data-binding/index.html#custom_setters)

**Wait...there is logic in your xml!!!?**

You could argue that the things I do in xml for `android:visibility` and `app:textColor` are wrong/anti-patterns in the MVVM context because there is view logic in my view.
However I would argue it is more important for me to keep android dependencies out of my ViewModel for testing reasons.

Besides, what really does `app:textColor` do? It only resolves a ressource pointer to the actual color associated with it. So the ViewModel still decides which color is shown based on some condition.

As for the `android:visibility` I feel because of how the method is named it is actually okay to use the ternary operator here. Because of the name `isLoadingVisible` and `isContentVisible` there is really no doubt about what each outcome should resolve to in the view. So I feel it is rather executing a command given by the ViewModel than really doing view logic.

On the other hand I would agree that using `viewModel.isLoading ? View.VISIBLE : View.GONE` would be a bad thing to do because you are making assumptions in the view what that state means for the view.

**Useful Material**

The following resources have helped me a lot in trying to understand this concept:

- Jeremy Likness - [Model-View-ViewModel (MVVM) Explained](http://www.codeproject.com/Articles/100175/Model-View-ViewModel-MVVM-Explained) (C#) (08.2010)
- Shamlia Shukkur - [Understanding the basics of MVVM design pattern](https://blogs.msdn.microsoft.com/msgulfcommunity/2013/03/13/understanding-the-basics-of-mvvm-design-pattern/) (C#) (03.2013)
- Frode Nilsen - [Android Databinding: Goodbye Presenter, Hello ViewModel!](http://tech.vg.no/2015/07/17/android-databinding-goodbye-presenter-hello-viewmodel/) (07.2015)
- Joe Birch - [Approaching Android with MVVM](https://labs.ribot.co.uk/approaching-android-with-mvvm-8ceec02d5442#.oz0arkgkx) (09.2015)
- Florina Muntenescu - [Android Architecture Patterns Part 3: Model-View-ViewModel](http://upday.github.io/blog/model-view-viewmodel/) (10.2016)

