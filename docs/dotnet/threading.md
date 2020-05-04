---
metaTitle: "Threading"
description: "Accessing form controls from other threads"
---

# Threading



## Accessing form controls from other threads


If you want to change an attribute of a control such as a textbox or label from another thread than the GUI thread that created the control, you will have to invoke it or else you might get an error message stating:

> 
"Cross-thread operation not valid: Control 'control_name' accessed from a thread other than the thread it was created on."


Using this example code on a system.windows.forms form will cast an exception with that message:

```dotnet
private void button4_Click(object sender, EventArgs e)
{
    Thread thread = new Thread(updatetextbox);
    thread.Start();
}

private void updatetextbox()
{
    textBox1.Text = "updated"; // Throws exception
}

```

Instead when you want to change a textbox's text from within a thread that doesn't own it use Control.Invoke or Control.BeginInvoke. You can also use Control.InvokeRequired to check if invoking the control is necessary.

```dotnet
private void updatetextbox()
{
    if (textBox1.InvokeRequired)
        textBox1.BeginInvoke((Action)(() => textBox1.Text = "updated"));
    else
        textBox1.Text = "updated";
}

```

If you need to do this often, you can write an extension for invokeable objects to reduce the amount of code necessary to make this check:

```dotnet
public static class Extensions
{
    public static void BeginInvokeIfRequired(this ISynchronizeInvoke obj, Action action)
    {
        if (obj.InvokeRequired)
            obj.BeginInvoke(action, new object[0]);
        else
            action();
    }
}

```

And updating the textbox from any thread becomes a bit simpler:

```dotnet
private void updatetextbox()
{
    textBox1.BeginInvokeIfRequired(() => textBox1.Text = "updated");
}

```

Be aware that Control.BeginInvoke as used in this example is asynchronous, meaning that code coming after a call to Control.BeginInvoke can be run immedeately after, whether or not the passed delegate has been executed yet.

If you need to be sure that textBox1 is updated before continuing, use Control.Invoke instead, which will block the calling thread until your delegate has been executed. Do note that this approach can slow your code down significantly if you make many invoke calls and note that it will deadlock your application if your GUI thread is waiting for the calling thread to complete or release a held resource.

