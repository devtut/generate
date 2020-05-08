---
metaTitle: "Android - Create Singleton Class for Toast Message"
description: "Create own singleton class for toast massages"
---

# Create Singleton Class for Toast Message


Toast messages are the most simple way of providing feedback to the user. By default, Android provide gray color message toast where we can set the message and the duration of the message. If we need to create more customizable and reusable toast message, we can implement it by ourselves with the use of a custom layout. More importantly when we are implementing it, the use of Singelton design pattern will make it easy for maintaining and development of the custom toast message class.



## Create own singleton class for toast massages


Here is how to create your own singleton class for toast messages,
If your application need to show success, warning and the danger messages for different use cases you can use this class after you have modified it to your own specifications.

```

   public class ToastGenerate {
        private static ToastGenerate ourInstance;

        public ToastGenerate (Context context) {
            this.context = context;
                                                }
        public static ToastGenerate getInstance(Context context) {
            if (ourInstance == null)
                ourInstance = new ToastGenerate(context);
            return ourInstance;
        }
    
       //pass message and message type to this method
        public void createToastMessage(String message,int type){
       
//inflate the custom layout 
            LayoutInflater layoutInflater = (LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
    
            LinearLayout toastLayout = (LinearLayout) layoutInflater.inflate(R.layout.layout_custome_toast,null);
            TextView toastShowMessage = (TextView) toastLayout.findViewById(R.id.textCustomToastTopic);
    
            switch (type){
                case 0:
                    //if the message type is 0 fail toaster method will call
                    createFailToast(toastLayout,toastShowMessage,message);
                    break;
                case 1:
                    //if the message type is 1 success toaster method will call
                    createSuccessToast(toastLayout,toastShowMessage,message);
                    break;
    
                case 2:
                    createWarningToast( toastLayout, toastShowMessage, message);
                    //if the message type is 2 warning toaster method will call
                    break;
                default:
                    createFailToast(toastLayout,toastShowMessage,message);
    
            }
        }
    
    //Failure toast message method
        private final void createFailToast(LinearLayout toastLayout,TextView toastMessage,String message){
            toastLayout.setBackgroundColor(context.getResources().getColor(R.color.button_alert_normal));
            toastMessage.setText(message);
            toastMessage.setTextColor(context.getResources().getColor(R.color.white));
            showToast(context,toastLayout);
        }
    
        //warning toast message method
        private final void createWarningToast( LinearLayout toastLayout, TextView toastMessage, String message) {
            toastLayout.setBackgroundColor(context.getResources().getColor(R.color.warning_toast));
            toastMessage.setText(message);
            toastMessage.setTextColor(context.getResources().getColor(R.color.white));
            showToast(context, toastLayout);
        }
    //success toast message method
        private final void createSuccessToast(LinearLayout toastLayout,TextView toastMessage,String message){
            toastLayout.setBackgroundColor(context.getResources().getColor(R.color.success_toast));
    
            toastMessage.setText(message);
            toastMessage.setTextColor(context.getResources().getColor(R.color.white));
            showToast(context,toastLayout);
        }
    
        private void showToast(View view){
            Toast toast = new Toast(context);
            toast.setGravity(Gravity.TOP,0,0); // show message in the top of the device
            toast.setDuration(Toast.LENGTH_SHORT);
            toast.setView(view);
            toast.show();
        }
    }

```



#### Syntax


- Toast Toast(Context contex)
- void setDuration(int duration)
- void setGravity(int gravity, int xOffset, int yOffset)
- void setView(View view)
- void show()



#### Parameters


|Parameter|details
|---|---|---|---|---|---|---|---|---|---
|context|Relevant context which needs to display your toast message. If you use this in the activity pass "this" keyword or If you use in fragement pass as "getActivity()".
|view|Create a custom view and pass that view object to this.
|gravity|Pass the gravity position of the toaster. All the position has added under the Gravity class as the static variables . The Most common positions are Gravity.TOP, Gravity.BOTTOM, Gravity.LEFT, Gravity.RIGHT.
|xOffset|Horizontal offset of the toast message.
|yOffset|Vertical offset of the toast message.
|duration|Duration of the toast show. We can set either Toast.LENGTH_SHORT or Toast.LENGTH_LONG



#### Remarks


Toast message is a simple way of providing feedback to user about something is happening. If you need a more advanced way to give feedback you can use dialogs or snackbar.

To get more details about the toast message please check this documentation.
[https://developer.android.com/reference/android/widget/Toast.html](https://developer.android.com/reference/android/widget/Toast.html)

