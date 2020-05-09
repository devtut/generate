---
metaTitle: "Xamarin - Working with local databases"
description: "Using SQLite.NET in a Shared Project, Working with local databases using xamarin.forms in visual studio 2015"
---

# Working with local databases



## Using SQLite.NET in a Shared Project


[SQLite.NET](https://github.com/praeclarum/sqlite-net) is an open source library which makes it possible to add local-databases support using `SQLite` version 3 in a `Xamarin.Forms` project.

The steps below demonstrate how to include this component in a `Xamarin.Forms` Shared Project:

<li>
<p>Download the latest version of the [SQLite.cs](https://github.com/praeclarum/sqlite-net/blob/master/src/SQLite.cs) class and add it
to the Shared Project.</p>
</li>
<li>
Every table that will be included in the database needs to be modeled as a class in the Shared Project. A table is defined by adding at least two attributes in the class: `Table` (for the class) and `PrimaryKey` (for a property).
</li>

For this example, a new class named `Song` is added to the Shared Project, defined as follows:

```cs
using System;
using SQLite;

namespace SongsApp
{
    [Table("Song")]
    public class Song
    {
        [PrimaryKey]
        public string ID { get; set; }
        public string SongName { get; set; }
        public string SingerName { get; set; }
    }
}

```


1. Next, add a new class called `Database`, which inherits from the `SQLiteConnection` class (included in SQLite.cs). In this new class, the code for database access, tables creation and CRUD operations for each table is defined. Sample code is shown below:

```cs
using System;
using System.Linq;
using System.Collections.Generic;
using SQLite;

namespace SongsApp
{
    public class BaseDatos : SQLiteConnection
    {
        public BaseDatos(string path) : base(path)
        {
            Initialize();
        }

        void Initialize()
        {
            CreateTable<Song>();
        }

        public List<Song> GetSongs()
        {
            return Table<Song>().ToList();
        }

        public Song GetSong(string id)
        {
            return Table<Song>().Where(t => t.ID == id).First();
        }

        public bool AddSong(Song song)
        {
            Insert(song);
        }

        public bool UpdateSong(Song song)
        {
            Update(song);
        }

        public void DeleteSong(Song song)
        {
            Delete(song);
        }
    }
}

```


1. As you could see in the previous step, the constructor of our `Database` class includes a `path` parameter, which represents the location of the file that stores the SQLite database file. A static `Database` object can be declared in `App.cs`. The `path` is platform-specific:

```cs
public class App : Application
{
    public static Database DB;

    public App ()
    {
        string dbFile = "SongsDB.db3";

#if __ANDROID__
        string docPath = Environment.GetFolderPath(Environment.SpecialFolder.Personal);
        var dbPath = System.IO.Path.Combine(docPath, dbFile);
#else
#if __IOS__
        string docPath = Environment.GetFolderPath(Environment.SpecialFolder.Personal);
        string libPath = System.IO.Path.Combine(docPath, "..", "Library");
        var dbPath = System.IO.Path.Combine(libPath, dbFile);
#else
        var dbPath = System.IO.Path.Combine(ApplicationData.Current.LocalFolder.Path, dbFile);
#endif
#endif

        DB = new Database(dbPath);

        // The root page of your application
        MainPage = new SongsPage();
    }
}

```


1. Now simply call the `DB` object through the `App` class anytime you need to perform a CRUD operation to the `Songs` table. For example, to insert a new `Song` after the user has clicked a button, you can use the following code:

```cs
void AddNewSongButton_Click(object sender, EventArgs a)
{
    Song s = new Song();
    s.ID = Guid.NewGuid().ToString();
    s.SongName = songNameEntry.Text;
    s.SingerName = singerNameEntry.Text;

    App.DB.AddSong(song);
}

```



## Working with local databases using xamarin.forms in visual studio 2015


**SQlite example Step by step Explanation**

<li>
<p>The steps below demonstrate how to include this component in a Xamarin.Forms Shared Project:
to add packages in (pcl,Andriod,Windows,Ios)
Add References Click on **Manage Nuget packages** ->click on Browse
to install **SQLite.Net.Core-PCL** , **SQLite Net Extensions** after installation is completed check it once in references then</p>
</li>
<li>
To add Class **Employee.cs** below code

```cs
 using SQLite.Net.Attributes;

     namespace DatabaseEmployeeCreation.SqlLite
     {
         public   class Employee
         {
             [PrimaryKey,AutoIncrement]
             public int Eid { get; set; }
             public string Ename { get; set; }
             public string Address { get; set; }
             public string phonenumber { get; set; }
             public string email { get; set; } 
         }
     }

```


</li>
<li>
To add one interface ISQLite
</li>

```

using SQLite.Net;  
            //using SQLite.Net;
            namespace DatabaseEmployeeCreation.SqlLite.ViewModel
            {
                public interface ISQLite
                {
                    SQLiteConnection GetConnection();
                }
            }

```


1. Create a one class for database logics and methods below code is follow .

using SQLite.Net;
using System.Collections.Generic;
using System.Linq;
using Xamarin.Forms;
namespace DatabaseEmployeeCreation.SqlLite.ViewModel
{
public  class DatabaseLogic
{
static object locker = new object();
SQLiteConnection database;

```

   public DatabaseLogic()
    {
        database = DependencyService.Get<ISQLite>().GetConnection();
        // create the tables
        database.CreateTable<Employee>();
    }

    public IEnumerable<Employee> GetItems()
    {
        lock (locker)
        {
            return (from i in database.Table<Employee>() select i).ToList();
        }
    }

    public IEnumerable<Employee> GetItemsNotDone()
    {
        lock (locker)
        {
            return database.Query<Employee>("SELECT * FROM [Employee]");
        }
    }

    public Employee GetItem(int id)
    {
        lock (locker)
        {
            return database.Table<Employee>().FirstOrDefault(x => x.Eid == id);
        }
    }

    public int SaveItem(Employee item)
    {
        lock (locker)
        {
            if (item.Eid != 0)
            {
                database.Update(item);
                return item.Eid;
            }
            else
            {
                return database.Insert(item);
            }
        }
    }

    public int DeleteItem(int Eid)
    {
        lock (locker)
        {
            return database.Delete<Employee>(Eid);
        }
    }
}

```

}

1. to Create a xaml.forms EmployeeRegistration.xaml

```

   <?xml version="1.0" encoding="utf-8" ?>
    <ContentPage xmlns="http://xamarin.com/schemas/2014/forms"
                 xmlns:x="http://schemas.microsoft.com/winfx/2009/xaml"
                 x:Class="DatabaseEmployeeCreation.SqlLite.EmployeeRegistration"
      Title="{Binding Name}" >
      <StackLayout VerticalOptions="StartAndExpand" Padding="20">
    
        <Label Text="Ename" />
        <Entry x:Name="nameEntry" Text="{Binding Ename}"/>
        <Label Text="Address" />
        <Editor x:Name="AddressEntry" Text="{Binding Address}"/>
        <Label Text="phonenumber" />
        <Entry x:Name="phonenumberEntry" Text="{Binding phonenumber}"/>
        <Label Text="email" />
        <Entry x:Name="emailEntry" Text="{Binding email}"/>
    
        <Button Text="Add" Clicked="addClicked"/>
    
       <!-- <Button Text="Delete" Clicked="deleteClicked"/>-->
    
        <Button Text="Details" Clicked="DetailsClicked"/>
    
        <!--  <Button Text="Edit" Clicked="speakClicked"/>-->
    
      </StackLayout>
    </ContentPage>

```

**EmployeeRegistration.cs**

```

   using DatabaseEmployeeCreation.SqlLite.ViewModel;
    using DatabaseEmployeeCreation.SqlLite.Views;
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Text;
    using System.Threading.Tasks;
    
    using Xamarin.Forms;
    
    namespace DatabaseEmployeeCreation.SqlLite
    {
        public partial class EmployeeRegistration : ContentPage
        {
            private int empid;
            private Employee obj;
    
            public EmployeeRegistration()
            {
                InitializeComponent();
    
            }
    
            public EmployeeRegistration(Employee obj)
            {
                this.obj = obj;
                var eid = obj.Eid;
                Navigation.PushModalAsync(new EmployeeRegistration());
                var Address = obj.Address;
                var email = obj.email;
                var Ename = obj.Ename;
                var phonenumber = obj.phonenumber;
                AddressEntry. = Address;
                emailEntry.Text = email;
                nameEntry.Text = Ename;
    
                //AddressEntry.Text = obj.Address;
                //emailEntry.Text = obj.email;
                //nameEntry.Text = obj.Ename;
                //phonenumberEntry.Text = obj.phonenumber;
    
                Employee empupdate = new Employee(); //updateing Values 
                empupdate.Address = AddressEntry.Text;
                empupdate.Ename = nameEntry.Text; 
                empupdate.email = emailEntry.Text;
                empupdate.Eid = obj.Eid;
                App.Database.SaveItem(empupdate);
                Navigation.PushModalAsync(new EmployeeRegistration());
              
            }
    
            public EmployeeRegistration(int empid)
            {
                this.empid = empid;
                Employee lst = App.Database.GetItem(empid);
                //var Address = lst.Address;
                //var email = lst.email;
                //var Ename = lst.Ename;
                //var phonenumber = lst.phonenumber;
                //AddressEntry.Text = Address;
                //emailEntry.Text = email;
                //nameEntry.Text = Ename;
                //phonenumberEntry.Text = phonenumber;
    
                // to retriva values based on id to 
                AddressEntry.Text = lst.Address;
                emailEntry.Text = lst.email;
                nameEntry.Text = lst.Ename;
                phonenumberEntry.Text = lst.phonenumber;
                
                Employee empupdate = new Employee(); //updateing Values 
                empupdate.Address = AddressEntry.Text;
                empupdate.email = emailEntry.Text;
                App.Database.SaveItem(empupdate);
                Navigation.PushModalAsync(new EmployeeRegistration());
            }
    
            void addClicked(object sender, EventArgs e)
            {
                //var createEmp = (Employee)BindingContext;
                Employee emp = new Employee();
                emp.Address = AddressEntry.Text;
                emp.email = emailEntry.Text;
                emp.Ename = nameEntry.Text;
                emp.phonenumber = phonenumberEntry.Text;
                App.Database.SaveItem(emp);
                this.Navigation.PushAsync(new EmployeeDetails());
    
            }
            //void deleteClicked(object sender, EventArgs e)
            //{
            //    var emp = (Employee)BindingContext;
            //    App.Database.DeleteItem(emp.Eid);
            //    this.Navigation.PopAsync();
            //}
            void DetailsClicked(object sender, EventArgs e)
            {
                var empcancel = (Employee)BindingContext;
                this.Navigation.PushAsync(new EmployeeDetails());
            }
            //    void speakClicked(object sender, EventArgs e)
            //    {
            //        var empspek = (Employee)BindingContext;
            //        //DependencyService.Get<ITextSpeak>().Speak(empspek.Address + " " + empspek.Ename);
            //    }
        }
    }

```


<li>
to display EmployeeDetails below code behind

```cs
 using DatabaseEmployeeCreation;
 using DatabaseEmployeeCreation.SqlLite;
 using System;
 using System.Collections.Generic;
 using System.Linq;
 using System.Text;
 using System.Threading.Tasks;
 
 using Xamarin.Forms;
 
 namespace DatabaseEmployeeCreation.SqlLite.Views
 {
     public partial class EmployeeDetails : ContentPage
     {
         ListView lv = new ListView();
         IEnumerable<Employee> lst;
         public EmployeeDetails()
         {
             InitializeComponent();
             displayemployee();
         }
 
         private void displayemployee()
         {
             Button btn = new Button()
             {
 
                 Text = "Details",
                 BackgroundColor = Color.Blue,
             };
             btn.Clicked += Btn_Clicked;
             //IEnumerable<Employee> lst = App.Database.GetItems();
             //IEnumerable<Employee> lst1 = App.Database.GetItemsNotDone();
             //IEnumerable<Employee> lst2 = App.Database.GetItemsNotDone();
             Content = new StackLayout()
             {
                 Children = { btn },
             };
         }
 
         private void Btn_Clicked(object sender, EventArgs e)
         {
             lst = App.Database.GetItems();
 
             lv.ItemsSource = lst;
             lv.HasUnevenRows = true;
             lv.ItemTemplate = new DataTemplate(typeof(OptionsViewCell));
 
             Content = new StackLayout()
             {
                 Children = { lv },
             };
 
         }
     }

```


</li>

```

       public class OptionsViewCell : ViewCell
        {
    
            int empid;
            Button btnEdit;
            public OptionsViewCell()
            {
            }
            protected override void OnBindingContextChanged()
            {
                base.OnBindingContextChanged();
    
                if (this.BindingContext == null)
                    return;
    
                dynamic obj = BindingContext;
                empid = Convert.ToInt32(obj.Eid);
                var lblname = new Label
                {
                    BackgroundColor = Color.Lime,
                    Text = obj.Ename,
                };
    
                var lblAddress = new Label
                {
                    BackgroundColor = Color.Yellow,
                    Text = obj.Address,
                };
    
                var lblphonenumber = new Label
                {
                    BackgroundColor = Color.Pink,
                    Text = obj.phonenumber,
                };
    
                var lblemail = new Label
                {
                    BackgroundColor = Color.Purple,
                    Text = obj.email,
                };
    
                var lbleid = new Label
                {
                    BackgroundColor = Color.Silver,
                    Text = (empid).ToString(),
                };
    
                //var lblname = new Label
                //{
                //    BackgroundColor = Color.Lime,
                //    // HorizontalOptions = LayoutOptions.Start
                //};
                //lblname.SetBinding(Label.TextProperty, "Ename");
    
                //var lblAddress = new Label
                //{
                //    BackgroundColor = Color.Yellow,
                //    //HorizontalOptions = LayoutOptions.Center,
                //};
                //lblAddress.SetBinding(Label.TextProperty, "Address");
    
                //var lblphonenumber = new Label
                //{
                //    BackgroundColor = Color.Pink,
                //    //HorizontalOptions = LayoutOptions.CenterAndExpand,
                //};
                //lblphonenumber.SetBinding(Label.TextProperty, "phonenumber");
    
                //var lblemail = new Label
                //{
                //    BackgroundColor = Color.Purple,
                //    // HorizontalOptions = LayoutOptions.CenterAndExpand
                //};
                //lblemail.SetBinding(Label.TextProperty, "email");
                //var lbleid = new Label
                //{
                //    BackgroundColor = Color.Silver,
                //    // HorizontalOptions = LayoutOptions.CenterAndExpand
                //};
                //lbleid.SetBinding(Label.TextProperty, "Eid");
                Button btnDelete = new Button
                {
                    BackgroundColor = Color.Gray,
    
                    Text = "Delete",
                    //WidthRequest = 15,
                    //HeightRequest = 20,
                    TextColor = Color.Red,
                    HorizontalOptions = LayoutOptions.EndAndExpand,
                };
                btnDelete.Clicked += BtnDelete_Clicked;
                //btnDelete.PropertyChanged += BtnDelete_PropertyChanged;  
    
                btnEdit = new Button
                {
                    BackgroundColor = Color.Gray,
                    Text = "Edit",
                    TextColor = Color.Green,
                };
                // lbleid.SetBinding(Label.TextProperty, "Eid");
                btnEdit.Clicked += BtnEdit_Clicked1; ;
                //btnEdit.Clicked += async (s, e) =>{
                //    await App.Current.MainPage.Navigation.PushModalAsync(new EmployeeRegistration());
                //};
    
                View = new StackLayout()
                {
                    Orientation = StackOrientation.Horizontal,
                    BackgroundColor = Color.White,
                    Children = { lbleid, lblname, lblAddress, lblemail, lblphonenumber, btnDelete, btnEdit },
                };
    
                //View = new StackLayout()
                //{ HorizontalOptions = LayoutOptions.Center, WidthRequest = 10, BackgroundColor = Color.Yellow, Children = { lblAddress } };
    
                //View = new StackLayout()
                //{ HorizontalOptions = LayoutOptions.End, WidthRequest = 30, BackgroundColor = Color.Yellow, Children = { lblemail } };
    
                //View = new StackLayout()
                //{ HorizontalOptions = LayoutOptions.End, BackgroundColor = Color.Green, Children = { lblphonenumber } };
    
    
    
    
                //string Empid =c.eid ;
    
            }
    
            private async void BtnEdit_Clicked1(object sender, EventArgs e)
            {
               Employee obj= App.Database.GetItem(empid);
                if (empid > 0)
                {
                    await App.Current.MainPage.Navigation.PushModalAsync(new EmployeeRegistration(obj));
                }
                else {
                await App.Current.MainPage.Navigation.PushModalAsync(new EmployeeRegistration(empid));
                }
            }
    
    
    
            private void BtnDelete_Clicked(object sender, EventArgs e)
            {
                // var eid = Convert.ToInt32(empid);
                // var item = (Xamarin.Forms.Button)sender;
                int eid = empid;
                App.Database.DeleteItem(eid);
            }
            //private void BtnDelete_PropertyChanged(object sender, System.ComponentModel.PropertyChangedEventArgs e)
            //{
            // var ename=  e.PropertyName;
            //}
        }
    
        //private void BtnDelete_Clicked(object sender, EventArgs e)
        //{
        //    var eid = 8;
        //    var item = (Xamarin.Forms.Button)sender;
    
        //    App.Database.DeleteItem(eid);
        //}
    }

```


1. To implement method in Android and ios  GetConnection() method

```

   using System;
    using Xamarin.Forms;
    using System.IO;
    using DatabaseEmployeeCreation.Droid;
    using DatabaseEmployeeCreation.SqlLite.ViewModel;
    using SQLite;
    using SQLite.Net;
    
    [assembly: Dependency(typeof(SQLiteEmployee_Andriod))]
    namespace DatabaseEmployeeCreation.Droid
    {
        public class SQLiteEmployee_Andriod : ISQLite
        {
            public SQLiteEmployee_Andriod()
            {
            }
    
            #region ISQLite implementation
            public SQLiteConnection GetConnection()
            {
                //var sqliteFilename = "EmployeeSQLite.db3";
                //string documentsPath = System.Environment.GetFolderPath(System.Environment.SpecialFolder.Personal); // Documents folder
                //var path = Path.Combine(documentsPath, sqliteFilename);
    
                //// This is where we copy in the prepopulated database
                //Console.WriteLine(path);
                //if (!File.Exists(path))
                //{
                //    var s = Forms.Context.Resources.OpenRawResource(Resource.Raw.EmployeeSQLite);  // RESOURCE NAME ###
    
                //    // create a write stream
                //    FileStream writeStream = new FileStream(path, FileMode.OpenOrCreate, FileAccess.Write);
                //    // write to the stream
                //    ReadWriteStream(s, writeStream);
                //}
    
                //var conn = new SQLiteConnection(path);
    
                //// Return the database connection 
                //return conn;
                var filename = "DatabaseEmployeeCreationSQLite.db3";
                var documentspath = Environment.GetFolderPath(Environment.SpecialFolder.Personal);
                var path = Path.Combine(documentspath, filename);
                var platform = new SQLite.Net.Platform.XamarinAndroid.SQLitePlatformAndroid();
                var connection = new SQLite.Net.SQLiteConnection(platform, path);
                return connection;
            }
    
            //public  SQLiteConnection GetConnection()
            //{
            //    var filename = "EmployeeSQLite.db3";
            //    var documentspath = Environment.GetFolderPath(Environment.SpecialFolder.Personal);
            //    var path = Path.Combine(documentspath, filename);
    
            //    var platform = new SQLite.Net.Platform.XamarinAndroid.SQLitePlatformAndroid();
            //    var connection = new SQLite.Net.SQLiteConnection(platform, path);
            //    return connection;
            //}
            #endregion
    
            /// <summary>
            /// helper method to get the database out of /raw/ and into the user filesystem
            /// </summary>
            void ReadWriteStream(Stream readStream, Stream writeStream)
            {
                int Length = 256;
                Byte[] buffer = new Byte[Length];
                int bytesRead = readStream.Read(buffer, 0, Length);
                // write the required bytes
                while (bytesRead > 0)
                {
                    writeStream.Write(buffer, 0, bytesRead);
                    bytesRead = readStream.Read(buffer, 0, Length);
                }
                readStream.Close();
                writeStream.Close();
            }
        }
    }

```

I hope this above example is very easy way i explained

