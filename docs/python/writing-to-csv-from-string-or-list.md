# Writing to CSV from String or List


Writing to a .csv file is not unlike writing to a regular file in most regards, and is fairly straightforward. I will, to the best of my ability, cover the easiest, and most efficient approach to the problem.



## Basic Write Example


```
import csv

#------ We will write to CSV in this function ------------

def csv_writer(data, path):
    
    #Open CSV file whose path we passed.
    with open(path, &quot;wb&quot;) as csv_file:
        
        writer = csv.writer(csv_file, delimiter=',')
        for line in data:
            writer.writerow(line)



#---- Define our list here, and call function ------------

if __name__ == &quot;__main__&quot;:

    &quot;&quot;&quot;
    data = our list that we want to write. 
    Split it so we get a list of lists.
    &quot;&quot;&quot;
    data = [&quot;first_name,last_name,age&quot;.split(&quot;,&quot;),
            &quot;John,Doe,22&quot;.split(&quot;,&quot;),
            &quot;Jane,Doe,31&quot;.split(&quot;,&quot;),
            &quot;Jack,Reacher,27&quot;.split(&quot;,&quot;)
            ]

    # Path to CSV file we want to write to.
    path = &quot;output.csv&quot;
    csv_writer(data, path)

```



## Appending a String as a newline in a CSV file


```
def append_to_csv(input_string):
    with open(&quot;fileName.csv&quot;, &quot;a&quot;) as csv_file:
        csv_file.write(input_row + &quot;\n&quot;)

```



#### Parameters


|Parameter|Details
|------
|open (**&quot;/path/&quot;**, &quot;mode&quot;)|Specify the path to your CSV file
|open (path, **&quot;mode&quot;**)|Specify mode to open file in (read, write, etc.)
|csv.writer(**file**, delimiter)|Pass opened CSV file here
|csv.writer(file, **delimiter=' '**)|Specify delimiter character or pattern



#### Remarks


> 
`open( path, &quot;wb&quot;)`


`&quot;wb&quot;` - Write mode.

The `b` parameter in `&quot;wb&quot;` we have used, is necessary only if you want to open it in binary mode, which is needed only in some operating systems like Windows.

> 
`csv.writer ( csv_file, delimiter=',' )`


Here the delimiter we have used, is `,`, because we want each cell of data in a row, to contain the first name, last name, and age respectively.
Since our list is split along the `,` too, it proves rather convenient for us.

