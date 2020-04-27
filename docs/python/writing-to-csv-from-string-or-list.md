# Writing to CSV from String or List


Writing to a .csv file is not unlike writing to a regular file in most regards, and is fairly straightforward. I will, to the best of my ability, cover the easiest, and most efficient approach to the problem.



## Basic Write Example


```
import csv

#------ We will write to CSV in this function ------------

def csv_writer(data, path):
    
    #Open CSV file whose path we passed.
    with open(path, "wb") as csv_file:
        
        writer = csv.writer(csv_file, delimiter=',')
        for line in data:
            writer.writerow(line)



#---- Define our list here, and call function ------------

if __name__ == "__main__":

    """
    data = our list that we want to write. 
    Split it so we get a list of lists.
    """
    data = ["first_name,last_name,age".split(","),
            "John,Doe,22".split(","),
            "Jane,Doe,31".split(","),
            "Jack,Reacher,27".split(",")
            ]

    # Path to CSV file we want to write to.
    path = "output.csv"
    csv_writer(data, path)

```



## Appending a String as a newline in a CSV file


```
def append_to_csv(input_string):
    with open("fileName.csv", "a") as csv_file:
        csv_file.write(input_row + "\n")

```



#### Parameters


|Parameter|Details
|------
|open (**"/path/"**, "mode")|Specify the path to your CSV file
|open (path, **"mode"**)|Specify mode to open file in (read, write, etc.)
|csv.writer(**file**, delimiter)|Pass opened CSV file here
|csv.writer(file, **delimiter=' '**)|Specify delimiter character or pattern



#### Remarks


> 
`open( path, "wb")`


`"wb"` - Write mode.

The `b` parameter in `"wb"` we have used, is necessary only if you want to open it in binary mode, which is needed only in some operating systems like Windows.

> 
`csv.writer ( csv_file, delimiter=',' )`


Here the delimiter we have used, is `,`, because we want each cell of data in a row, to contain the first name, last name, and age respectively.
Since our list is split along the `,` too, it proves rather convenient for us.

