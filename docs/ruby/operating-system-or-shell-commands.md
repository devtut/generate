---
metaTitle: "Ruby - Operating System or Shell commands"
description: "Recommended ways to execute shell code in Ruby:, Clasic ways to execute shell code in Ruby:"
---

# Operating System or Shell commands


There are many ways to interact with the operating system. From within Ruby you can run shell/system commands or sub-processes.



## Recommended ways to execute shell code in Ruby:


**Open3.popen3 or Open3.capture3:**<br />
Open3 actually just uses Ruby's spawn command, but gives you a much better API.

**Open3.popen3**

Popen3 runs in a sub-process and returns stdin, stdout, stderr and wait_thr.

```ruby
require 'open3'
stdin, stdout, stderr, wait_thr = Open3.popen3("sleep 5s && ls")
puts "#{stdout.read} #{stderr.read} #{wait_thr.value.exitstatus}"

```

or

```ruby
require 'open3'
cmd = 'git push heroku master'
Open3.popen3(cmd) do |stdin, stdout, stderr, wait_thr|
  puts "stdout is:" + stdout.read
  puts "stderr is:" + stderr.read
end

```

will output:
<strong>stdout is:
stderr is:fatal: Not a git repository (or any of the parent directories): .git</strong>

or

```ruby
require 'open3'
cmd = 'ping www.google.com'
Open3.popen3(cmd) do |stdin, stdout, stderr, wait_thr|
  while line = stdout.gets
    puts line
  end
end

```

will output:

<strong>Pinging www.google.com [216.58.223.36] with 32 bytes of data:<br />
Reply from 216.58.223.36: bytes=32 time=16ms TTL=54<br />
Reply from 216.58.223.36: bytes=32 time=10ms TTL=54<br />
Reply from 216.58.223.36: bytes=32 time=21ms TTL=54<br />
Reply from 216.58.223.36: bytes=32 time=29ms TTL=54<br />
Ping statistics for 216.58.223.36:<br />
Packets: Sent = 4, Received = 4, Lost = 0 (0% loss),<br />
Approximate round trip times in milli-seconds:<br />
Minimum = 10ms, Maximum = 29ms, Average = 19ms</strong>

**Open3.capture3:**

```ruby
require 'open3'

stdout, stderr, status = Open3.capture3('my_funky_command', 'and', 'some', 'argumants')
if status.success?
  # command completed successfully, do some more stuff
else
  raise "An error occured"
end

```

or

```ruby
Open3.capture3('/some/binary with some args')  

```

Not recommended though, due to additional overhead and the potential for shell injections.

If the command reads from stdin and you want to feed it some data:

```ruby
Open3.capture3('my_funky_command', stdin_data: 'read from stdin')  

```

Run the command with a different working directory, by using chdir:

```ruby
Open3.capture3('my_funky_command', chdir: '/some/directory')  

```



## Clasic ways to execute shell code in Ruby:


**Exec:**

```ruby
exec 'echo "hello world"'

```

or

```ruby
exec ('echo "hello world"')

```

**The System Command:**

```ruby
system 'echo "hello world"'

```

Will output "hello world" in the command window.

or

```ruby
system ('echo "hello world"')

```

The system command can return a true if the command was successful or nill when not.

```ruby
result = system 'echo "hello world"'
puts result  # will return a true in the command window

```

**The backticks (`):**

`echo "hello world"`
Will output "hello world" in the command window.

You can also catch the result.

```ruby
result = `echo "hello world"`  
puts "We always code a " + result  

```

**IO.popen:**

```ruby
# Will get and return the current date from the system
IO.popen("date") { |f| puts f.gets }

```



#### Remarks


**Exec:**<br />
Exec is very limited in functionality and when executed will exit the Ruby program and run the command.

**The System Command:**<br />
The System command runs in a sub-shell instead of replacing the current process and returns true or nill. The system command is, like backticks, a blocking operation where the main application waits until the result of the system operation completes. Here the main operation never needs to worry about capturing an exception raised from the child process.

The output of system function will always be true or nil depending on whether or not the script has been executed without error. Therefore, every error while executing the script will not be passed to our application. The main operation never needs to worry about capturing an exception raised from the child process. In this case the output is nil because the child process raised an exception.<br />
This is a blocking operation where the Ruby program will wait until the operation of the command completes before going on.<br />
The system operation use fork to fork the current process and then execute the given operation using exec.

**The backticks (`):**<br />
The backtick character is usualy located under the escape key on the keyboard.
Backticks runs in a sub-shell instead of replacing the current process and returns the result of the command.<br />
Here we can get the output of the command but the program will crash when an exception is generated.<br />
If there is an exception in the sub-process then that exception is given to the main process and the main process might terminate if exception is not handled.
This is a blocking operation where the Ruby program will wait until the operation of the command completes before going on.<br />
The system operation use fork to fork the current process and then execute the given operation using exec.

**IO.popen:**<br />
IO.popen runs in a sub-process. Here the sub-process standard input and standard output are connected to the IO object.

**Popen3:**<br />
Popen3 allows you to access the standard input, standard output and standard error.<br />
The subprocess's standard input and output will be returned into IO objects.

**$? (same as $CHILD_STATUS)**<br />
Can be used with the backticks, system() or %x{} operations and will give the status of the last system executed command.<br />
This might be usefull to access the `exitstatus` and the `pid` properties.

