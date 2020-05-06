---
metaTitle: "Ruby - Thread"
description: "Basic Thread Semantics, Accessing shared resources, How to kill a thread, Terminating a Thread"
---

# Thread




## Basic Thread Semantics


A new thread separate from the main thread's execution, can be created using `Thread.new`.

```ruby
thr = Thread.new {
  sleep 1 # 1 second sleep of sub thread
  puts "Whats the big deal"
}

```

This will automatically start the execution of the new thread.

To freeze execution of the main Thread, until the new thread stops, use `join`:

```ruby
thr.join #=> ... "Whats the big deal"

```

Note that the Thread may have already finished when you call join, in which case execution will continue normally. If a sub-thread is never joined, and the main thread completes, the sub-thread will not execute any remaining code.



## Accessing shared resources


Use a mutex to synchronise access to a variable which is accessed from multiple threads:

```ruby
counter = 0
counter_mutex = Mutex.new

# Start three parallel threads and increment counter
3.times.map do |index|
  Thread.new do
    counter_mutex.synchronize { counter += 1 }
  end
end.each(&:join) # Wait for all threads to finish before killing the process

```

Otherwise, the value of `counter` currently visible to one thread could be changed by another thread.

Example **without** `Mutex` (see e.g. `Thread 0`, where `Before` and `After` differ by more than `1`):

```ruby
2.2.0 :224 > counter = 0; 3.times.map { |i| Thread.new { puts "[Thread #{i}] Before: #{counter}"; counter += 1; puts "[Thread #{i}] After: #{counter}"; } }.each(&:join)
[Thread 2] Before: 0
[Thread 0] Before: 0
[Thread 0] After: 2
[Thread 1] Before: 0
[Thread 1] After: 3
[Thread 2] After: 1

```

Example **with** `Mutex`:

```ruby
2.2.0 :226 > mutex = Mutex.new; counter = 0; 3.times.map { |i| Thread.new { mutex.synchronize { puts "[Thread #{i}] Before: #{counter}"; counter += 1; puts "[Thread #{i}] After: #{counter}"; } } }.each(&:join)
[Thread 2] Before: 0
[Thread 2] After: 1
[Thread 1] Before: 1
[Thread 1] After: 2
[Thread 0] Before: 2
[Thread 0] After: 3

```



## How to kill a thread


You call use `Thread.kill` or `Thread.terminate`:

```ruby
thr = Thread.new { ... }
Thread.kill(thr)

```



## Terminating a Thread


A thread terminates if it reaches the end of its code block. The best way to terminate a thread early is to convince it to reach the end of its code block. This way, the thread can run cleanup code before dying.

This thread runs a loop while the instance variable continue is true. Set this variable to false, and the thread will die a natural death:

```ruby
require 'thread'

class CounterThread < Thread
  def initialize
    @count = 0
    @continue = true

    super do
      @count += 1 while @continue
      puts "I counted up to #{@count} before I was cruelly stopped."
    end
  end

  def stop
    @continue = false
  end
end

counter = CounterThread.new
sleep 2
counter.stop

```

