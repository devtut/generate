---
metaTitle: "Objective C - Grand Central Dispatch"
description: "What is Grand central dispatch."
---

# Grand Central Dispatch




## What is Grand central dispatch.


**What is Concurrency?**

- Doing multiple things at the same time.

- Taking advantage of number of cores available in multicore CPUs.

- Running multiple programs in parallel.

**Objectives of Concurrency**

- Running program in background without hogging CPU.

<li>Define Tasks, Define Rules and let the system take the responsibility
of performing them.</li>

<li>Improve responsiveness by ensuring that the main thread is free to
respond to user events.</li>

**DISPATCH QUEUES**

Grand central dispatch – dispatch queues allows us to execute arbitrary blocks of code either asynchronously or synchronously
All Dispatch Queues are first in – first out
All the tasks added to dispatch queue are started in the order they were added to the dispatch queue.

