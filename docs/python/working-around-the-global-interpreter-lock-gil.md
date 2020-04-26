# Working around the Global Interpreter Lock (GIL)



## Multiprocessing.Pool


The simple answer, when asking how to use threads in Python is: &quot;Don't.  Use processes, instead.&quot;  The multiprocessing module lets you create processes with similar syntax to creating threads, but I prefer using their convenient Pool object.

Using [the code that David Beazley first used to show the dangers of threads against the GIL](http://www.dabeaz.com/GIL/gilvis/measure2.py), we'll rewrite it using [multiprocessing.Pool](https://docs.python.org/3/library/multiprocessing.html#using-a-pool-of-workers):

### David Beazley's code that showed GIL threading problems

```
from threading import Thread
import time
def countdown(n):
    while n > 0:
        n -= 1

COUNT = 10000000

t1 = Thread(target=countdown,args=(COUNT/2,))
t2 = Thread(target=countdown,args=(COUNT/2,))
start = time.time()
t1.start();t2.start()
t1.join();t2.join()
end = time.time()
print end-start

```

```
import multiprocessing
import time
def countdown(n):
    while n > 0:
        n -= 1

COUNT = 10000000

start = time.time()
with multiprocessing.Pool as pool:
    pool.map(countdown, [COUNT/2, COUNT/2])

    pool.close()
    pool.join()

end = time.time()
print(end-start)

```

Instead of creating threads, this creates new processes.  Since each process is its own interpreter, there are no GIL collisions.  multiprocessing.Pool will open as many processes as there are cores on the machine, though in the example above, it would only need two.  In a real-world scenario, you want to design your list to have at least as much length as there are processors on your machine.  The Pool will run the function you tell it to run with each argument, up to the number of processes it creates.  When the function finishes, any remaining functions in the list will be run on that process.

I've found that, even using the `with` statement, if you don't close and join the pool, the processes continue to exist.  To clean up resources, I always close and join my pools.



## Cython nogil:


Cython is an alternative python interpreter.  It uses the GIL, but lets you disable it.  See [their documentation](http://docs.cython.org/en/latest/src/userguide/external_C_code.html?highlight=nogil.html#acquiring-and-releasing-the-gil)

As an example, using [the code that David Beazley first used to show the dangers of threads against the GIL](http://www.dabeaz.com/GIL/gilvis/measure2.py), we'll rewrite it using nogil:

### David Beazley's code that showed GIL threading problems

```
from threading import Thread
import time
def countdown(n):
    while n > 0:
        n -= 1

COUNT = 10000000

t1 = Thread(target=countdown,args=(COUNT/2,))
t2 = Thread(target=countdown,args=(COUNT/2,))
start = time.time()
t1.start();t2.start()
t1.join();t2.join()
end = time.time()
print end-start

```

### Re-written using nogil (ONLY WORKS IN CYTHON):

```
from threading import Thread
import time
def countdown(n):
    while n > 0:
        n -= 1

COUNT = 10000000

with nogil:
    t1 = Thread(target=countdown,args=(COUNT/2,))
    t2 = Thread(target=countdown,args=(COUNT/2,))
    start = time.time()
    t1.start();t2.start()
    t1.join();t2.join()
    
end = time.time()
print end-start

```

It's that simple, as long as you're using cython.  Note that the documentation says you must make sure not to change any python objects:

> 
<p>Code in the body of the statement must not manipulate Python objects
in any way, and must not call anything that manipulates Python objects
without first re-acquiring the GIL. Cython currently does not check
this.</p>




#### Remarks


### Why is there a GIL?

Short version: the GIL ensures that no matter how many processors and threads you have, *only one thread of a python interpreter will run at one time.*

This has a lot of ease-of-use benefits, but also has a lot of negative benefits as well.

Note that a GIL is not a requirment of the Python language.  Consequently, you can't access the GIL directly from standard python code.  Not all implementations of Python use a GIL.

**Interpreters that have a GIL:** CPython, PyPy, Cython (but you can disable the GIL with `nogil`)

**Interpreters that do not have a GIL:** Jython, IronPython

### Details on how the GIL operates:


<p>CPython automatically releases the GIL when a thread performs an I/O operation.  Image processing libraries and numpy number crunching operations release the GIL before doing their processing.

### Benefits of the GIL

- Garbage collection - thread-safe reference counts must be modified while the GIL is locked.  *In CPython, all of garbarge collection is tied to the GIL.*  This is a big one; see the python.org wiki article about the GIL (listed in References, below) for details about what must still be functional if one wanted to remove the GIL.
- Ease for programmers dealing with the GIL - locking everything is simplistic, but easy to code to
- Eases the import of modules from other languages

### Consequences of the GIL


<a class="remarks-subsection-anchor" name="remarks-references:-4"></a>
<h3>References:</h3>
<p>[https://wiki.python.org/moin/GlobalInterpreterLock](https://wiki.python.org/moin/GlobalInterpreterLock) - quick summary of what it does, fine details on all the benefits

[http://programmers.stackexchange.com/questions/186889/why-was-python-written-with-the-gil](http://programmers.stackexchange.com/questions/186889/why-was-python-written-with-the-gil) - clearly written summary

[http://www.dabeaz.com/python/UnderstandingGIL.pdf](http://www.dabeaz.com/python/UnderstandingGIL.pdf) - how the GIL works and why it slows down on multiple cores

[http://www.dabeaz.com/GIL/gilvis/index.html](http://www.dabeaz.com/GIL/gilvis/index.html) - visualization of the data showing how the GIL locks up threads

[http://jeffknupp.com/blog/2012/03/31/pythons-hardest-problem/](http://jeffknupp.com/blog/2012/03/31/pythons-hardest-problem/) - simple to understand history of the  GIL problem

[https://jeffknupp.com/blog/2013/06/30/pythons-hardest-problem-revisited/](https://jeffknupp.com/blog/2013/06/30/pythons-hardest-problem-revisited/) - details on ways to work around the GIL's limitations

