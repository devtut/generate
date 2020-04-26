# Multiprocessing




## Running Two Simple Processes


A simple example of using multiple processes would be two processes (workers) that are executed separately. In the following example, two processes are started:

- `countUp()` counts 1 up, every second.
- `countDown()` counts 1 down, every second.

```
import multiprocessing
import time
from random import randint

def countUp():
    i = 0
    while i <= 3:
        print('Up:\t{}'.format(i))
        time.sleep(randint(1, 3)) # sleep 1, 2 or 3 seconds
        i += 1

def countDown():
    i = 3
    while i >= 0:
        print('Down:\t{}'.format(i))
        time.sleep(randint(1, 3)) # sleep 1, 2 or 3 seconds
        i -= 1

if __name__ == '__main__':
    # Initiate the workers.
    workerUp = multiprocessing.Process(target=countUp)
    workerDown = multiprocessing.Process(target=countDown)
    
    # Start the workers.
    workerUp.start()
    workerDown.start()

    # Join the workers. This will block in the main (parent) process
    # until the workers are complete.
    workerUp.join()
    workerDown.join()

```

The output is as follows:

```
Up:    0
Down:    3
Up:    1
Up:    2
Down:    2
Up:    3
Down:    1
Down:    0

```



## Using Pool and Map


```
from multiprocessing import Pool

def cube(x):
    return x ** 3

if __name__ == =__main__=:
    pool = Pool(5)
    result = pool.map(cube, [0, 1, 2, 3])

```

`Pool` is a class which manages multiple `Workers` (processes) behind the scenes and lets you, the programmer, use.

`Pool(5)` creates a new Pool with 5 processes, and `pool.map` works just like [map](https://docs.python.org/2/library/functions.html#map) but it uses multiple processes (the amount defined when creating the pool).

Similar results can be achieved using `map_async`, `apply` and `apply_async` which can be found in [the documentation](https://docs.python.org/2/library/multiprocessing.html).

