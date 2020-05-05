---
metaTitle: "Bash - Design Patterns"
description: "The Publish/Subscribe (Pub/Sub) Pattern"
---

# Design Patterns


Accomplish some common design patterns in Bash



## The Publish/Subscribe (Pub/Sub) Pattern


When a Bash project turns into a library, it can become difficult to add new functionality. Function names, variables and parameters usually need to be changed in the scripts that utilize them. In scenarios like this, it is helpful to decouple the code and use an event driven design pattern. In said pattern, an external script can subscribe to an event. When that event is triggered (published) the script can execute the code that it registered with the event.

**pubsub.sh:**

```

   #!/usr/bin/env bash

    #
    # Save the path to this script's directory in a global env variable
    #
    DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

    #
    # Array that will contain all registered events
    #
    EVENTS=()

    function action1() {
        echo "Action #1 was performed ${2}"
    }

    function action2() {
        echo "Action #2 was performed"
    }

    #
    # @desc   :: Registers an event
    # @param  :: string $1 - The name of the event. Basically an alias for a function name
    # @param  :: string $2 - The name of the function to be called
    # @param  :: string $3 - Full path to script that includes the function being called
    #
    function subscribe() {
        EVENTS+=("${1};${2};${3}")
    }

    #
    # @desc   :: Public an event
    # @param  :: string $1 - The name of the event being published
    #
    function publish() {
        for event in ${EVENTS[@]}; do
            local IFS=";"
            read -r -a event <<< "$event"
            if [[  "${event[0]}" ==  "${1}" ]]; then
                ${event[1]} "$@"
            fi
        done
    }

    #
    # Register our events and the functions that handle them
    #
    subscribe "/do/work"           "action1" "${DIR}"
    subscribe "/do/more/work"      "action2" "${DIR}"
    subscribe "/do/even/more/work" "action1" "${DIR}"

    #
    # Execute our events
    #
    publish "/do/work"
    publish "/do/more/work"
    publish "/do/even/more/work" "again"

```

**Run:**

```bash
chmod +x pubsub.sh
./pubsub.sh

```

