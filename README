# CreepySwitch
Application layer switch replica.

## Message format
```
SWITCH MESSAGE\n
Source: <node id>\n
Destination: <node id>\n
<message body>\n
```

## Accept process for new node
```
$ ./client <NODE ADDRESS>

SWITCH |_______| CLIENT
            /---1. user define address
step 2:----/
Check available addresses
       \----\
             \--> Accept: Send messages
              \--> Wrong address: Exit process
```

Address request message:
```
SWITCH REQUEST\n
Address: <node id>\n
```

Switch respond:
```
SWITCH RESPOND\n
Address (accepted|not accepted)\n
```
