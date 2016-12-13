```
$ python loader.py --conf=./index.json
 ==  ./index.json  ==
 {
   "morning": "./morning/index.json",
   "evening": "./evening/index.json"
 }
   ==  ./morning/index.json  ==
   {
     "greeting": "./greeting.json",
     "end-greeting": "../evening/index.json"
   }
     ==  ./morning/greeting.json  ==
     "hello"
morning: hello
     ==  ./evening/index.json  ==
     {
       "greeting": "./greeting.json"
     }
       ==  ./evening/greeting.json  ==
       "bye"
evening: bye
```
