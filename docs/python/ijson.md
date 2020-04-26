# ijson




## Simple Example


Sample Example Taken from one [benchmarking](http://explique.me/Ijson/)

```
import ijson

def load_json(filename):
    with open(filename, 'r') as fd:
        parser = ijson.parse(fd)
        ret = {'builders': {}}
        for prefix, event, value in parser:
            if (prefix, event) == ('builders', 'map_key'):
                buildername = value
                ret['builders'][buildername] = {}
            elif prefix.endswith('.shortname'):
                ret['builders'][buildername]['shortname'] = value

        return ret

if __name__ == &quot;__main__&quot;:
    load_json('allthethings.json')

```

JSON FILE [LINK](https://secure.pub.build.mozilla.org/builddata/reports/allthethings.json)

