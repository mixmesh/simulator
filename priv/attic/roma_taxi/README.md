In order to use the Roma data set in the simulator do the following:

* Create an account on https://crawdad.org/
* Download taxi_february.tar.gz from https://crawdad.org/roma/taxi/20140717/taxicabs/ and put it in this directory
* gunzip taxi_february.tar.gz
* ./split.sh

This will take several minutes...

When done update ./etc/obscrete.conf with the following entry:

```
"simulator": {
    "enabled": true,
    "data-set": "roma"
}
```

Start the simulator:

`./bin/obscrete --config etc/obscrete.conf`
