schememilk [![Build Status](https://travis-ci.org/philopon/schememilk.svg?branch=master)](https://travis-ci.org/philopon/schememilk)
===
DB schema version magagement tool inspired by migr8

installation
---
```.bash
$ cabal install schememilk-bin
```

usage
---
```.bash
# init sqlite repository.
$ schememilk init sqlite db.sqlite3

# or init postgresql repository.
# $ schememilk init postgresql -h localhost -d db

# new schema(open $SCHEMEMILK_EDITOR || $EDITOR)
$ schememilk new

# show schema
$ schememilk log
2014-06-18 04:35:44 UTC	sJquVpYF	create test table.

# show status
$ schememilk status
not migrated(1 schema not applied exists.).

# apply 1 schema
$ schememilk up
up to sJquVpYF

# show status
$ schememilk status
db version: sJquVpYF(latest)
```
