# NicLib ChangeLog

## v0.2.2

* added changelog ;p
* replaced all things ```ListLike``` with monomorphic reifications and ```mono-traversable```. In particular, the following in ```NicLib.List``` have been replaced by equivalent ```mono-traversable``` functions:

| NicLib.List | mono-traversable |
|-------------|------------------|
| split       | splitWhen        |
| intercalate | ointercalate     |
| replace     | replaceSeq       |
| splitOn     | splitSeq         |

The main reason for leaving ```ListLike``` is that it uses injective functional dependencies, whereas ```mono-traversable``` uses a non-injective type family. Not only that, but ```mono-traversable``` has connections with other snoyman libraries.
