# AveVerb REST API

### List verbs

* _Request:_ `GET` **/list**
* _Args:_ name={all, popular}
* _Returns:_ the list of verbs.

### Rule for verb

* _Request:_ `GET` **/rule**
* _Args:_ verb={verb}.
* _Returns:_ the rule for the _verb_.

### List samples

* _Request:_ `GET` **/samples**
* _Args:_ verb={verb}, page=num, count=num
* _Returns:_ the list sample for the _verb_.

