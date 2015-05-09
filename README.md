# scala-json-parser
private study for parser combinator in Scala

## Example

```
  println(JsonParser("{'hoge':{'hoge':'hoge','hage':'hage'}}"))
  Right(((None~(({~List((((('~List(h, o, g, e))~')~:)~(({~List((((('~List(h, o, g, e))~')~:)~(('~List(h, o, g, e))~')), (((('~List(h, a, g, e))~')~:)~(('~List(h, a, g, e))~'))))~}))))~}))~None))
```

## Todo
refer to [RFC7159](http://rfc7159.net/rfc7159), but actually, it hasn't been implemented correctly based on RFC7159.
