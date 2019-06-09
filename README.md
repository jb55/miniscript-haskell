
# miniscript-haskell

A Bitcoin miniscript implementation in Haskell


## Status

This is a work in progress

- [ ] parsing
- [ ] miniscript -> script
- [ ] script -> miniscript


## What's working

I've just started, but there's a basic parser in place:

```
$ parse exprP "" "or(pk(121212121212121212121212121212121212121212121212121212121212121a),hash(H))"
Right (Or (Pk 121212121212121212121212121212121212121212121212121212121212121a) (Hash 8888888888888888888888888888888888888888888888888888888888888888))
```

