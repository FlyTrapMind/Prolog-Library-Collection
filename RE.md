Regular Expressions
===================

Using [jEdit notation](http://www.jedit.org/users-guide/regexps.html).

Markdown hyperlink swap
-----------------------

From:

```re
\[([^\]]*)\]\(([^\)]*)\)
```

To:

```re
\[$2\]\($1\)
```



Prolog arguments must be compounds
----------------------------------

From:

```re
between_hex\(\'([0-9A-Z]+)\', \'([0-9A-Z]+)\'\)
```

To:

```re
between\(hex\(\'$1\'\), hex\(\'$2\'\)\)
```



ISO language tag conversion
---------------------------

From:

```re
'iso639-1'\('iso639-3':([a-z]+)\) --> "([a-z]+)".
```

To:

```re
owl_assert_identity\('iso639-1':$2, 'iso639-3':$1\).
```
