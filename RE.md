Regular Expressions
===================

Using [jEdit notation](http://www.jedit.org/users-guide/regexps.html).

Markdown hyperlink swap
-----------------------

From:

    \[([^\]]*)\]\(([^\)]*)\)

To:

    \[$2\]\($1\)


Prolog arguments must be compounds
----------------------------------

From:

    between_hex\(\'([0-9A-Z]+)\', \'([0-9A-Z]+)\'\)

To:

    between\(hex\(\'$1\'\), hex\(\'$2\'\)\)
