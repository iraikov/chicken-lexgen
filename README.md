# chicken-lexgen
Lexer and parser combinators in Chicken Scheme

## Description

`lexgen` is a lexer generator comprised in its core of only five
small procedures that can be combined to form pattern matchers.

A pattern matcher procedure takes an input stream, and returns a new
stream advanced by the pattern.

A stream is defined as a list that contains a list of characters
consumed by the pattern matcher, and a list of characters not yet
consumed. E.g., the list

  ((#\a) (#\b #\c #\d #\e))

represents a stream that contains the consumed character a, and the
unconsumed characters b c d e.

A pattern matcher has the form of a procedure that takes a success
continuation, which is invoked when the pattern matches and the stream
is advanced, an error continuation, which is invoked when the pattern
does not match, and an input stream.

## Library Procedures

Every combinator procedure in this library returns a procedure that
takes in a success continuation, error continuation and input stream
as arguments.

### Basic procedures

<procedure>(seq MATCHER1 MATCHER2) => MATCHER</procedure>

`seq` builds a matcher that matches a sequence of patterns. 

<procedure>(bar MATCHER1 MATCHER2) => MATCHER</procedure>

`bar` matches either of two patterns. It's analogous to patterns
separated by `|` in traditional regular expressions.

<procedure>(star MATCHER) => MATCHER</procedure>

`star` is an implementation of the Kleene closure. It is analogous
to `*` in traditional regular expressions.

### Token procedure

<procedure>(tok <Input>) => (LAMBDA TOKEN PROC) => MATCHER</procedure>

Procedure `tok` builds pattern matchers based on character
comparison operations. It is intended for matching input sequences of
arbitrary kinds, e.g. character lists, strings, or other kinds of
sequences.

For each stream given, `tok` applies a procedure to the given token
`TOKEN` and an input character. If the procedure returns a true
value, that value is prepended to the list of consumed elements, and
the input character is removed from the list of input elements.


<procedure>(char CHAR) => MATCHER</procedure>

Matches a single character.

<procedure>(set CHAR-SET) => MATCHER</procedure>

Matches any of a SRFI-14 set of characters. 

<procedure>(range CHAR CHAR) => MATCHER</procedure>

Matches a range of characters. Analogous to character class `[]`.

<procedure>(lit STRING) => MATCHER</procedure>

Matches a literal string `s`.

### Convenience procedures

These procedures are built from the basic procedures and are provided
for convenience.

<procedure>(try PROC) => PROC</procedure>

Converts a binary predicate procedure to a binary procedure that
returns its right argument when the predicate is true, and false
otherwise.

<procedure>(lst MATCHER-LIST) => MATCHER</procedure>

Constructs a matcher for the sequence of matchers in `MATCHER-LIST`.

<procedure>(pass) => MATCHER</procedure>

This matcher returns without consuming any input.

<procedure>(pos MATCHER) => MATCHER</procedure>

Positive closure. Analogous to `+`.

<procedure>(opt MATCHER) => MATCHER</procedure>

Optional pattern. Analogous to `?`.

<procedure>(bind F P) => MATCHER</procedure>

Given a rule `P` and function `F`, returns a matcher that first
applies `P` to the input stream, then applies `F` to the returned
list of consumed tokens, and returns the result and the remainder of
the input stream.

Note: this combinator will signal failure if the input stream is
empty.

<procedure>(bind* F P) => MATCHER</procedure>

The same as `bind`, but will signal success if the input stream is
empty.

<procedure>(rebind F G P) => MATCHER</procedure>

Given a rule `P` and procedures `F` and `G`, returns a matcher
that first applies `F` to the input stream, then applies `P` to
the resulting stream, then applies `G` to the resulting list of
consumed elements and returns the result along with the remainder of
the input stream.

Note: this combinator will signal failure if the input stream is
empty.

<procedure>(rebind* F G P) => MATCHER</procedure>

The same as `rebind`, but will signal success if the input stream is
empty.

<procedure>(drop P) => MATCHER</procedure>

Given a rule `P`, returns a matcher that always returns an empty
list of consumed tokens when `P` succeeds.

### Lexer procedure

<procedure>(lex MATCHER ERROR STRING) => CHAR-LIST</procedure>

`lex` takes a pattern and a string, turns the string into a list of
streams (containing one stream), applies the pattern, and returns the
first possible match. Argument `ERROR` is a single-argument
procedure called when the pattern does not match anything.

## Examples

### A pattern to match floating point numbers

```scheme

;;  A pattern to match floating point numbers. 
;;  "-"?(([0-9]+(\\.[0-9]+)?)|(\\.[0-9]+))([eE][+-]?[0-9]+)? 

(define numpat
  (let* ((digit        (range #\0 #\9))
	 (digits       (pos digit))
	 (fraction     (seq (char #\.) digits))
	 (significand  (bar (seq digits (opt fraction)) fraction))
	 (exp          (seq (set "eE") (seq (opt (set "+-")) digits)))
	 (sign         (opt (char #\-))))
    (seq sign (seq significand (opt exp)))))
 
 (define (err s)
  (print "lexical error on stream: " s)
  (list))

 (lex numpat err "-123.45e-6")
```

## Version History

* 8.9 Ported to CHICKEN 5 and yasos collections interface
* 7.1 Bug fix in bind*  [thanks to Peter Bex]
* 7.0 Added bind* and rebind* variants of bind and rebind [thanks to Peter Bex]
* 6.1-6.2 Corrected behavior of the tok combinator so that the failure continuation is invoked upon end-of-input [thanks to Chris Salch]
* 6.0 Using utf8 for char operations
* 5.2 Ensure test script returns proper exit status
* 5.0-5.1 Added error continuation to the matcher interface and eliminated multiple stream matching
* 4.0 Implemented typeclass interface for abstracting over input sequences
* 3.8 Added procedure `star*` (greedy Kleene closure matching)
* 3.6 Added procedure redo [thanks to Christian Kellermann]
* 3.5 Bug fixes in bind [reported by Peter Bex]
* 3.3 Bug fixes in stream comparison
* 3.2 Improved input stream comparison procedures
* 3.1 Added rebind combinator and stream-unfold procedure 
* 3.0 Added an extension mechanism for input streams of different
  types (to be elaborated and documented in subsequent versions).
* 2.6 Added bind and drop combinators
* 2.5 The seq combinator checks whether the first parser in the sequence has failed
* 2.4 Added (require-library srfi-1); using lset<= instead of equal? in star
* 2.3 Bug fix in procedure range; added procedure cps-table
* 2.2 Bug fix in procedure star
* 2.1 Added procedure lst
* 2.0 Core procedures rewritten in continuation-passing style
* 1.5 Using (require-extension srfi-1)
* 1.4 Ported to Chicken 4
* 1.2 Added procedures try and tok (supersedes pred)
* 1.0 Initial release

## License

Based on the [SML lexer generator](http://www.standarddeviance.com/projects/combinators/combinators.html) by Thant Tessman.
>
>  Copyright 2009-2018 Ivan Raikov.
> 
> 
>  This program is free software: you can redistribute it and/or modify
>  it under the terms of the GNU General Public License as published by
>  the Free Software Foundation, either version 3 of the License, or
>  (at your option) any later version.
> 
>  This program is distributed in the hope that it will be useful, but
>  WITHOUT ANY WARRANTY; without even the implied warranty of
>  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
>  General Public License for more details.
> 
>  A full copy of the GPL license can be found at
>  <http://www.gnu.org/licenses/>.
>
