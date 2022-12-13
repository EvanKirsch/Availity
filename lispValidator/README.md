Building & Running lispValidator 
---
Written in java 11. Nothing to crazy here so you should be good compiling/running with anything >= 8

- Building
  1. `cd <path to repo>/lispValidator/src`
  1. `javac *.java`

- Running
  1. `java LispValidator <expression>`

- Build/Run Example:
1. `cd Repos/Availity/lispValidator/src`
1. `javac *.java`
1. `java LispValidator "(define (state-create-empty) '())"`

- File Flag `-f`
    - To make testing easier I've also added a 'file' flag `-f`. This can allow a text file to be passed as a parameter rather than a string.
    - Example: `java LispValidator -f ../test/artc.rkt`

Assumptions
---
In a 'valid' Lisp program the following is true:
1. All open parentheses have a corresponding closing parentheses 
2. There can be zero to N tokens between the open and close parentheses (ie tokens that aren't `(` or `)` don't matter)
3. There are no escape characters

Examples of valid LISP programs:
- ` `
- `(())`
- `(())()`

Examples of not valid LISP programs:
- `((((`
- `)(())(`
- `(())(`
