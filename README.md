# Juno
Juno is a computer language experimentation kit. It's meant to provide a handy starting point from which you can 
build your own scripping language or experiment with ideas. Juno will provide a number of starting languages
that you can use as a starting point for your own language with no need to implement the basics, such as
parsing and evaluating expressions.

I don't think it's quite ready for primetime so I haven't fully documented it yet, but feel free to poke around and
if you have suggestions they are welcome. If you'd like to experiment with Juno I highly suggest reading the rest of this
and then starting with the Expressionator language, and the unit tests.

Under src/main/scala/com/twilightfair/juno/language there will be a number of sample languages, right now there are
only three but you'll be able to pick the one that best matches what you want to do, copy it into your own language 
and start creating from there.

The three samples in juno/language are Expressionator: an expression executor, Test: an outdated test, and ASoft which
implements about 90% of Applesoft BASIC. Why Applesoft? Because I wasn't sure if Juno would be able to handle the
freeform nature of a true basic with GOTO's, GOSUB's, NEXT arrays and DATA blocks with optional quoted strings. It did uncover
some issues with Juno so it was a good choice. More sample languages are to come.


## Basic functionality

A Juno Language object defines which Matchers will be used by the Lexx tokenizer and then defines which sub Parser 
will be executed on the event of a given token. The sub Parser will likely (but not necessarily) generate an Element 
as a result of it's parsing. A sub Parser can recursively call parse() on the next token(s) if it needs to and store 
the resulting Elements in it's generated Element. 

The end result of a parse is a tree structure of Elements representing the code parsed. Each Element has a run() 
method, executing the code is as simple as calling run() on the root node of the Element tree (well, maybe not that 
simple in many cases, you need to be a little trickier to avoid causing a stack/heap overflow if your language supports 
rampant recursion).

Note that Elements can have other methods for doing other things as well. For example the Expressionator languages 
Elements have graph() methods which create a PlantUML representation of the Element tree. Very handy for debugging. For 
example if we have the Expressionator parse the string "6 + (2 - 1) * 4" calling graph() on the root Element results in 
(you can have this rendered into a graph at http://plantuml.sourceforge.net/):

```
class Integer1{
  6
}
package Block1{
class Integer2{
	2
}
class Integer3{
	1
}
class InfixMath1{
	"-"
}
InfixMath1 <|-- Integer2: left
InfixMath1 <|-- Integer3: right
}
class Integer4{
	4
}
class InfixMath2{
	"*"
}
InfixMath2 <|-- Block1: left
InfixMath2 <|-- Integer4: right
class InfixMath3{
	"+"
}
InfixMath3 <|-- Integer1: left
InfixMath3 <|-- InfixMath2: right
```

## Basic structure

juno/lexx is the string tokenizer used by Juno. In juno/lexx/matchers is a set of pre-made matchers for numbers, identifiers, 
symbols ect. You can add your own if you wish (ASoft uses a special one for unquoted DATA blocks, for example).

juno/parse is the Pratt inspired recursive decent parser used by Juno. 

juno/runtime contains utility objects for running scripts. But most of the actual execution is handled by language Elements 
themselves
