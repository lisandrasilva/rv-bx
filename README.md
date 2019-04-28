# From Regular Expressions to Deterministic Finite Automata and back

## Conversion Algorithms

The present work has the implementation of the most popular algorithms to convert between a Regular Expression (RE) into a Deterministic Finite Automata (DFA) and back.

When converting a RE, the first step is to obtain a Non-Deterministic Finite Automata (NDFA), through one of the following algorithms:
- Thompson's Construction: implemented according to https://en.wikipedia.org/wiki/Thompson%27s_construction
- Glushkov's Construction Algorithm: implemented according to https://en.wikipedia.org/wiki/Glushkov%27s_construction_algorithm

After getting the NDFA, one need to obtain the Deterministic Finite Automata (DFA) through the: 
- Powerset construction algorithm: also implemented according https://en.wikipedia.org/wiki/Powerset_construction


To go from DFA to RE, there is the: 
- Kleen's Algorithm: implemented according to https://en.wikipedia.org/wiki/Kleene%27s_algorithm

## Bidirectional Transformation between NDFA and DFA

A bidirectional transformation (BX) consists of a pair of functions: a _get_ function that returns a view by extracting part of information from a source, and a _put_ function that accepts an updated view and the original source and produces an updated source that is consistent with the view. The pair of functions needs to satisfy the _well-behavedness_ laws: 

 - put s (get s) = s     (GetPut)
 - get (put s v) = v     (PutGet)

The present work implements a BX between NDFA and DFA, where the NDFA is the source and the DFA is the view. The NDFA used is the result from the Glushkov's Algorithm which means that it has no epsilon transitions, and the _get_ (from NDFA to DFA) function is the Powerset construction algorithm. A _put_ function receiving a NDFA and a (possibly modified) DFA was defined and returns the updated NDFA.

## Usage
### Commands to explore conversion algorithms and obtain graphical representations
Compile the file `Interpreter.hs` using the command

```bash
rv-bx: ghci Interpreter.hs
```

When wanting to convert a RE to a NDFA use the following command:

```bash
*Interpreter> ndfa = [algorithm] re
```
where algorithm can be `glushkov`or `thompson` and the `re` must be of the type in file `RegExp.hs`. Some examples are available in the file `Examples.hs`.

If you want to see the DFA of a given NDFA use the command `nonDet2Det`:

```bash
*Interpreter> dfa = nonDet2Det ndfa
```

Use `displayFA` when wanting to see the graphical representations of an automata. For instance:

```bash 
*Interpreter> displayFA $ ndfa
*Interpreter> displayFA $ dfa
```
  
It is possible to obtain both graphical representations at once through the command:
```bash
*Interpreter> getAutomata re
```

### Commands to explore BX
When wanting to explore the _put_ and _get_ functions of the BX run the command:

```bash
*Interpreter> menuBX re
```

This command will display both the NDFA and DFA to the given RE and also a menu. You can use the commands to change the view (DFA) and put it back into the NDFA with the command _put_.

As `re` is a RE present in the `Examples.hs` file, after running the above command one can run the following sequence of commands to edit the view and to put back the changes into the source:

```bash
addT [a1 a2] c [c1]
addT [c1] b [b3]
remT [a2] b [b3]
put
```
