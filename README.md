# Assignment - Parsers and State

## Part 1: Imp Parser
In this first part, you will complete the parser for the Imp language.
Most of the parser has already been implemented.

**TODO:**  
Implement the `cWhileP` parser. The while command has the following syntax (EBNF):
```
cWhile = "while" boolExpr "do" cseq "endwhile"
```

Example:
```
q := 0;
r := m;
while (n <= r) do
  q := (q + 1);
  r := (r - n)
endwhile
```

In [`Main.hs`](./Lang/Imp/Main.hs), you can find the code that loads the content of a file, runs the parser and prints the output. Its `main` looks as follows:
```haskell  
main :: IO ()
main = do
    [fileName] <- getArgs
    content <- readFile fileName
    putStrLn "Parsed file:"
    print $ parseImp content
```

- To run the above code on a test file, execute the following command:  
`cabal run imp Lang/Test/intDiv.iml`
- To run the parser tests, execute the following command:  
`cabal test imp-test --test-show-details=direct --test-option=--match --test-option="4. Parser"`.

Hints:
- Get inspiration from the `if` command parser.
- You may want to add more tests to the `parseSpec` in [`Tests.hs`](./Lang/Test/Tests.hs) to ensure that your implementation works correctly.


## Part 2: Syntactic Sugar
Currently, our Imp language is somewhat limited. In this part, you will add syntactic sugar to the language.
Our `Expr` type only supports the relational operators `=` and less-than `<`. The `<=` operator is only available in the front end and desugared by the parser as follows:
`a <= b` is translated to `a < b || a = b`

**TODO:**  
Implement support for `>` and `>=` without changing the `Expr a` type.  
Add tests to `parseSpec` in [`Tests.hs`](./Lang/Test/Tests.hs) to ensure that your implementation works correctly.

- To run the parser tests, execute the following command:  
`cabal test imp-test --test-show-details=direct --test-option=--match --test-option="4. Parser"`.


## Part 3: Imp Interpreter Refactoring
In this part you will refactor the interpreter to use the `ImpCmd` monad.

```haskell
newtype ImpCmd a = ImpCmd { runState :: State -> Either String (State, a)}
```

The `ImpCmd` monad is a stateful computation which can fail with an error message.

**TODO:**  
Complete the implementation of the `execCmd` function in [`Interpreter.hs`](./Lang/Imp/Interpreter.hs).

Hint: Give it a try. If you run into a roadblock, here are the solutions in [rot13](https://rot13.com):

```
trgFgngr = VzcPzq $ \fg -> Evtug (fg, fg)
frgFgngr fg = VzcPzq $ \_ -> Evtug (fg, ())
nffvtaInevnoyr a i = zbqvslFgngr (Z.vafreg a i)

rkrp PFxvc           = cher ()
rkrp (PNffvta a i)   = qb
  inyhr <- yvsgErnqre $ riny i
  nffvtaInevnoyr a inyhr
rkrp (PVsGuRy o g r) = qb
  p <- yvsgErnqre $ riny o
  rkrp (vs p gura g ryfr r)
rkrp (PJuvyr o p)    = qb
  pbaq <- yvsgErnqre $ riny o
  vs pbaq gura qb
    rkrp p
    rkrp (PJuvyr o p) 
  ryfr cher ()
rkrp (PFrd pf) = zncZ_ rkrp pf
```


## Part 4: Imp Cmd Line Tool
In this part, you will implement a command line application for the Imp language. 
It takes a file name and variable values as command line arguments. 
It should parse the file and execute the program with the given variable values.
Finally, it should print the final state of the program.

Example:
```
> cabal run imp Lang/Test/intDiv.iml m=6 n=2
Up to date
Input State:
"m" -> 6
"n" -> 2
Result State:
"m" -> 6
"n" -> 2
"q" -> 3
"r" -> 0
```

**TODO:**  
Implement the command line tool in the file [`Main.hs`](./Lang/Imp/Main.hs).


## Part 5: Pretty Printer (Optional)
In this part, you will implement a pretty printer for the language. The pretty printer should print the program in a nice and consistent format. This can be used to automatically format the source code.

Integrate the pretty printer into the command line tool. The driver should have a flag `--pretty` which formats the program and prints it to stdout. Use the following command to run the pretty printer on the `intDiv.iml` file:  
`cabal run imp -- --pretty Lang/Test/intDiv.iml`

**TODO:**  
Implement the pretty printer in the file [`Pretty.hs`](./Lang/Imp/Pretty.hs).
