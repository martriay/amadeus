amadeus
=======

Musical harmony utility
Usage:
-----
Load main.hs into ghci and start asking for scales
```haskell
Prelude> :l main.hs 
[1 of 1] Compiling Main             ( main.hs, interpreted )
Ok, modules loaded: Main
*Main> mayor "do"
["Do","Re","Mi","Fa","Sol","La","Si"]
*Main> menor "FA"
["Fa","Sol","Lab","Sib","Do","Reb","Mib"]
*Main> mixolidio "soL"
["Sol","La","Si","Do","Re","Mi","Fa"]
*Main> locrio "re#"
["Re#","Mi","Fa#","Sol#","La","Si","Do#"]

```
Works with latin notation (Do, Re, Mi, etc) and suppors the following modes: `jonico` (also works with `mayor`), `dorico`, `frigio`, `lidio`, `mixolidio`, `eolico` (also works with `menor`), `locrio`
