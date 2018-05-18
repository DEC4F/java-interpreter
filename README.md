# Overview
This is a Scheme implementation of the Java interpreter. It supports most Java language features, including __object type__, __exception throwing__, __dot operator__, __static & non-static declaration__, and so on.



This is NOT a [JVM](https://docs.oracle.com/javase/specs/jvms/se8/jvms8.pdf) implementation. Also, this does NOT support non-Integer number, method overloading, non-default constructors and abstract methods, as all these topics are beyond the scope of our course.

# Installation
1. `Configure`

We highly recommend using the Pretty Big interpreter that comes with [Dr.Racket](https://racket-lang.org/) to run our program. Other interpreter such as [MIT/GNU Scheme](https://www.gnu.org/software/mit-scheme/) and [Chez Scheme](https://github.com/cisco/ChezScheme) would also work but will fail some of the our test cases.

2. `Install`

```bash
$ git clone https://github.com/DEC4F/Java-Interpreter.git
$ cd Java-Interpreter
```

3. `Run`

When using one of the Scheme interpreter mentioned above, if you want to interpret a certain Java file:
```scheme
(load "Interpreter.scm")
(interpret filename)
```

Or if you simply want to test our program:
```scheme
(load "Test.scm")
(test)
```

`Test.scm` is script that automatically runs all of our test cases. For some issue we had with using box in the implementation, our interpreter didn't pass Test_13.

# License

This repository is licensed under the Gnu Public License 3.0.
