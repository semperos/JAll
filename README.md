# JAll

JAll is a powerful set of tools for writing Clojure, JRuby or Scala *inside your Java source code* cleanly and easily.

## Components

 * Parser/Compiler (this project)
 * [Maven plugin](https://github.com/semperos/jall-maven-plugin)

## What does JAll look like? ##

JAll allows you to write snippets of Alternative JVM (AJVM) languages in your Java source files. Here's what a method definition for a Clojure JAll method looks like:

```java
package com.example;

public class Foo {
    public static Integer squareNum(Integer x) {
        !def_clj square-num(x : Integer) : Integer {{
          (* x x)
        }}
        return !clj_square-num(x);
    }
    ....
```

The JAll snippet is transformed at pre-compilation time to the following:

```clj
(defn -squareNum
  [x]
  (* x x))
```

This is part of a file that uses Clojure's Java interop mechanisms to generate a class that Java can consume. Your original Java source code is transformed as follows:

```java
package com.example;

import com.example.FooClj;

public class Foo {
    public static Integer squareNum(Integer x) {
    /*
        !def_clj square-num(x : Integer) : Integer {{
          (* x x)
        }}
    */
        return new com.example.FooClj().squareNum(x);
    }
    ....
```

This is a high-level summary of how JAll processes a file. For more details, you may reference the source code, until the Github wiki is fleshed out.

## Installation

Until JAll ends up in a Maven repository, you need to install it locally.

First, [install Leiningen 1.7.1](https://github.com/technomancy/leiningen#readme), then run the following:

```
lein install
```

If you're going to distribute a JAR of JAll to others, do as follows:

```
lein pom
lein jar
```

Make sure to distribute both the `pom.xml` file and the JAR. To do a local Maven install of this JAR and its POM, do as follows:

```
mvn install:install-file -DpomFile=pom.xml -Dfile=jall-0.1.0-SNAPSHOT.jar -DgroupId=com.semperos -DartifactId=jall -Dversion=0.1.0-SNAPSHOT -Dpackaging=jar
```

This then allows you to use [the jall-maven-plugin](https://github.com/semperos/jall-maven-plugin), which is the recommended way to run the JAll compiler if you're not interested in hacking on it yourself.

## License

Copyright © 2012 Daniel Gregoire

Distributed under the Eclipse Public License, the same as Clojure. See the `epl-v10.html` file at the root of this distribution for more details.
