# class-filter

This library uses Apache BCEL to support class dependency resolution.

```
// Returns all transitively dependent classes for the given input classes
EssentialClassFilter.getClassNames(inputClasses: String*): Array[String]

// Returns a dependency graph mapping each class to its dependencies
EssentialClassFilter.getDependencyGraph(inputClasses: String*): ConcurrentHashMap[String, Array[String]] 
```

See 'src/classfilter/EssentialClassFilterTests.scala' for examples.

Requirements

Inspected code must be compiled with compiler option '-g'. Maven enables this by default; 
for Mill and similar tools, set it explicitly.(see https://docs.oracle.com/en/java/javacard/3.1/guide/setting-java-compiler-options.html).
