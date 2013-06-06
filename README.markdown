#class.el --- an OOP System in Emacs Lisp

class.el is an OOP system in Emacs Lisp. Its syntax comes from
[Python](http://docs.python.org/tutorial/classes.html).

## A Simple Example

Define a class `My-Class`:

```elisp
     (class My-Class ()

       (defun init (self arg)
         "This method initialize the instance"
         (@ self 'arg arg))

       (defun set-arg (self new-arg)
         "Set arg"
         (@ self 'arg new-arg))

       (defun print-arg (self)
         "Print out the member `arg'"
         (message (@ self 'arg))))
```

`My-Class` is now defined as a function, which can be called to create and
initialize a new instance:

```elisp
    (setq obj (My-Class "hello"))
    (@ obj 'arg)
    => "hello"
```

`obj` is now an instance of `My-Class`. Call the method `set-arg` to
change its initial value of `arg`:

```elisp
    (@ obj 'set-arg (concat (@ obj 'arg) " world"))
```

Call the method `print-arg` to print out `arg`:

```elisp
    (@ obj 'print-arg)
    => "hello world"
```

## Class Definition Syntax

```elisp
    (class NAME BASES &optional DOC &rest SLOTS)
```

See more information about the definition of `class`, please `C-h f
class`.

## Inheritance

Define class `A` that inherits from class `B`:

```elisp
    (class A (B)
      ...)
```

This library supports multi inheritance as well. Class `A` with
multiple bases `B`, `C` and `D` looks like this:

```elisp
    (class A (B C D)
      ...)
```

Note: The method resolution order is **depth-first**. For example:

```elisp
    (class O ()
      (defun foo (self) "from class O"))
    (class B (O))
    (class C (O)
      (defun foo (self) "from class C"))
    (class D (O)
      (defun init (self)))
    (class A (B C D))

    (setq a (A))
    (@ a 'foo)
    => "from class O"
```

Since the search order of method `foo` is `B, O, C, D`, the `foo` called
above is from class `O`.

Custom classes are supposed to inherit from the built-in class `Object`,
which provides some useful methods such as `get-member`, default `init`,
etc.

## Access to Members

Like other OOP languages, class members can be private, protected or
public. To specify the access level for class members, you can use the
following form:

    (MODIFIER
      SLOTS...)

where `MODIFIER` can be `private`, `protected` or `public` (default).

## Static and Class Methods

Static methods do not receive an implicit first argument, while class
methods receives the class as its first argument. They are defined
inside the form starting with modifier `staticmethod` and `classmethod`
respectively, for example:

```elisp
    (class Class (Object)
      ...

      (staticmethod
        (defun foo ()
          (print "greet from static method")))

       (private
         (setq var 10))
       
       (classmethod
         (defun bar (cls)
           (1+ (@ cls 'var))))

      ...)

      (@ Class 'foo)
      => "greet from static method"
      
      (@ Class 'bar)
      => 11
```
