#class.el --- an OOP System in Emacs Lisp

class.el is an OOP system in Emacs Lisp. Its syntax comes from
[Python](http://docs.python.org/tutorial/classes.html).

## A Simple Example

Define a class `My-Class`:

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

`My-Class` is now defined as a function, which can be called to create and
initialize a new instance:

    (setq obj (My-Class "hello"))
    (setq obj 'arg)
    => "hello"

`obj` is now an instance of `My-Class`. Call the method `set-arg` to
change its initial value of `arg`:

    (@ obj 'set-arg (concat (@ obj 'arg) " world"))

Call the method `print-arg` to print out `arg`:

    (@ obj 'print-arg)
    => "hello world"

## Class Definition Syntax

    (class NAME BASES &optional DOC &rest SLOTS)

See more information about the definition of `class`, please `C-h f
class`.

## Inheritance

Define a class `A` inherited from the class `B` is easy as the following:

    (class A (B)
      ...)

This library supports multi inheritance as well. A class `A` with
multiple base classes `B`, `C` and `D` looks like this:

    (class A (B C D)
      ...)

Note: The method resolution order is **depth-first**. For example:

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

Since the method `foo`'s search order is `B, O, C, D`, the `foo` called
above is from class `O`.

Custom classes are recommanded to inherit from the built-in class `Object`,
which provides some useful methods such as `get-member`, default `init`,
etc.

## Access to class members

The following modifiers are supported define the type of members:

	private, protected, staticmethod, classmethod
