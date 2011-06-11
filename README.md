xcode.el
========

This library attempts to ease the development of Xcode projects within Emacs.

Currently, you can do two things:
  1. Compile (build) the project
  2. Compile (build) and run the project
  
Installation
============

If you are using [el-get](https://github.com/dimitri/el-get), use the following recipe:

    (:name xcode
       :type git
       :url "https://github.com/mig/xcode.el.git"
       :load "xcode.el")

Traditional way:

    $ cd ~/.emacs.d/anywhere-you-want
    $ git clone git://github.com/mig/xcode.el
    
In your emacs config:

    (load "anywhere-you-want/xcode.el/xcode.el")
    
Usage
=====

This library provides two interactive functions:
  
    (xcode-compile)
    
and...

    (xcode-compile-and-run)

So you could bind those to keys for whatever mode you want. I've been having fun with MacRuby so I did something like this:

    (define-key ruby-mode-map (kbd "M-r b") 'xcode-compile)
    (define-key ruby-mode-map (kbd "M-r r") 'xcode-compile-and-run)
    
Notes
=====

At the moment I've only tested this library in the development version of Cocoa Emacs (24)

Additionally, there is no build failure checking as of now.

