clippy.el
=========

Show tooltip with function documentation at point.

Bind the function `clippy-describe-function`, then while point is
over a function, call it.  A popup with helpfull clippy will
appear. To make it go away, simply invoke any command (move forward/backward, type, `C-g` etc.).

This package depends on `pos-tip`.

Code is roughly based on: http://www.emacswiki.org/emacs/PosTip#toc3

The inspiration to write this package came from [http://www.geekzone.co.nz/foobar/5656](http://www.geekzone.co.nz/foobar/5656), a crazy discussion on #emacs and my terrible headache stopping me from doing anything more productive.

Screenshot
=========

![Clippy trying to be helpful](https://raw.github.com/Fuco1/clippy.el/master/clippy.png)

Alternative clippy art.

![Alternative clippy trying to be helpful](https://raw.github.com/Fuco1/clippy.el/master/clippy2.png)
