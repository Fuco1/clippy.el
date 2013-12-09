clippy.el [![Paypal logo](https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=TAWNECQR3TTUY)
=========

This library implements rendering of popup box with "Clippy, the paper clip". You can make him say various things by calling `clippy-say` function. To hide the pop-up, simply invoke any command (move forward/backward, type, `C-g` etc., any event is recognized).

As inspiration, two functions are provided: `clippy-describe-function` and `clippy-describe-variable`. Bind any of these functions to a key, then while point is over a function/variable, call it. A popup with helpfull clippy will appear, telling you about the function/variable (using `describe-function` and `describe-variable` respectively).

This package depends on `pos-tip`.

Code is roughly based on: http://www.emacswiki.org/emacs/PosTip#toc3

The inspiration to write this package came from [http://www.geekzone.co.nz/foobar/5656](http://www.geekzone.co.nz/foobar/5656), a crazy discussion on #emacs and my terrible headache stopping me from doing anything more productive.

Screenshot
=========

![Clippy trying to be helpful](https://raw.github.com/Fuco1/clippy.el/master/clippy.png)

Alternative clippy art.

![Alternative clippy trying to be helpful](https://raw.github.com/Fuco1/clippy.el/master/clippy2.png)
