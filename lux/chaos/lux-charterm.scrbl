#lang scribble/manual
@(require (for-label charterm-fork
                     lux/chaos
                     lux/chaos/charterm))

@title{lux-charterm: amazing terminal programs}
@author{Jay McCarthy}

@defmodule[lux/chaos/charterm]

The @racketmodname[lux/chaos/charterm] module provides a @tech[#:doc
'(lib "lux/scribblings/lux.scrbl")]{chaos} for terminal
interfaces. This builds on @racketmodname[charterm], which is a
temporary fork of the @litchar{CharTerm} Planet package.  Refer to the
@link["http://www.neilvandyke.org/racket-charterm/"]{original's
documentation} for interacting with @racketmodname[charterm].

@defproc[(make-charterm) chaos?]{

Returns a @tech[#:doc '(lib "lux/scribblings/lux.scrbl")]{chaos} that manages the character terminal.

The values that @racket[word-event] is called with are those returned
by @racket[charterm-read-key], which are characters or symbols for
special keys, like @racket['escape] for the @litchar{ESC} key.

The values that @racket[word-output] should return are functions that
are called with the @racket[current-charterm] set to the charterm
managed by @racket[make-charterm], so that charterm functions like
@racket[charterm-display], @racket[charterm-cursor], and
@racket[charterm-clear-screen] work without specifying the charterm
argument.}
