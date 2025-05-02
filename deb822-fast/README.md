Lossy parser for deb822 format.

This parser is lossy in the sense that it will discard whitespace and comments
in the input.

This parser is optimized for speed and memory usage. For editing
purposes, you may want to use a more feature-complete parser like
``deb822-lossless``.
