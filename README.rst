gotham-theme
============

.. image:: https://raw.github.com/wasamasa/gotham-theme/master/img/gotham.png

..

    It's the colorscheme we set that defines us. (Batman)

About
-----

Gotham is a **very dark** Emacs color theme.  It's a port of the
`Gotham theme for Vim <https://github.com/whatyouhide/vim-gotham>`_
and tries adhering closely to the original.

Screenshot
----------

.. image:: https://raw.github.com/wasamasa/gotham-theme/master/img/scrot.png

Installation
------------

To manually install the theme, download ``gotham-theme.el`` and put it in
a suitable location such as ``~/.emacs.d/themes/``.  Add the following
to your init file:

.. code:: cl

    (add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))

Alternatively you can install the theme via `quelpa
<https://github.com/quelpa/quelpa>`_ with ``M-: (quelpa '(gotham-theme
:fetcher github :repo "wasamasa/gotham-theme"))``.

Once the theme is installed, you can enable it with ``M-x load-theme
RET gotham RET``.  To enable it automatically at startup, add the
following to your init file:

.. code:: cl

    (load-theme 'gotham t)

Contributing
------------

If you find bugs, have suggestions or any other problems, feel free to
report an issue on the issue tracker or hit me up on IRC, I'm always on
``#emacs``.  Patches are welcome, too, just fork, work on a separate
branch and open a pull request with it.

List of contributors:

`See here <https://github.com/wasamasa/gotham-theme/graphs/contributors>`_

Changelog
---------

`See here <https://github.com/wasamasa/gotham-theme/commits/master>`_
