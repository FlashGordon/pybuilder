pybuilder
=========

elisp code for emacs python development

1. Creates build server for python project, and runs unittests in the
    background and reports status in emacs using nosetests

2. Creates coverage reports after each unittest run and marks up
   project buffers accordingly, using coverage.

3. Integrates pyflakes with flymake.

4. Dependencies

    Elisp:  python-mode, fringe-helper
    Python: pep8, autopep8, jedi, epc
