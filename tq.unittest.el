(require 'cl)
(require 'tq)

(defvar tq-unittest-suit)

(defun tq-ut-foo ()
  (assert nil))
(add-hook 'tq-unittest-suit 'tq-ut-foo)

(eval-when-compile
  (run-hooks 'tq-unittest-suit))
