;; gpm-review.el -- review with org-capture with gpm (git project manager)
;;
;; Copyright (c) 2018 Yann Esposito
;;
;; Author: Yann Esposito <yann.esposito@gmail.com>
;; Keywords: git
;; Version: 0.0.1
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:

;; Provide elisp functions in emacs to help code review within
;; gpm (git project manager)

;;; Code:

(require 'url)
(defun get-prop (prop-name)
  (org-with-point-at org-babel-current-src-block-location
    (org-entry-get nil prop-name t)))

(defun gen-review-file-name ()
  (let* ((branch (get-prop "BRANCH"))
         (reviewer (get-prop "REVIEWER"))
         (review-file-name ))
    (concat branch "-" reviewer ".org")))

(defun start-review ()
  "to be called at the beginning of the review"
  (let* ((review-file-name (gen-review-file-name))
         (review-dir "~/.reviews/")
         (local-review (concat "file:../reviews/" review-file-name))
         (global-review (concat review-dir review-file-name)))
    (url-copy-file local-review global-review)
    (setq org-capture-templates
       `(("c" "Change requested" entry (file+headline global-review)
            "* CHANGE_REQUESTED %?\n  %i\n  %a")
         ("q" "question" entry (file+headline global-review)
            "* QUESTION %?\n  %i\n  %a")
         ("r" "refused" entry (file+headline global-review)
            "* REFUSED %?\n  %i\n  %a")))
   (setq org-default-notes-file global-review)))

(defun end-review ()
  "to be call when the reviewer ends its review."
  (let* ((review-file-name (gen-review-file-name))
         (review-dir "~/.reviews/")
         (local-review (concat "file:../reviews/" review-file-name))
         (global-review (concat review-dir review-file-name)))
    (url-copy-file global-review local-review)
    (setq org-default-notes-file global-review)))

(provide 'gpm-review)
;;; gpm-review.el ends here
