;;; **ecco** is a port of docco. It renders
;;;
;;; * comments through markdown
;;;
;;; * code through pygments or through emacs's built-int
;;;   `htmlfontify', in case pygments isn't available or the user
;;;   set `ecco-use-pygments' to `nil'
;;;
(require 'newcomment)
(require 'htmlfontify)


;;; Parsing
;;; -------
;;;
;;; The idea is to gather pairs of comments and code snippets
;;; and render them
;;;
(defun ecco--gather-groups ()
  "Returns a list of conses of strings (COMMENT . SNIPPET)"
  (save-excursion
    (goto-char (point-min))
    (let ((stop nil)
          (comments nil)
          (snippets nil)
          (mode major-mode))
      ;; Maybe this could be turned into a `loop`...
      ;;
      (while (not stop)
        ;; Collect the next comment
        ;;
        (let ((start (point)))
          (comment-forward (point-max))
          (let ((comment (buffer-substring-no-properties  start (point))))
            (with-temp-buffer
              (funcall mode)
              (loop for fn in ecco-comment-cleanup-functions
                    do (setq comment (funcall fn comment)))
              (insert comment)
              (uncomment-region (point-min) (point-max))
              (skip-chars-backward " \t\r\n")
              
              (push (buffer-substring-no-properties  (point-min) (point)) comments))))
        ;; Collect the next code snippet
        ;;
        (let ((start (line-beginning-position)))
          (comment-search-forward (point-max) t)
          (let ((comment-beginning (comment-beginning)))
            (if comment-beginning
                (goto-char comment-beginning)
              (setq stop t)))
          (push (buffer-substring start
                                  (save-excursion
                                    (skip-chars-backward " \t\r\n")
                                    (point)))
                snippets)))
      ;; Return this a list of conses
      ;;
      (reverse (map 'list #'cons
                    comments
                    snippets)))))


;;; Rendering
;;; ---------
;;;
;;; There are two types of rendering:
;;;
;;; * a "blob" render is an optimization used for markdown and pygments (if
;;;   that is in use). It consists of joining strings using a divider,
;;;   rendering and splitting them using another divider, and should
;;;   effectively be equivalent to piping each string through the external
;;;   process, which is very slow.
;;;
;;; * if pygments is not in use, `htmlfontify-string' will take care of the
;;;   job, and we don't use blob rendering here.
;;;
(defun ecco--render-groups (groups)
  (let ((comments
         (ecco--blob-render (mapcar #'car groups)
                            (ecco--markdown-dividers)
                            #'(lambda (text)
                                (ecco--pipe-text-through-program text ecco-markdown-program))))
        (snippets
         (cond (ecco-use-pygments
                (ecco--blob-render (mapcar #'cdr groups)
                                   (ecco--pygments-dividers)
                                   #'(lambda (text)
                                       (let ((render (ecco--pipe-text-through-program text
                                                                                      (format "%s %s -f html"
                                                                                              ecco-pygmentize-program
                                                                                              (ecco--lexer-args)))))
                                         (setq render
                                               (replace-regexp-in-string "^<div class=\"highlight\"><pre>" "" render))
                                         (replace-regexp-in-string "</pre></div>$" "" render)))))
               (t
                (let ((hfy-optimisations (list 'keep-overlays
                                               'merge-adjacent-tags
                                               'body-text-only)))
                  (mapcar #'htmlfontify-string (mapcar #'cdr groups)))))))
    (map 'list #'cons comments snippets)))


(defun ecco--blob-render (strings dividers renderer)
  (split-string (funcall renderer
                         (mapconcat #'identity strings (car dividers)))
                (cdr dividers)))

;;; **ecco** uses `shell-command-on-region' to pipe to external processes
;;; 
(defun ecco--pipe-text-through-program (text program)
  (with-temp-buffer
    (insert text)
    (shell-command-on-region (point-min) (point-max) program (current-buffer) 'replace)
    (buffer-string)))


;;; User options
;;; ------------
;;;
(defvar ecco-comment-cleanup-functions '(ecco-backtick-and-quote-to-double-backtick))

(defun ecco-backtick-and-quote-to-double-backtick (text)
  (replace-regexp-in-string "`\\([^\n]+?\\)'" "`\\1`" text))
;;;
;;; This group controls the use of pygments.
;;; 
(defvar ecco-use-pygments t)
(defvar ecco-pygmentize-program "pygmentize")
(defvar ecco-pygments-lexer 'guess)
(defvar ecco-pygments-lexer-table
  '((lisp-mode . "cl")
    (emacs-lisp-mode . "cl")
    (sh-mode . "sh")
    (c-mode . "c")))

(defun ecco--lexer-args ()
  (cond
   ((eq ecco-pygments-lexer 'guess)
    (let ((lexer (cdr (assoc major-mode ecco-pygments-lexer-table))))
      (if lexer
          (format "-l %s" lexer)
        "-g")))
   (ecco-pygments-lexer
    (format "-l %s" ecco-pygments-lexer))
   (t
    "-g")))

(defun ecco--pygments-dividers ()
  (let* ((mode major-mode)
         (snippet-divider (with-temp-buffer
                            (funcall mode)
                            (insert "ECCO-SNIPPET-DIVIDER")
                            (comment-region (point-min) (point-max))
                            (buffer-string))))
    (cons (format "\n\n%s\n\n" snippet-divider)
          (format "\n*<span class=\"c.?\">%s</span>\n*" snippet-divider))))


;;; This group controls the use of markdown
;;; 
(defvar ecco-markdown-program "markdown")

(defun ecco--markdown-dividers ()
  (cons "\n\n##### ECCO-COMMENT-DIVIDER\n\n"
        "\n*<h5>ECCO-COMMENT-DIVIDER</h5>\n*"))



;;; Main entry point
;;; ----------------
;;;
(defun ecco ()
  (interactive)
  (let* ((groups (ecco--gather-groups))
         (rendered-groups (ecco--render-groups groups))
         (title (buffer-name (current-buffer))))
    (with-current-buffer
        (get-buffer-create (format "*ecco for %s*" title))
      (erase-buffer)
      (insert (format "
<!DOCTYPE html>
<html>
<head>
    <meta http-eqiv='content-type' content='text/html;charset=utf-8'>
    <title>%s</title>
    <link rel=stylesheet href=\"http://jashkenas.github.com/docco/resources/docco.css\">
</head>
<body>
<div id=container>
    <div id=background></div>
    <table cellspacing=0 cellpadding=0>
    <thead>
      <tr>
        <th class=docs><h1>%s</h1></th>
        <th class=code></th>
      </tr>
    </thead>
    <tbody> " title title))
      ;; iterate the groups collected before
      ;;
      (dolist (group rendered-groups)
        (insert "<tr><td class='docs'>")
        (insert (car group))
        (insert "</td><td class='code'><div class='highlight'><pre>")
        (insert (cdr group))
        (insert "</pre></div></td></tr>"))
      (insert "</tbody>
    </table>
</div>
</body>
</html>")
      (goto-char (point-min))
      (if (y-or-n-p "Launch browse-url-of-buffer?")
          (browse-url-of-buffer)
          (pop-to-buffer (current-buffer))))))


;;; Debug functions
;;; ---------------
;;;
;;; for now, the only debug function is `ecco--gather-groups-debug`
(defun ecco--gather-groups-debug ()
  (interactive)
  (let ((groups (ecco--render-groups (ecco--gather-groups))))
    (with-current-buffer
        (get-buffer-create (format "*ecco--debug for %s*" (buffer-name (current-buffer))))
      (erase-buffer)
      (dolist (group groups)
        (insert "\n-**- COMMENT -**-\n")
        (insert (car group))
        (insert "\n-**- SNIPPET -**-\n")
        (insert (cdr group)))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))
