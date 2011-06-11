(defun buffer-dir ()
  (file-name-directory buffer-file-name))

(defun parent-dir (dir)
  (expand-file-name (concat (file-name-as-directory dir) "..")))

(defun home-dir ()
  (expand-file-name "~/"))

(defun home-dir-p (dir)
  (equal dir (home-dir)))

(defun file-for-pattern-in-dir (pattern dir)
  (let ((match (directory-files dir nil pattern)))
    (when match
      (pop match))))

(defun dir-of-file-found-walking-up-fs (pattern &optional current-directory)
  (let (found-file)
    (when (null current-directory) (setq current-directory (buffer-dir)))
    (cond ((file-for-pattern-in-dir pattern current-directory) (expand-file-name current-directory))
          ((home-dir-p current-directory) nil)
          (t (dir-of-file-found-walking-up-fs pattern (parent-dir current-directory))))))

(defun xcode-project-dir ()
  (dir-of-file-found-walking-up-fs "xcodeproj"))

(defun xcode-project-debug-dir ()
  (expand-file-name (concat (xcode-project-dir) "/build/Debug/")))

(defun xcode-compile ()
  (interactive)
    (let (working-dir)
      (setq working-dir (xcode-project-dir))
      (cat-run (list "cd" working-dir "&&" "xcodebuild" "-configuration" "Debug"))))

(defun xcode-compile-and-run ()
  (interactive)
  (xcode-compile)
  (let (app cmd)
    (setq app (file-for-pattern-in-dir "app" (xcode-project-debug-dir)))
    (setq cmd (combine-and-quote-strings (list "open" (concat (xcode-project-debug-dir) app))))
    (when app
        (call-process-shell-command cmd))))
