(defun buffer-dir ()
  "The directory that your current buffer's file lives"
  (file-name-directory buffer-file-name))

(defun parent-dir (dir)
  "The parent directory of the dir argument"
  (expand-file-name (concat (file-name-as-directory dir) "..")))

(defun home-dir ()
  "Your home directory"
  (expand-file-name "~/"))

(defun home-dir-p (dir)
  "Determines if dir argument is your home directory"
  (equal dir (home-dir)))

(defun file-for-pattern-in-dir (pattern dir)
  "Returns the first matching filename for pattern argument in given directory"
  (let ((match (directory-files dir nil pattern)))
    (when match
      (pop match))))

(defun dir-of-file-found-walking-up-fs (pattern &optional current-directory)
  "Returns the path of the first directory containing a file whose name matches the pattern argument.
   If no file is found, it goes up into the parent directory and so on until a file is found or it
   reaches your home directory."
  (let (found-file)
    (when (null current-directory) (setq current-directory (buffer-dir)))
    (cond ((file-for-pattern-in-dir pattern current-directory) (expand-file-name current-directory))
          ((home-dir-p current-directory) nil)
          (t (dir-of-file-found-walking-up-fs pattern (parent-dir current-directory))))))

(defun xcode-project-dir ()
  "Returns the root directory of the xcode project whose file is loaded in the current buffer"
  (dir-of-file-found-walking-up-fs "xcodeproj"))

(defun xcode-project ()
  "Returns the name of the xcodeproj directory"
  (file-for-pattern-in-dir "xcodeproj" (xcode-project-dir)))

(defun xcode-project-debug-dir ()
  "Returns the debug build directory of the xcode project"
  (expand-file-name (concat (xcode-project-dir) "/build/Debug/")))

(defun xcode-open-project-in-xcode ()
  "Opens the xcode project in Xcode.app"
  (interactive)
  (let (working-dir)
    (setq working-dir (xcode-project-dir))
    (cat-run (list "cd" working-dir "&&" "open" (xcode-project)))))

(defun xcode-compile ()
  "Builds the xcode project whose file is loaded in the current buffer"
  (interactive)
    (let (working-dir)
      (setq working-dir (xcode-project-dir))
      (cat-run (list "cd" working-dir "&&" "xcodebuild" "-configuration" "Debug"))))

(defun xcode-compile-and-run ()
  "Builds the xcode project and subsequently launches the built application"
  (interactive)
  (xcode-compile)
  (let (app cmd)
    (setq app (file-for-pattern-in-dir "app" (xcode-project-debug-dir)))
    (setq cmd (combine-and-quote-strings (list "open" (concat (xcode-project-debug-dir) app))))
    (when app
        (call-process-shell-command cmd))))
