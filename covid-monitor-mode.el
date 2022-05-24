(require 'json)

(defvar covid-monitor-data-buffer "*covid-data-raw*")
(defvar covid-monitor-area-stat-buffer "*covid-area-stat*")
(defvar covid-monitor-data-anchor-start "try { window.getAreaStat = ")
(defvar covid-monitor-data-anchor-end "}catch(e){}")
(defvar covid-monitor-require-feedback t)
(defvar covid-monitor-status-string "")
(defvar covid-monitor-update-timer nil)

(defcustom covid-monitor-update-interval 600
  "covid monitor update interval (sec)"
  :type 'integer)

(defcustom covid-monitor-curl-path "curl"
  "path for curl binary"
  :type 'string)

(defcustom covid-monitor-area-list '()
  "list of areas to monitor"
  :type '(repeat string))

(defun covid-monitor-extract-area-stat()
  (goto-char 1)
  (let* ((anchor-start (search-forward covid-monitor-data-anchor-start))
         (anchor-end (- (search-forward covid-monitor-data-anchor-end) (length covid-monitor-data-anchor-end))))
    (copy-region-as-kill anchor-start anchor-end)
    (with-current-buffer (get-buffer-create covid-monitor-area-stat-buffer)
      (erase-buffer)
      (yank))))

(defun covid-monitor-find-city-data (city-list area-name)
  (cl-find-if (lambda (x) (equal area-name (gethash "cityName" x))) city-list))

(defun covid-monitor-find-area-data (province-list area-name)
  (if (not (eq nil province-list))
      (let ((area (car province-list))
            (rest (cdr province-list)))
        (if (equal area-name (gethash "provinceShortName" area))
            area
          (let ((city (covid-monitor-find-city-data (gethash "cities" area) area-name)))
            (if (not (eq nil city))
                city
              (covid-monitor-find-area-data rest area-name)))))))

(defun covid-monitor-process-json-data (json-data area-name)
  (let* ((area (covid-monitor-find-area-data json-data area-name))
         (cities (gethash "cities" area))
         (imported (covid-monitor-find-city-data cities "境外输入")))
    (puthash "areaName" area-name area)
    (if (not (eq nil imported))
        (puthash "currentImportedCount" (gethash "currentConfirmedCount" imported) area))
    area))

(defun covid-monitor-process-raw-data ()
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (extracted-json (json-read-from-string (buffer-string))))
    (cl-mapcar (lambda (x) (covid-monitor-process-json-data extracted-json x)) covid-monitor-area-list)))

(defun covid-monitor-format-area-data (area)
  (let ((name (gethash "areaName" area))
        (currentConfirmed (gethash "currentConfirmedCount" area))
        (imported (gethash "currentImportedCount" area)))
    (if (eq nil imported)
        (format "%s %d" name currentConfirmed)
      (format "%s %d(%d)" name currentConfirmed imported))))

(defun covid-monitor-format-message (area-list)
  (format "COVID-19 %s "
          (string-join (cl-mapcar #'covid-monitor-format-area-data area-list) " ")))

(defun covid-monitor-process-data ()
  (with-current-buffer (get-buffer-create covid-monitor-data-buffer)
    (when (< 0 (length (buffer-string)))
      (covid-monitor-extract-area-stat)))
  (with-current-buffer (get-buffer-create covid-monitor-area-stat-buffer)
    (setq covid-monitor-status-string
          (covid-monitor-format-message (covid-monitor-process-raw-data))))
  (force-mode-line-update 'all))

(defun covid-monitor-fetch-data-callback (process event)
  (when (not (process-live-p process))
    (let ((exit-code (process-exit-status process)))
      (if (= 0 exit-code)
          (covid-monitor-process-data)
        (when covid-monitor-require-feedback
          (setq covid-monitor-require-feedback nil)
          (setq covid-monitor-status-string "Failed to fetch COVID-19 data "))))))

(defun covid-monitor-fetch-data ()
  (with-current-buffer (get-buffer-create covid-monitor-data-buffer)
    (erase-buffer)
    (make-process :name "covid-data-fetcher"
                  :buffer covid-monitor-data-buffer
                  :command (list covid-monitor-curl-path "-s"
                                 "https://ncov.dxy.cn/ncovh5/view/pneumonia")
                  :sentinel #'covid-monitor-fetch-data-callback)))

(defun covid-monitor-force-update ()
  (interactive)
  (setq covid-monitor-status-string "Fetching COVID-19 data... ")
  (setq covid-monitor-require-feedback t)
  (covid-monitor-fetch-data))

(define-minor-mode covid-monitor-mode
  "covid-monitor-mode"
  :lighter ""
  (and covid-monitor-update-timer (cancel-timer covid-monitor-update-timer))
  (setq covid-monitor-update-timer nil)
  (setq covid-monitor-status-string "")
  (or global-mode-string (setq global-mode-string '("")))
  (when covid-monitor-mode
    (setq covid-monitor-status-string "Fetching COVID-19 data... ")
    (or (memq 'covid-monitor-status-string global-mode-string)
        (setq global-mode-string
              (append global-mode-string '(covid-monitor-status-string))))
    (setq covid-monitor-update-timer
          (run-at-time "0 sec" covid-monitor-update-interval
                       'covid-monitor-fetch-data))))

(provide 'covid-monitor-mode)
