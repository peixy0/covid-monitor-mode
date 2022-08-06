(require 'json)

(defvar covid-monitor-data-buffer "*covid-data-raw*")
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

(defun covid-monitor-extract-province-data (json-data area-name)
  (let* ((data-top-layer (gethash "data" json-data))
         (data-second-layer (gethash "diseaseh5Shelf" data-top-layer))
         (data-third-layer (gethash "areaTree" data-second-layer))
         (country-data (car data-third-layer))
         (province-data-list (gethash "children" country-data)))
    (cl-find-if (lambda (x) (equal area-name (gethash "name" x))) province-data-list)))

(defun covid-monitor-extract-province-data-list (json-data)
  (cl-mapcar (lambda (x) (covid-monitor-extract-province-data json-data x)) covid-monitor-area-list))

(defun covid-monitor-process-raw-data (raw-data)
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (extracted-json (json-read-from-string raw-data)))
    extracted-json))

(defun covid-monitor-format-area-data (data)
  (let* ((name (gethash "name" data))
         (today (gethash "today" data))
         (confirm-add (gethash "local_confirm_add" today))
         (wzz-add (gethash "wzz_add" today))
         (total (gethash "total" data))
         (confirmed (gethash "nowConfirm" total)))
    (format "%s %d(+%d|无症状+%d)" name confirmed confirm-add wzz-add)))

(defun covid-monitor-format-message (area-list)
  (format "COVID-19 %s "
          (string-join (cl-mapcar #'covid-monitor-format-area-data area-list) " ")))

(defun covid-monitor-process-data ()
  (with-current-buffer (get-buffer-create covid-monitor-data-buffer)
    (when (< 0 (length (buffer-string)))
      (let* ((raw-data (buffer-string))
             (json-data (covid-monitor-process-raw-data raw-data))
             (province-data-list (covid-monitor-extract-province-data-list json-data))
             (message (covid-monitor-format-message province-data-list)))
        (setq covid-monitor-status-string message)
        (force-mode-line-update 'all)))))

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
                                 "https://api.inews.qq.com/newsqa/v1/query/inner/publish/modules/list?modules=diseaseh5Shelf")
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
