(require 'json)

(defvar covid-monitor-data-buffer "*covid-data*")
(defvar covid-monitor-require-feedback t)
(defvar covid-monitor-status-string "")
(defvar covid-monitor-update-timer nil)

(defcustom covid-monitor-update-interval 600
  "covid monitor update interval (sec)"
  :type 'integer)

(defcustom covid-monitor-alicloud-api-appcode ""
  "alicloud appcode for covid data api"
  :type 'string)

(defcustom covid-monitor-province-list '()
  "list of provinces to monitor"
  :type '(repeat string))

(defun covid-monitor-calculate-count (statistics)
  (if (not statistics) 0
    (let ((confirmed (gethash "totalConfirmed" statistics))
          (doubtful (gethash "totalDoubtful" statistics))
          (cured (gethash "totalCured" statistics))
          (death (gethash "totalDeath" statistics)))
      (- (+ confirmed doubtful) (+ cured death)))))

(defun covid-monitor-find-child (data-list key)
  (cl-find-if (lambda (x) (equal key (gethash "childStatistic" x)))
              data-list))

(defun covid-monitor-extract-province-imported-count (province-data)
  (covid-monitor-calculate-count (covid-monitor-find-child (gethash "cityArray" province-data) "境外输入")))

(defun covid-monitor-extract-total-imported-count (covid-raw-data)
  (apply '+ (cl-mapcar 'covid-monitor-extract-province-imported-count
                       (gethash "provinceArray" covid-raw-data))))

(defun covid-monitor-format-imported-count (imported-count)
  (if (= 0 imported-count) ""
    (format "(%d)" imported-count)))

(defun covid-monitor-format-total-data (covid-raw-data)
  (format " %d%s"
          (covid-monitor-calculate-count (gethash "country" covid-raw-data))
          (covid-monitor-format-imported-count (covid-monitor-extract-total-imported-count covid-raw-data))))

(defun covid-monitor-format-province-data (province-data)
  (format " %s %d%s"
          (gethash "childStatistic" province-data)
          (covid-monitor-calculate-count province-data)
          (covid-monitor-format-imported-count (covid-monitor-extract-province-imported-count province-data))))

(defun covid-monitor-extract-province-data-list (covid-raw-data)
  (let ((province-data-list (gethash "provinceArray" covid-raw-data)))
    (cl-mapcar (lambda (x) (covid-monitor-find-child province-data-list x)) covid-monitor-province-list)))

(defun covid-monitor-format-covid-status-string (covid-raw-data)
  (let ((province-data-list (covid-monitor-extract-province-data-list covid-raw-data)))
    (apply 'cl-concatenate 'string "COVID-19 全国" (covid-monitor-format-total-data covid-raw-data)
           (cl-mapcar 'covid-monitor-format-province-data province-data-list))))

(defun covid-monitor-extract-covid-data ()
  (with-current-buffer (get-buffer-create covid-monitor-data-buffer)
    (when (< 0 (length (buffer-string)))
      (let* ((json-object-type 'hash-table)
             (json-array-type 'list)
             (json-key-type 'string)
             (covid-raw-data (json-read-from-string (buffer-string))))
        (setq covid-monitor-status-string (covid-monitor-format-covid-status-string covid-raw-data))
        (force-mode-line-update 'all)))))

(defun covid-monitor-fetcher-callback (process event)
  (when (not (process-live-p process))
    (let ((exit-code (process-exit-status process)))
      (if (= 0 exit-code)
          (covid-monitor-extract-covid-data)
        (when covid-monitor-require-feedback
          (setq covid-monitor-require-feedback nil)
          (setq covid-monitor-status-string "Failed to fetch COVID-19 data"))))))

(defun covid-monitor-covid-data-update ()
  (with-current-buffer (get-buffer-create covid-monitor-data-buffer)
    (erase-buffer)
    (make-process :name "covid-data-fetcher"
                  :buffer covid-monitor-data-buffer
                  :command (list "curl" "-s"
                                 "https://ncovdata.market.alicloudapi.com/ncov/cityDiseaseInfoWithTrend"
                                 "-H" (format "Authorization: APPCODE %s" covid-monitor-alicloud-api-appcode))
                  :sentinel #'covid-monitor-fetcher-callback)))

(defun covid-monitor-force-update ()
  (interactive)
  (setq covid-monitor-status-string "Fetching COVID-19 data...")
  (setq covid-monitor-require-feedback t)
  (covid-monitor-covid-data-update))

(define-minor-mode covid-monitor-mode
  "covid-monitor-mode"
  :lighter ""
  (and covid-monitor-update-timer (cancel-timer covid-monitor-update-timer))
  (setq covid-monitor-update-timer nil)
  (setq covid-monitor-status-string "")
  (or global-mode-string (setq global-mode-string '("")))
  (when covid-monitor-mode
    (setq covid-monitor-status-string "Fetching COVID-19 data...")
    (or (memq 'covid-monitor-status-string global-mode-string)
        (setq global-mode-string
              (append global-mode-string '(covid-monitor-status-string))))
    (setq covid-monitor-update-timer
          (run-at-time "0 sec" covid-monitor-update-interval
                       'covid-monitor-covid-data-update))))

(provide 'covid-monitor-mode)
