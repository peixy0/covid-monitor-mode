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

(cl-defstruct covid-stat total imported)

(defun covid-monitor-format-covid-stat (covid-stat)
  (let ((total (covid-stat-total covid-stat))
        (imported (covid-stat-imported covid-stat)))
    (if (= 0 imported)
        (format "%d" total)
      (format "%d(%d)" total imported))))

(defun covid-monitor-format-covid-status-string (country-stat province-stat-list)
  (apply 'cl-concatenate 'string (format "COVID-19 全国 %s" (covid-monitor-format-covid-stat country-stat))
         (cl-mapcar (lambda (p stat) (format " %s %s" p (covid-monitor-format-covid-stat stat)))
                    covid-monitor-province-list province-stat-list)))

(defun covid-monitor-extract-stat (stat-data)
  (if (not stat-data) 0
    (let ((confirmed (gethash "totalConfirmed" stat-data))
          (doubtful (gethash "totalDoubtful" stat-data))
          (cured (gethash "totalCured" stat-data))
          (death (gethash "totalDeath" stat-data)))
      (- (+ confirmed doubtful) (+ cured death)))))

(defun covid-monitor-find-data (data-list key)
  (cl-find-if (lambda (x) (equal key (gethash "childStatistic" x)))
              data-list))

(defun covid-monitor-extract-province-data (province-data-list province)
  (let* ((province-data (covid-monitor-find-data province-data-list province))
         (city-data-list (gethash "cityArray" province-data))
         (imported-data (covid-monitor-find-data city-data-list "境外输入")))
    (make-covid-stat :total (covid-monitor-extract-stat province-data) :imported (covid-monitor-extract-stat imported-data))))

(defun covid-monitor-extract-province-data-list (raw-data)
  (let ((province-data-list (gethash "provinceArray" raw-data)))
    (cl-mapcar (lambda (x) (covid-monitor-extract-province-data province-data-list x))
               covid-monitor-province-list)))

(defun covid-monitor-extract-total-imported-stat (province-data-list)
  (apply '+ (cl-mapcar (lambda (province-data)
                         (let* ((city-data-list (gethash "cityArray" province-data))
                                (imported-data (covid-monitor-find-data city-data-list "境外输入")))
                           (covid-monitor-extract-stat imported-data)))
                       province-data-list)))

(defun covid-monitor-extract-country-data (raw-data)
  (let* ((country-stat (covid-monitor-extract-stat (gethash "country" raw-data)))
         (province-data-list (gethash "provinceArray" raw-data))
         (imported-stat (covid-monitor-extract-total-imported-stat province-data-list)))
    (make-covid-stat :total country-stat :imported imported-stat)))

(defun covid-monitor-extract-covid-data ()
  (with-current-buffer (get-buffer-create covid-monitor-data-buffer)
    (when (< 0 (length (buffer-string)))
      (let* ((json-object-type 'hash-table)
             (json-array-type 'list)
             (json-key-type 'string)
             (covid-raw-data (json-read-from-string (buffer-string)))
             (country-stat (covid-monitor-extract-country-data covid-raw-data))
             (province-stat-list (covid-monitor-extract-province-data-list covid-raw-data)))
        (setq covid-monitor-status-string (covid-monitor-format-covid-status-string country-stat province-stat-list))
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
