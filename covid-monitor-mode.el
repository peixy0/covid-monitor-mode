(require 'json)

(defvar covid-monitor-data-buffer "*covid-data*")
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

(defun covid-monitor-format-covid-status-string (country-count province-count-list)
  (string-join (cons (format "COVID-19 全国 %d" country-count)
                     (cl-mapcar (lambda (p count) (format "%s %d" p count))
                                covid-monitor-province-list province-count-list))
               " "))

(defun covid-monitor-extract-province-data (province-data province)
  (gethash "totalConfirmed"
           (cl-find-if (lambda (x) (equal province (gethash "childStatistic" x)))
                       province-data)))

(defun covid-monitor-extract-province-data-list (raw-data)
  (mapcar (lambda (x) (covid-monitor-extract-province-data (gethash "provinceArray" raw-data) x))
          covid-monitor-province-list))

(defun covid-monitor-extract-country-data (raw-data)
  (gethash "totalConfirmed" (gethash "country" raw-data)))

(defun covid-monitor-extract-covid-data ()
  (with-current-buffer (get-buffer-create covid-monitor-data-buffer)
    (when (< 0 (length (buffer-string)))
      (let* ((json-object-type 'hash-table)
             (json-array-type 'list)
             (json-key-type 'string)
             (covid-raw-data (json-read-from-string (buffer-string)))
             (country-count (covid-monitor-extract-country-data covid-raw-data))
             (province-count-list (covid-monitor-extract-province-data-list covid-raw-data)))
        (setq covid-monitor-status-string (covid-monitor-format-covid-status-string country-count province-count-list))
        (force-mode-line-update 'all)))))

(defun covid-monitor-covid-data-update ()
  (with-current-buffer (get-buffer-create covid-monitor-data-buffer)
    (erase-buffer)
    (make-process :name "covid-data-fetcher"
                  :buffer covid-monitor-data-buffer
                  :command (list "curl" "--get"
                                 "https://ncovdata.market.alicloudapi.com/ncov/cityDiseaseInfoWithTrend"
                                 "-H" (format "Authorization: APPCODE %s" covid-monitor-alicloud-api-appcode))
                  :sentinel (lambda (process event)
                              (when (string-prefix-p "finished" event)
                                (covid-monitor-extract-covid-data))))))

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
