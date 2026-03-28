;;; init-calendar.el --- Calendar configuration -*- lexical-binding: t -*-

;; Copyright © 2003 Michael Shields
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:
;; Configuration for calendar and time functions

;;; Code:

;; Calendar configuration
;; Use local (US Pacific) time, not my usual TZ, which is UTC.
(setopt calendar-time-zone -480)
(setopt calendar-standard-time-zone-name "PST")
(setopt calendar-daylight-time-zone-name "PDT")
(setopt calendar-daylight-savings-starts
        '(calendar-nth-named-day 2 0 3 year))
(setopt calendar-daylight-savings-ends
        '(calendar-nth-named-day 1 0 11 year))
(setopt calendar-daylight-time-offset 60)
(setopt calendar-daylight-savings-starts-time 120)
(setopt calendar-daylight-savings-ends-time 120)

(setopt calendar-week-start-day 1)

(setopt calendar-date-display-form '(year "-" (format "%02d-%02d"
                                                      (string-to-number month)
                                                      (string-to-number day))))
(setopt calendar-time-display-form '(24-hours ":" minutes
                                              (if time-zone" ") time-zone))

(add-hook 'initial-calendar-window-hook 'mark-calendar-holidays)

(provide 'init-calendar)
;;; init-calendar.el ends here
