(ns pulsar.cfengine.portage
  (:gen-class)
  (:require
   [clojure.java.io :as io]
   [clojure.java.shell :refer [sh]]
   [clojure.string :as string]
   [clojure.spec.alpha :as spec]
   [cfengine-promise-protocol :as cfepp])
  (:import
   [java.io BufferedReader]))

(def ACCEPT_KEYWORDS_LOC "/etc/portage/package.accept_keywords/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Promise Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spec/def ::not-nil-string
  (spec/and
   string?
   (comp not nil?)
   (comp not string/blank?)))

(spec/def ::fully-qualified-package-name
  (spec/and ::not-nil-string
   #(string/includes? % "/")))

(spec/def ::string-true-or-false
  (spec/and ::not-nil-string
            #(re-matches #"true|false" %)))

(spec/def ::installed
  ::string-true-or-false)

(spec/def ::oneshot
  ::string-true-or-false)

(spec/def ::accept-keywords
  ::not-nil-string)

(spec/def ::promiser ::fully-qualified-package-name)

(spec/def ::promise-attributes
  (spec/keys :req-un [::installed]
             :opt-un [::oneshot ::accept-keywords]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Gather Packages on the System
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (def test-packages
;;  ["media-libs/opensubdiv-3.4.4-r3"
;;   "media-libs/libmpeg2-0.5.1-r3"        
;;   "sys-kernel/installkernel-gentoo-7"
;;   "perl-core/File-Temp-0.231.100"
;;   "perl-file/File-t"])

(defn pkg-munge [pkg-string]
  "Expects a fully qualified emerge package
   ex. media-libs/glfw-3.3.8"
  (let [pkg-split (re-matches #"(.*)/(.*)-([0-9].*)" pkg-string)]
    {::category (nth pkg-split 1)
     ::package (nth pkg-split 2)
     ::version (nth pkg-split 3)}))

(defn get-package-list [] 
  (let [prefix "/var/db/pkg/"
        ret (sh "/usr/bin/env" "find" prefix "-maxdepth" "2" "-mindepth" "2" "-type" "d")]
    (if (= 0 (:exit ret))
      (let [lines (string/split-lines (:out ret))
            remove-prefix #(string/replace % prefix "")
            parse-pkg (comp pkg-munge remove-prefix)]
        {::package-list (map parse-pkg lines) ::error nil})
      {::package-list nil ::error (:err ret)})))

(defn list-package-error? [list-package-output]
  (nil? (::package-list list-package-output)))

(defn get-list-package-error-msg [list-package-output]
  (::error list-package-output))

(defn get-installed-packages [list-package-output]
  (::package-list list-package-output))

(defmacro fsome [& args]
  `(if (nil? (some ~@args))
     false
     true))

;;(package-installed? "sys-cluster/kubelet" (get-installed-packages (get-package-list)))
;;(package-installed? "roger/roger" (get-installed-packages (get-package-list)))
;;(package-installed? "ff" (get-installed-packages (get-package-list)))
(defn package-installed? [package-name installed-packages-list]
  (if (nil? (re-matches #".*/.*" package-name))
    false
    (let [[_ category package] (re-matches #"(.*)/(.*)" package-name)]
      (fsome #(and (= (::package %) package)
                   (= (::category %) category)) installed-packages-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Get Current Package State to Desired State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro exit0? [sh-output]
  `(= 0 (:exit ~sh-output)))

(defmacro str-to-bool [var]
  `(if (= ~var "true") true false))
  
;;(portage-install-package "sys-cluster/kubelet")
(defn portage-install-package [fq-package oneshot?]
  (sh "/usr/bin/env" "emerge"
      (if oneshot? "--oneshot" "")
      "--quiet-build" fq-package))

(defn portage-uninstall-package [fq-package]
  (sh "/usr/bin/env" "emerge" "--unmerge" fq-package))

(defmulti handle-package ::desired-state)

(defmethod handle-package ::installed [{installed-packages ::installed-packages
                                        oneshot? ::oneshot
                                        package-name ::fq-package}]
  (if (package-installed? package-name installed-packages)
    (cfepp/promise-kept (str package-name " is already installed"))
    (let [install-output (portage-install-package package-name oneshot?)]
      (if (exit0? install-output)
        (cfepp/promise-repaired (:out install-output))
        (cfepp/promise-not-kept (:err install-output))))))
    
(defmethod handle-package ::not-installed [{installed-packages ::installed-packages
                                            package-name ::fq-package}]
  (if (not (package-installed? package-name installed-packages))
    (cfepp/promise-kept (str package-name " is not installed"))
    (let [uninstall-output (portage-uninstall-package package-name)]
      (if (exit0? uninstall-output)
        (cfepp/promise-repaired (:out uninstall-output))
        (cfepp/promise-not-kept (:err uninstall-output))))))

(defn get-first-line [filename]
  (if (.exists (io/file filename))
    (with-open [rdr (io/reader filename)]
      (first (line-seq rdr)))))

;;(accept-keywords-filename "sys-cluster/kubelet")
(defn accept-keywords-filename [package-name]
  (str ACCEPT_KEYWORDS_LOC "CFPORTAGE-" (string/replace package-name "/" "-")))

;;(handle-accept-keywords {::accept-keywords "~arm64" ::fq-package "sys/cluster/kubelet"})
;;(handle-accept-keywords {::accept-keywords nil ::fq-package "sys/cluster/kubelet"})
(defmulti handle-accept-keywords ::accept-keywords)

(defmethod handle-accept-keywords :default [{accept-keywords ::accept-keywords
                                             package-name ::fq-package}]
  (let [filename (accept-keywords-filename package-name)
        desired-val (str package-name " " accept-keywords)
        curr-val (get-first-line filename)]
    (if (not (= curr-val desired-val))
      (spit filename desired-val))))

(defmethod handle-accept-keywords nil [{package-name ::fq-package}]
  "No accept_keywords provided, must delete if they exist"
  (io/delete-file (accept-keywords-filename package-name) true))


;;(evalute-promise "sys-cluster/kubelet" {:installed "true" :oneshot "true"})
;;(evalute-promise "sys-cluster/kubelet" {:installed "false"})
(defn evalute-promise [promiser {installed-input :installed
                                 oneshot :oneshot
                                 accept-keywords :accept_keywords
                                 :as attributes}]
  (let [package-name promiser
        list-package-output (get-package-list)]
    (if (list-package-error? list-package-output)
      (cfepp/promise-not-kept (get-list-package-error-msg list-package-output))
      (do
        (handle-accept-keywords {::accept-keywords accept-keywords
                                 ::fq-package package-name})
        (handle-package {::oneshot (str-to-bool oneshot)
                         ::desired-state (if (= "true" installed-input) ::installed ::not-installed)
                         ::installed-packages (get-installed-packages list-package-output)
                         ::fq-package package-name})))))

(defn -main [& args]
  (cfepp/start-promise-module (BufferedReader. *in*)
                              "portage_package_module_clj"
                              "0.0.1"
                              ::promiser
                              ::promise-attributes
                              evalute-promise)
  (System/exit 0))
