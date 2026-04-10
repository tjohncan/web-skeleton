(defsystem "web-skeleton-tls"
  :description "TLS support for web-skeleton (outbound HTTPS via libssl)"
  :version "0.0.7"
  :author "Tiger Johnson"
  :license "MIT"
  :depends-on ("web-skeleton")
  :components ((:file "src/tls")))
