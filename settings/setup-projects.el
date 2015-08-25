(prodigy-define-tag
  :name 'docker
  :ready-message "Attaching to .*")

(prodigy-define-tag
  :name 'thin
  :ready-message "Listening on 0\\.0\\.0\\.0:[0-9]+, CTRL\\+C to stop")

(prodigy-define-tag
  :name 'webrick
  :ready-message "WEBrick::HTTPServer#start: pid=[0-9]+ port=[0-9]+")

(prodigy-define-tag
  :name 'mongrel
  :ready-message "Ctrl-C to shutdown server")

(prodigy-define-tag
  :name 'rails
  :tags '(thin mongrel webrick))

(prodigy-define-service
  :name "Yummikarma Rails Server"
  :command "~/.rbenv/shims/bundle"
  :args '("exec" "rails" "server")
  :cwd "~/src/rails/yummikarma"
  :tags '(rails))

(prodigy-define-service
  :name "Coinwise docker-compose"
  :command "docker-compose"
  :args '("up")
  :cwd "~/src/node/coinwise"
  :tags '(docker))

(provide 'setup-projects)
