(crown|def-recipe 
  :name 'evil
  :version "1.14"
  :source (crown|recipe-get-source 
           :method 'git
           :uri "https://github.com/emacs-evil/evil.git"
           :commit "commit")
  :build (crown|default-builder
           :files '(""))
  :install "asdsda"
  :dependencies '('evil-dep)
  :doc "asdasdasd")
