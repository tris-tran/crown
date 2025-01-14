(crown|def-recipe 
  :name 'evil
  :version "1.14"
  :source (crown|recipe-get-source 
           :method 'git
           :uri "https://github.com/emacs-evil/evil.git"
           :commit "commit")
  :build (crown|default-builder
	  :files '(:defaults
		   :exclude ("evil-test-helpers.el")))

  :install (crown|default-installer)
  :dependencies nil
  :doc "asdasdasd")
