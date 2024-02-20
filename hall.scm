(hall-description
  (name "doorbell")
  (prefix "")
  (version "0.1")
  (author "Justin Bishop")
  (email "")
  (copyright (2024))
  (synopsis "")
  (description "")
  (home-page "")
  (license gpl3+)
  (dependencies `())
  (skip ())
  (features
    ((guix #f)
     (use-guix-specs-for-dependencies #f)
     (native-language-support #f)
     (licensing #f)))
  (files (libraries
           ((scheme-file "doorbell")
            (directory "doorbell" ((scheme-file "hconfig")))))
         (tests ((directory "tests" ())))
         (programs ((directory "scripts" ())))
         (documentation
           ((org-file "README")
            (symlink "README" "README.org")
            (text-file "HACKING")
            (text-file "COPYING")
            (directory "doc" ((texi-file "doorbell")))))
         (infrastructure
           ((scheme-file "guix")
            (text-file ".gitignore")
            (scheme-file "hall")))))