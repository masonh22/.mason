org-mode

(title    & "#+title: " p n "#+author: Mason Hall" n "#+language: en")
(begin    & "#+begin_" (s name) n> r> n> "#+end_" name)
(quote    & "#+begin_quote" n> r> n> "#+end_quote")
(src      & "#+begin_src " p n r n "#+end_src"
 :post (org-edit-src-code))
(python   & "#+begin_src python" n r n "#+end_src"
 :post (org-edit-src-code))
(bash     & "#+begin_src bash" n r n "#+end_src"
 :post (org-edit-src-code))
(caption  & "#+caption: ")
(drawer   & ":" p ":" n r ":end:")
