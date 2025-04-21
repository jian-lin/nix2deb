;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((eglot-workspace-configuration . (:haskell
                                           (:plugin
                                            (:hlint
                                             (:config
                                              (:flags ["--hint=.hlint.relude.yaml" "--hint=.hlint.yaml"]))
                                             :rename (:config (:crossModule t)))
                                            :sessionLoading "multipleComponents"))))))
