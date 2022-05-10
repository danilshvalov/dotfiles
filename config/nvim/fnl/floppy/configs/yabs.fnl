(import-macros {: nmap!} :dna.vim)
(import-macros {: package!} :dna.plug)

(local yabs (require :yabs))

(fn setup []
  (yabs:setup {:languages {:tex {:tasks {:run {:command :make
                                               :output :echo
                                               :type :shell}}}}}))
(nmap! :<Leader>rt (fn [] (yabs:run_task :run)))

{: setup}
