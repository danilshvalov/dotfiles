(import-macros {: nmap!} :dna.vim)
(local reach (require :reach))


(nmap! :<C-X> #(reach.buffers {:handle :dynamic}))
(nmap! :<Leader>q :<Cmd>Bdelete<CR>)


(reach.setup {
  :notifications false
})
